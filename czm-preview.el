;;; czm-preview.el --- Extensions for preview.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-preview.el
;; Package-Requires: ((emacs "29.1") (auctex "11.86.1") (czm-tex-util "0.0"))
;; Keywords: tex, wp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds the following features to preview.el:
;;
;; -`czm-preview-mode' activates a timer that aggressively previews
;; any unpreviewed environments in the visible portion of the buffer.
;;
;; - Previews work in indirect buffers and in buffers not associated
;; with files (e.g., indirect org mode source blocks).
;;
;; - Equation numbers extracted from the .aux file, when possible.
;;
;; -`czm-preview-current-environment' shows a preview of the active
;; equation environment just above where the user is editing.  This is
;; useful for visualizing what you're working on.
;;
;;
;; This package OVERRIDES the following functions from preview.el:
;; 
;; - preview-region
;; 
;; - preview-parse-messages
;; 
;; - preview-kill-buffer-cleanup
;; 
;; - TeX-region-create
;; 
;; It also adds "around" advice to preview-place-preview.

;;; Code:

(require 'cl-lib)
(require 'latex)
(require 'preview)
(require 'czm-tex-util)
(require 'czm-preview-override)

(defgroup czm-preview nil
  "Settings for czm-preview."
  :group 'czm
  :prefix "czm-preview-")

(defcustom czm-preview-valid-environments
  '("equation" "equation*" "align" "align*" "tabular" "tabular*" "multline" "multline*")
  "Valid LaTeX environments for active preview."
  :type '(repeat string)
  :group 'czm-preview)

(defcustom czm-preview-TeX-master nil
  "TeX-master value to be used for AUCTeX preview."
  :type 'string
  :group 'czm-preview)

(defcustom czm-preview-max-region-radius 5000
  "Maximum radius of region to be previewed."
  :type 'number
  :group 'czm-preview)

(defcustom czm-preview-latex-prefix-directory ""
  "Prefix directory for LaTeX binaries."
  :type 'string
  :group 'czm-preview)

(defcustom czm-preview-timer-interval 0.1
  "Interval for preview timer.
For this to have any effect, it must be set before
czm-preview-mode is activated for the first time."
  :type 'number
  :group 'czm-preview)

;; because texlive 2023 seems super slow
(when (equal (system-name) "Pauls-MBP-3")
  (setq czm-preview-latex-prefix-directory "/usr/local/texlive/2020/bin/x86_64-darwin/"))
;; (setq czm-preview-latex-prefix-directory "/usr/local/texlive/2023/bin/universal-darwin/")
;; /usr/local/texlive/2023/bin/universal-darwin/

(with-eval-after-load 'preview
  (setq preview-LaTeX-command
	`(
	 ,(concat
	  "%`"
	  czm-preview-latex-prefix-directory
	  "%l \"\\nonstopmode\\nofiles\\PassOptionsToPackage{")
	 ("," . preview-required-option-list)
	 "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined" preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\"")))



(defvar czm-preview--active-environment-start nil)

(defvar czm-preview--debug nil)

(defvar czm-preview--timer nil)

(defvar-local czm-preview--style-hooks-applied nil
  "Has `TeX-update-style' been run in this buffer?

")

(defvar-local czm-preview--timer-enabled nil
  "Is the preview timer is enabled in this buffer?
We want the preview timer to be active only in the current
buffer.  For this reason, it is a global object.  This local
variable keeps track of the buffers in which the timer should do
anything.")

(defvar-local czm-preview--active-region nil
  "Stores the region currently being processed by `preview-region'.")

(defvar-local preview-region--last-time nil)

(defvar-local preview-region--begin nil)

(defvar-local preview-region--end nil)





(defun czm-preview--after-change-function (beg end _length)
  "Function called after a change is made in `latex-mode' buffer.
The function checks if the modification affects the active
preview region and kills the current preview job if necessary.

BEG is the start of the modified region, END is the end of the
 region, and LENGTH is the length of the modification."
  (when (eq major-mode 'latex-mode)
    (when-let ((active-region-end (cdr czm-preview--active-region)))
      (with-current-buffer (get-buffer-create "*DebugPreview*")
	(goto-char (point-max))
	(insert (format-time-string "%Y-%m-%d %T.%3N\n"))
	(insert (format "region being modified: (%s %s)\n" beg end))
	(insert (format "active preview region: (%s %s)\n"
			(car czm-preview--active-region)
			(cdr czm-preview--active-region))))
      ;; Cancel any previews positioned after an active edit.
      (when (< beg active-region-end)
	(ignore-errors (TeX-kill-job))))))

(defun czm-preview--post-command-function ()
  "Function called after each command in `latex-mode' buffer."
  (and (eq major-mode 'latex-mode)
       czm-preview--active-region
       (not (eq czm-preview--active-environment-start
		(car czm-preview--active-region)))
       (texmathp)
       (<= (car czm-preview--active-region) (point))
       (< (point) (cdr czm-preview--active-region))
       (ignore-errors (TeX-kill-job))))



;;;#autoload
(defun czm-preview-current-environment ()
  "Generate preview just before current environment.
If the environment already has an (inactive) overlay generated by
preview, then move that overlay to just before the environment,
where it can be seen."
  (interactive ())
  (when-let ((env-start (czm-preview--valid-env-start)))
    (if-let ((ov
	      (cl-find-if
	       (lambda (ov) (overlay-get ov 'preview-state))
	       (overlays-at (point)))))
	(move-overlay ov env-start env-start)
      ;; (preview-clearout-at-point)
      (if (czm-preview-processes)
	  (error "Existing TeX process")
	(setq czm-preview--active-environment-start env-start)
	(save-mark-and-excursion
	  (LaTeX-mark-environment)
	  (let ((inhibit-message t))
	    (preview-region (point) (mark))))
	;; (preview-environment 0)
	))))

(defun czm-preview--valid-environment-start ()
  "Return start of current equation or align environment.
Return nil if not currently in such an environment."
  (and
   (texmathp)
   (member (car texmathp-why) czm-preview-valid-environments)
   (cdr texmathp-why)))








;;  more generally, we should kill the job if the point moves into a
;;  math environment in the region.

(defun czm-preview--place-preview-advice (orig-func &rest args)
  "Modify overlay positioning of `preview-place-preview'.
If `czm-preview--active-environment-start' is non-nil and equal to the
starting position of the current environment, then the start and
end positions for the overlay are both adjusted to be at the
start of the environment.  The result is that the overlay does
not obscure the environment itself.

ORIG-FUNC is the original
function being advised, and ARGS are its argument."
  (setq czm-preview--active-region nil)
  (cl-destructuring-bind (snippet start end box counters tempdir place-opts) args
    (when czm-preview--active-environment-start
      (save-excursion
	(beginning-of-line)
	(when-let ((env-start (czm-preview--valid-environment-start)))
	  (when (eq env-start czm-preview--active-environment-start)
	    (preview-clearout start end tempdir)
	    (setq czm-preview--active-environment-start nil)
	    (setq start env-start)
	    (setq end env-start)))))
    (apply orig-func (list snippet start end box counters tempdir place-opts))))


(defun czm-preview-processes ()
  "Return list of preview processes for current LaTeX document."
  (or
   (TeX-process
    (concat default-directory "_region_"))
   ;; (get-buffer-process (TeX-process-buffer-name (concat (TeX-master-directory) (TeX-active-master))))
   (get-buffer-process (TeX-process-buffer-name (concat (TeX-active-master))))))



(defun czm-preview-find-first-stale-math-region (beg end)
  "Return convex hull of first state math intervals.
Search between BEG and END.  Return a cons cell of beginning and
ending positions."
  (when czm-preview--debug
    (message "czm-preview-find-first-stale-math-region: %s %s" beg end))
  (when (< beg end (+ beg (* 2 czm-preview-max-region-radius)))
    (let* ((top-level-math-intervals
	    (czm-preview--find-top-level-math-intervals beg end))
	   (regions
	    (mapcar (lambda (interval)
		      (buffer-substring-no-properties
		       (car interval) (cdr interval)))
		    top-level-math-intervals))
	   (staleness
	    (cl-mapcar (lambda (interval region)
		         (and
			  (not (czm-preview--active-or-inactive-preview-at-point-p (car interval)))
			  (not (string-match-p (regexp-opt '("<++>" "<+++>"))
					       region))))
		       top-level-math-intervals regions))
	   (line-numbers         ; only for stuff occuring on one line
	    (mapcar (lambda (interval)
		      (let ((line-beg (line-number-at-pos (car interval) ))
			    (line-end (line-number-at-pos (cdr interval) )))
		        (when (equal line-beg line-end)
			  line-beg)))
		    top-level-math-intervals)))

      ;;  run preview-region on the first continguous stale chunk
      (when-let ((first-visible-chunk
		  (car (czm-preview--non-nil-intervals staleness))))
        (let* ((first-interval-index (car first-visible-chunk))
	       (last-interval-index (cdr first-visible-chunk))
	       (first-interval (nth first-interval-index top-level-math-intervals))
	       (last-interval (nth last-interval-index top-level-math-intervals))
	       (begin-pos
	        (car first-interval))
	       (end-pos (cdr last-interval)))
          (when czm-preview--debug
            (message "begin-pos: %s, end-pos: %s" begin-pos end-pos))
	  (cons begin-pos end-pos))))))




(defun czm-preview--first-visible-stale-region ()
  "Preview an appropriate chunk at top of window.

  Identify the top level math intervals in the visible portion of
  the buffer.  Find the first contiguous group of intervals at
  which there are no active or inactive previews at point.  Call
  `preview-region' on the smallest interval that contains this
  group."
  (interactive)
  (when czm-preview--debug
    (message "czm-preview--first-visible-stale-region"))
  (unless (czm-preview-processes)
    (let*
	((margin-search-paragraphs 3)
	 (above-window-start
	  (save-excursion
            (let ((threshold (max (window-start)
                                  (- (point) czm-preview-max-region-radius))))
	      (goto-char threshold)
              (backward-paragraph margin-search-paragraphs)
              (point))))
	 (below-window-end
	  (save-excursion
            (let ((threshold (min (window-end)
                                  (+ (point) czm-preview-max-region-radius))))
	      (goto-char threshold)
              (forward-paragraph margin-search-paragraphs)
              (point))))
	 (action
	  (lambda (interval)
	    (let ((inhibit-message t)
                  (beg (car interval))
                  (end (cdr interval)))
              (when czm-preview--debug
                (message (format "previewing region (%s %s)" beg end)))
              (preview-region beg end)))))
      ;; TODO.  Okay, sometimes this functions calls
      ;; czm-preview-find-first-stale-math-region with reversed
      ;; arguments.  You're not really sure what's up with that.  For
      ;; now you'll just "plug the hole" by having
      ;; czm-preview-find-first-stale-math-region do nothing in such cases.
      (let (interval)
        (cond
         ((setq interval (czm-preview-find-first-stale-math-region
                          above-window-start
			  (point)))
          (when czm-preview--debug
            (message "above-window-start: %s, point: %s" above-window-start (point)))
	  (funcall action interval))
         ((setq interval (czm-preview-find-first-stale-math-region
                          (point)
                          below-window-end))
          (when czm-preview--debug
            (message "point: %s, below-window-end: %s" (point) below-window-end))
	  (funcall action interval)))))))

(defun czm-preview--find-top-level-math-intervals (start end)
  "Find top-level LaTeX math environments between START and END.
Returns a list of cons cells representing the intervals, with
start and end positions in buffer."
  (save-excursion
    (let ((math-intervals '()))
      (goto-char start)
      (while (re-search-forward "\\(\\\\begin{\\([^{}]*\\)}\\|\\$\\|\\\\\\[\\)" end t)
	(let ((env-text (match-string 0))
	      (env-name (match-string 2))
	      (env-after-begin (match-end 1))
	      (interval-start (match-beginning 0)))
	  (when
	      (and
	       (or
		;; found mathy environment
		(member env-name texmathp-environments)
		;; found $...$ or \[...\]
		(and (null env-name) (texmathp)))
	       ;; not in comments
	       (save-excursion
		 (let ((p (point)))
		   (beginning-of-line)
		   (not (search-forward "%" p t)))))
	    (let*
		(contents interval-end)
	      ;; environment
	      (cond
	       (env-name
		(progn (goto-char env-after-begin)
		       (setq interval-end
			     (re-search-forward
			      (concat "\\\\end{" (regexp-quote env-name) "}")
			      end t))
		       (let* ((contents-start
			       (save-excursion
				 (goto-char interval-start)
				 (end-of-line)
				 (point)))
			      (contents-end
			       (save-excursion
				 (beginning-of-line)
				 (point))))
			 (setq contents
			       (buffer-substring-no-properties
				contents-start
				contents-end)))))
	       ;; $...$
	       ((string= env-text "$")
		(let ((line-end
		       (min end (save-excursion
				  (end-of-line)
				  (point)))))
		  (setq interval-end
			(re-search-forward "\\$" line-end t))
		  (setq contents
			(buffer-substring-no-properties (1+ interval-start) (1- (point))))))
	       ;; \[...\]
	       ((string= env-text "\\[")
		(setq interval-end
		      (re-search-forward "\\\\\\]" end t))
		(when interval-end
		  (setq contents
			(buffer-substring-no-properties
			 (+ 2 interval-start)
			 (- (point) 2)))))
	       )
	      (and interval-start interval-end
		   (string-match-p "[^[:space:]\n\r]" contents) ; check that contents are non-empty
		   (push (cons interval-start interval-end) math-intervals))))))
      (nreverse math-intervals))))

(defun czm-preview--active-or-inactive-preview-at-point-p (&optional pos)
  "Is there a non-disabled preview overlay at POS?
POS defaults to (point)."
  (cl-intersection (mapcar
		    (lambda (ov) (overlay-get ov 'preview-state))
		    (overlays-at (or pos (point))))
		   '(active inactive)))

(defun czm-preview--non-nil-intervals (input-list)
  "Return a list of intervals of non-nil values in INPUT-LIST."
  (let ((intervals nil)
        (start-index nil)
        (end-index nil))
    (dotimes (i (length input-list))
      (if (nth i input-list)
          (progn
            (when (not start-index)
              (setq start-index i))
            (setq end-index i))
        (when (and start-index end-index)
          (push (cons start-index end-index) intervals)
          (setq start-index nil)
          (setq end-index nil))))
    (when (and start-index end-index)
      (push (cons start-index end-index) intervals))
    (nreverse intervals)))

(defun czm-preview--flag-style-hooks-applied ()
  (setq-local czm-preview--style-hooks-applied t))

(defun czm-preview-reset-timer ()
  "Reset the preview timer."
  (interactive)
  (when czm-preview--timer
    (cancel-timer czm-preview--timer)
    (setq czm-preview--timer nil))
  (setq czm-preview--timer
        (run-with-timer czm-preview-timer-interval czm-preview-timer-interval #'czm-preview--timer-function)))

(defun czm-preview--timer-function ()
  "Function called by the preview timer to update LaTeX previews."
  (interactive)
  (and
   czm-preview--timer
   czm-preview--timer-enabled
   czm-preview--style-hooks-applied
   font-lock-set-defaults ;; this is key
   (or (not preview-region--last-time)
       (> (float-time) (+ preview-region--last-time 0.25)))
   ;; (not czm-preview--active-region)
   (eq major-mode 'latex-mode)
   (cond
    ((czm-preview--first-visible-stale-region))
    (nil (texmathp)
	 (unless (czm-preview-processes)
	   (czm-preview-current-environment))))))



(defun czm-preview--init ()
  "Initialize advice and hooks for `czm-preview'."
  (add-hook 'after-change-functions #'czm-preview--after-change-function nil t)
  (add-hook 'post-command-hook #'czm-preview--post-command-function nil t)
  (add-hook 'TeX-update-style-hook #'czm-preview--flag-style-hooks-applied nil t)
  (advice-add 'preview-region :override #'czm-preview-override-region)
  (advice-add 'preview-place-preview :around #'czm-preview--place-preview-advice)
  (advice-add 'preview-parse-messages :override #'czm-preview-override-parse-messages)
  (advice-add 'preview-kill-buffer-cleanup :override #'czm-preview-override-kill-buffer-cleanup)
  (advice-add 'TeX-region-create :override #'czm-preview-override-TeX-region-create)
  (advice-add 'TeX-process-check :override #'czm-preview-override-TeX-process-check))

(defun czm-preview--close ()
  "Remove advice and hooks for `czm-preview'."
  (remove-hook 'after-change-functions #'czm-preview--after-change-function t)
  (remove-hook 'post-command-hook #'czm-preview--post-command-function t)
  (remove-hook 'TeX-update-style-hook #'czm-preview--flag-style-hooks-applied t)
  (advice-remove 'preview-region #'czm-preview-override-region)
  (advice-remove 'preview-place-preview #'czm-preview--place-preview-advice)
  (advice-remove 'preview-parse-messages #'czm-preview-override-parse-messages)
  (advice-remove 'preview-kill-buffer-cleanup #'czm-preview-override-kill-buffer-cleanup)
  (advice-remove 'TeX-region-create #'czm-preview-override-TeX-pregion-create)
  (advice-remove 'TeX-process-check #'czm-preview-override-TeX-process-check))

(define-minor-mode czm-preview-mode
  "Minor mode for running LaTeX preview on a timer."
  :lighter " PrT"
  :global nil
  :group 'czm-preview
  (if czm-preview-mode
    (progn
      (czm-preview--init)
      ; maybe this should be a separate setting?
      (when czm-preview-TeX-master
        (setq-local TeX-master czm-preview-TeX-master))
      (setq-local TeX-PDF-mode nil)
      ;; Start the timer if it's not already running
      (czm-preview-reset-timer)
      ;; Enable the timer.
      (setq-local czm-preview--timer-enabled t)
      (message "czm-preview-mode enabled."))
    ;; Disable the timer.
    (setq-local czm-preview--timer-enabled nil)
    ;; Hacky:
    (setq-local czm-preview--active-region nil)

    ;; I think the hooks add a benefit anyway, right?  Maybe just want a separate function
    ;; (czm-preview--close)

    (message "czm-preview-mode disabled.")))



;;;###autoload
(defun czm-preview-toggle-master ()
  "Toggle `TeX-master' between `czm-preview-TeX-master' and t.
Display message in the minibuffer indicating old and new value."
  (interactive)
  (message "TeX-master: %s -> %s"
	   TeX-master
	   (if (equal TeX-master czm-preview-TeX-master)
	       (progn
		 (TeX-PDF-mode 1)
		 (setq TeX-master t))
	     (TeX-PDF-mode 0)
	     (setq TeX-master czm-preview-TeX-master))))

;; (defun my-preview-TeX-master-advice (orig-fun file command)
;;   "Temporarily set the value of `TeX-master' to `preview-TeX-master' for AUCTeX preview."
;;   (let ((TeX-master preview-TeX-master))
;;     (funcall orig-fun file command)))

;; (advice-add 'preview-generate-preview :around #'my-preview-TeX-master-advice)
;; (advice-remove 'preview-generate-preview #'my-preview-TeX-master-advice)

(defun czm-preview--preprocess (str)
  "Preprocess STR for preview."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert str)
      ;; (latex-mode)
      (goto-char (point-min))
      (while (re-search-forward "\\\\label{\\([^}]+\\)}" nil t)
	(let ((label (match-string 1)))
	  (when-let ((number
		      (with-current-buffer buf
			(czm-tex-util-get-label-number label))))
	    (when
                ; hack - texmathp expects to be run in LaTeX-mode
                (let ((comment-start-skip
                       "\\(\\(^\\|[^\\
]\\)\\(\\\\\\\\\\)*\\)\\(%+[ 	]*\\)"))
                  (texmathp))
	      (insert (format "\\tag{%s}" number))))))
      (buffer-substring-no-properties (point-min) (point-max)))))




(provide 'czm-preview)
;;; czm-preview.el ends here
