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

;;; ------------------------------ REQUIRE ------------------------------

(require 'cl-lib)
(require 'latex)
(require 'preview)
(require 'czm-tex-util)

;;; ------------------------------ CUSTOMIZATIONS ------------------------------

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
  "TeX-master value to be used for AUCTeX preview.
TODO: document this better."
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

(defcustom czm-preview-timer-interval 0.25
  "Interval for preview timer.
For this to have any effect, it must be set before
czm-preview-mode is activated for the first time."
  :type 'number
  :group 'czm-preview)

;;; ------------------------------ INTERNAL VARIABLES ------------------------------

(defvar czm-preview--debug nil
  "If non-nil, print debug messages.")



;;; ------------------------------ OVERRIDES ------------------------------


(defun czm-preview-override-TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command.

OVERRIDE DIFFERENCES: We never kill the current process.  We do
not ask the user about this."
  (let (process)
    (while (and (setq process (TeX-process name))
                (eq (process-status process) 'run))
      (cond
       ((eq (process-status process) 'run)
           (error "Cannot have two processes for the same document"))))))


(defun czm-preview-override-TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION.

The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file.

OVERRIDE DIFFERENCES: Three lines are commented out (see the
source).  TODO: explain why"
  (if (fboundp 'preview--skip-preamble-region)
      (let ((temp (preview--skip-preamble-region region offset)))
        (if temp
            ;; Skip preamble for the sake of predumped formats.
            (setq region (car temp)
                  offset (cdr temp)))))

  (let* (;; We shift buffer a lot, so we must keep track of the buffer
         ;; local variables.
         (header-end TeX-header-end)
         (trailer-start TeX-trailer-start)

         ;; We seach for header and trailer in the master file.
         (orig-buffer (current-buffer))
         (master-name (TeX-master-file TeX-default-extension))
         (master-buffer (find-file-noselect master-name))

         ;; Attempt to disable font lock.
         (font-lock-mode-hook nil)
         ;; And insert them into the FILE buffer.
         (file-buffer (let (;; Don't query for master file
                            (TeX-transient-master t)
                            ;; Don't choose a special mode (and call its hooks)
                            (auto-mode-alist nil)
                            (magic-mode-alist nil)
                            (enable-local-variables nil)
                            ;; Don't run any f-f hooks
                            (find-file-hook nil))
                        (find-file-noselect file t)))
         ;; But remember original content.
         original-content

         ;; We search for the header from the master file, if it is
         ;; not present in the region.
         (header (if (string-match header-end region)
                     ""
                   (save-excursion
                     (save-restriction
                       (set-buffer master-buffer)
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (point-min))
                           ;; NOTE: We use the local value of
                           ;; TeX-header-end from the master file.
                           (if (not (re-search-forward TeX-header-end nil t))
                               ""
                             (re-search-forward "[\r\n]" nil t)
                             (buffer-substring-no-properties
                              (point-min) (point)))))))))
         (header-offset 0)
         first-line
         ;; We search for the trailer from the master file, if it is
         ;; not present in the region.
         (trailer-offset 0)
         (trailer (if (string-match trailer-start region)
                      ""
                    (save-excursion
                      (save-restriction
                        (set-buffer master-buffer)
                        (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-max))
                            ;; NOTE: We use the local value of
                            ;; TeX-trailer-start from the master file.
                            (if (not (re-search-backward TeX-trailer-start nil t))
                                ""
                              ;;(beginning-of-line 1)
                              (re-search-backward "[\r\n]" nil t)
                              (setq trailer-offset (TeX-current-offset))
                              (buffer-substring-no-properties
                               (point) (point-max))))))))))
    ;; file name should be relative to master
    
    ;; ---------------------------------------------------------------------
    ;; The only change is that we comment out the following three lines.
    ;; ---------------------------------------------------------------------
    ;; (setq original (TeX-quote-filename (file-relative-name
    ;;                                     original (TeX-master-directory)))
    ;;       master-name (TeX-quote-filename master-name))

    ;; If the first line begins with "%&", put that line separately on
    ;; the very first line of the region file so that the first line
    ;; parsing will work.
    (setq first-line (if (and (> (length header) 1)
                              (string= (substring header 0 2) "%&"))
                         ;; This would work even if header has no newline.
                         (substring header 0 (string-match "\n" header))
                       ""))
    (unless (string= first-line "")
      ;; Remove first-line from header.
      (setq header (substring header (length first-line)))
      (setq first-line (concat first-line "\n")))

    (with-current-buffer file-buffer
      (setq buffer-read-only t
            buffer-undo-list t)
      (setq original-content (buffer-string))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq buffer-file-coding-system
              (with-current-buffer master-buffer buffer-file-coding-system))
        (insert first-line
                "\\message{ !name(" master-name ")}"
                header
                TeX-region-extra
                "\n\\message{ !name(" original ") !offset(")
        (setq header-offset (- offset
                               (1+ (TeX-current-offset))))
        (insert (int-to-string header-offset)
                ") }\n"
                region
                "\n\\message{ !name("  master-name ") !offset(")
        (insert (int-to-string (- trailer-offset
                                  (1+ (TeX-current-offset))))
                ") }\n"
                trailer)
        (setq TeX-region-orig-buffer orig-buffer)
        (setq TeX-region-master-buffer master-buffer)
        (run-hooks 'TeX-region-hook)
        (if (string-equal (buffer-string) original-content)
            (set-buffer-modified-p nil)
          (save-buffer 0))))))

(defun czm-preview--preprocess (str)
  "Preprocess STR for preview.
Uses `czm-tex-util-get-label-number' to extract label numbers
from the appropriate .aux file, either for the current document
or any external documents that it references.  These are inserted
into STR as tags."
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

(defvar-local czm-preview--region-time nil)

(defvar-local czm-preview--region-begin nil)

(defvar-local czm-preview--region-end nil)

(defvar-local czm-preview--active-region nil
  "Stores the region currently being processed by `preview-region'.")

(defun czm-preview-override-region (begin end)
  "Run preview on region between BEGIN and END.

OVERRIDE DIFFERENCES:

(1) We add a preprocessing step to add label numbers as tags.

(2) When we call `TeX-region-create', the third argument is the
name of the buffer (rather than either <none> or the name of the
file).  This is used for TODO

(3) We store information about the region being processed (BEGIN,
END and the current time) in buffer-local variables.  TODO: why?"
  (interactive "r")
  (when czm-preview--debug
    (message "Region: %s %s" begin end))
  (let ((TeX-region-extra
	 ;; Write out counter information to region.
	 (concat (preview--counter-information begin)
		 TeX-region-extra)))
    (TeX-region-create (TeX-region-file TeX-default-extension)
		       (czm-preview--preprocess
			(buffer-substring-no-properties begin end))
		       (buffer-name)
		       ;; (or
		       ;; 	(and buffer-file-name
		       ;; 	     (file-name-nondirectory buffer-file-name))
		       ;; 	(and (buffer-base-buffer)
		       ;; 	     (buffer-file-name (buffer-base-buffer))
		       ;; 	     (file-name-nondirectory (buffer-file-name (buffer-base-buffer))))
		       ;; 	"<none>")
		       ;; (if buffer-file-name
		       ;; 	   (file-name-nondirectory buffer-file-name)
		       ;; 	 "<none>")
		       (TeX-current-offset begin)))
  (setq TeX-current-process-region-p t)
  (setq-local czm-preview--region-begin begin)
  (setq-local czm-preview--region-end end)
  (setq-local czm-preview--region-time (float-time))
  ;; (setq-local czm-preview--preview-region-already-run t)
  (setq czm-preview--active-region (cons begin end))
  (with-current-buffer (get-buffer-create "*Debug Preview*")
    (goto-char (point-max))
    (insert (format-time-string "%Y-%m-%d %T.%3N\n"))
    (insert (format "%s %s\n\n" begin end)))

  (preview-generate-preview (TeX-region-file)
			    (preview-do-replacements
			     (TeX-command-expand
			      (preview-string-expand preview-LaTeX-command))
			     preview-LaTeX-command-replacements)))



(defun czm-preview-override-parse-messages (open-closure)
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview document
style for LaTeX.  OPEN-CLOSURE is called once it is certain that
we have a valid output file, and it has to return in its CAR the
PROCESS parameter for the CLOSE call, and in its CDR the final
stuff for the placement hook.

OVERRIDE DIFFERENCES: see the comments labelled OVERRIDE
DIFFERENCE in the body of the function for details.  In short:

(1) We add support for indirect buffers and org-mode source
blocks.  This requires our modifications to `preview-region'.

(2) We fix a subtle bug that shows up when `preview-region' is
called in slightly unusual circumstances.  For details, see
https://www.mail-archive.com/bug-auctex@gnu.org/msg04327.html."
  (with-temp-message nil
    (let (TeX-error-file TeX-error-offset snippet box counters
			 file line
			 (lsnippet 0) lstart (lfile "") lline lbuffer lpoint
			 lcounters
			 string after-string
			 offset
			 parsestate (case-fold-search nil)
			 (run-buffer (current-buffer))
			 (_run-directory default-directory)
			 tempdir
			 close-data
			 open-data
			 fast-hook
			 slow-hook
			 TeX-translate-location-file
			 TeX-translate-location-line
			 TeX-translate-location-error
			 TeX-translate-location-offset
			 TeX-translate-location-context
			 TeX-translate-location-string)
      ;; clear parsing variables
      (dolist (var preview-parse-variables)
        (set (nth 1 var) nil))
      (goto-char (point-min))
      (unwind-protect
          (progn
            (while
                (re-search-forward "\
^\\(!\\|\\(.*?\\):[0-9]+:\\) \\|\
\(\\(/*\
\\(?:\\.+[^()\r\n{} /]*\\|[^()\r\n{} ./]+\
\\(?: [^()\r\n{} ./]+\\)*\\(?:\\.[-0-9a-zA-Z_.]*\\)?\\)\
\\(?:/+\\(?:\\.+[^()\r\n{} /]*\\|[^()\r\n{} ./]+\
\\(?: [^()\r\n{} ./]+\\)*\\(?:\\.[-0-9a-zA-Z_.]*\\)?\\)?\\)*\\)\
)*\\(?: \\|\r?$\\)\\|\
\\()+\\)\\|\
 !\\(?:offset(\\([---0-9]+\\))\\|\
name(\\([^)]+\\))\\)\\|\
^Preview: \\([a-zA-Z]+\\) \\([^\n\r]*\\)\r?$" nil t)
              ;;   Ok, here is a line by line breakdown:
              ;;   match-alternative 1:
              ;;   error indicator for TeX error, either style.
              ;;   match-alternative 2:
              ;;   The same, but file-line-error-style, matching on file name.
              ;;   match-alternative 3:
              ;;   Too ugly to describe in detail.  In short, we try to catch file
              ;;   names built from path components that don't contain spaces or
              ;;   other special characters once the file extension has started.
              ;;
              ;;   Position for searching immediately after the file name so as to
              ;;   not miss closing parens or something.
              ;;   (match-string 3) is the file name.
              ;;   match-alternative 4:
              ;;   )+\( \|$\)
              ;;   a closing paren followed by the end of line or a space: a just
              ;;   closed file.
              ;;   match-alternative 5 (wrapped into one shy group with
              ;;   match-alternative 6, so that the match on first char is slightly
              ;;   faster):
              ;;   !offset(\([---0-9]+\))
              ;;   an AUCTeX offset message. (match-string 5) is the offset itself
              ;;   !name(\([^)]+\))
              ;;   an AUCTeX file name message.  (match-string 6) is the file name
              ;;   TODO: Actually, the latter two should probably again match only
              ;;   after a space or newline, since that it what \message produces.
              ;;  disabled in prauctex.def:
              ;;  \(?:Ov\|Und\)erfull \\.*[0-9]*--[0-9]*
              ;;  \(?:.\{79\}
              ;;  \)*.*$\)\|
              ;;   This would have caught overfull box messages that consist of
              ;;   several lines of context all with 79 characters in length except
              ;;   of the last one.  prauctex.def kills all such messages.
              (setq file (match-string-no-properties 2))
              (cond
               ((match-beginning 1)
                (if (looking-at "\
\\(?:Preview\\|Package Preview Error\\): Snippet \\([---0-9]+\\) \\(started\\|ended\\(\
\\.? *(\\([---0-9]+\\)\\+\\([---0-9]+\\)x\\([---0-9]+\\))\\)?\\)\\.")
                    (progn
                      (when file
                        (unless TeX-error-file
                          (push nil TeX-error-file)
                          (push nil TeX-error-offset))
                        (unless (car TeX-error-offset)
                          (rplaca TeX-error-file file)))
                      (setq snippet (string-to-number (match-string 1))
                            box (unless
                                    (string= (match-string 2) "started")
                                  (if (match-string 4)
                                      (mapcar #'(lambda (x)
                                                  (* (preview-get-magnification)
                                                     (string-to-number x)))
                                              (list
                                               (match-string 4)
                                               (match-string 5)
                                               (match-string 6)))
                                    t))
                            counters (mapcar #'cdr preview-parsed-counters)

                            ;; And the line number to position the cursor.
                            line (progn
                                   (setq lpoint (point))
                                   (end-of-line)
                                   ;;  variant 1: profiling seems to indicate the regexp-heavy solution
                                   ;;  to be favorable.  Removing incomplete characters from the error
                                   ;;  context is an absolute nuisance.
                                   (and (re-search-forward "\
^l\\.\\([0-9]+\\) \\(\\.\\.\\.\\(?:\\^*\\(?:[89a-f][0-9a-f]\\|[]@-\\_?]\\)\\|\
\[0-9a-f]?\\)\\)?\\([^\n\r]*?\\)\r?
\\([^\n\r]*?\\)\\(\\(?:\\^+[89a-f]?\\)?\\.\\.\\.\\)?\r?$" nil t)
                                        (string-to-number (match-string 1))))
                            ;; And a string of the context to search for.
                            string (and line (match-string 3))
                            after-string (and line (buffer-substring
                                                    (+ (match-beginning 4)
                                                       (- (match-end 3)
                                                          (match-beginning 0)))
                                                    (match-end 4)))

                            ;; We may use these in another buffer.
                            offset (or (car TeX-error-offset) 0)
                            file (car TeX-error-file))
                      (when (and (stringp file)
                                 (or (string= file "<none>")
                                     (TeX-match-extension file)
                                     ;; OVERRIDE DIFFERENCE: we add
                                     ;; the following two options,
                                     ;; which support indirect buffers
                                     ;; for .tex files and org-mode
                                     ;; indirect source blocks.
				     (string-match "\\.tex\\(<\\([^>]+\\)>\\)*$" file)
				     (string-match "\\[ latex \\]\\*\\(<\\([^>]+\\)>\\)*$" file)))
                        ;; if we are the first time round, check for fast hooks:
                        (when (null parsestate)
                          (setq open-data
                                (save-excursion (funcall open-closure))
                                tempdir TeX-active-tempdir)
                          (dolist
                              (lst (if (listp TeX-translate-location-hook)
                                       TeX-translate-location-hook
                                     (list TeX-translate-location-hook)))
                            (let ((fast
                                   (and (symbolp lst)
                                        (get lst 'TeX-translate-via-list))))
                              (if fast
                                  (setq fast-hook
                                        (nconc fast-hook (list fast)))
                                (setq slow-hook
                                      (nconc slow-hook (list lst)))))))
                        ;; Functions in `TeX-translate-location-hook'
                        ;; may examine and modify the following variables.
                        (setq TeX-translate-location-file file
                              TeX-translate-location-line line
                              ;; TeX-translate-location-error error
                              TeX-translate-location-offset offset
                              ;; TeX-translate-location-context context
                              TeX-translate-location-string string)
                        (condition-case err
                            (save-excursion (mapc #'funcall slow-hook))
                          (error (preview-log-error err "Translation hook")))
                        (setq file TeX-translate-location-file
                              line TeX-translate-location-line
                              ;; error TeX-translate-location-error
                              offset TeX-translate-location-offset
                              ;; context TeX-translate-location-context
                              string TeX-translate-location-string)
                        (push (vector file (+ line offset)
                                      string after-string
                                      snippet box counters)
                              parsestate)))
                  ;; else normal error message
                  (forward-line)
                  (re-search-forward "^l\\.[0-9]" nil t)
                  (forward-line 2)))
               ((match-beginning 3)
                ;; New file -- Push on stack
                (push (match-string-no-properties 3) TeX-error-file)
                (push nil TeX-error-offset)
                (goto-char (match-end 3)))
               ((match-beginning 4)
                ;; End of file -- Pop from stack
                (when (> (length TeX-error-file) 1)
                  (pop TeX-error-file)
                  (pop TeX-error-offset))
                (goto-char (1+ (match-beginning 0))))
               ((match-beginning 5)
                ;; Hook to change line numbers
                (setq TeX-error-offset
                      (list (string-to-number (match-string 5)))))
               ((match-beginning 6)
                ;; Hook to change file name
                (setq TeX-error-file (list (match-string-no-properties 6))))
               ((match-beginning 7)
                (let ((var
                       (assoc (match-string-no-properties 7)
                              preview-parse-variables))
                      (offset (- (match-beginning 0) (match-beginning 8)))
                      (str (match-string-no-properties 8)))
                  ;; paste together continuation lines:
                  (while (= (- (length str) offset) 79)
                    (search-forward-regexp "^\\([^\n\r]*\\)\r?$")
                    (setq offset (- (length str))
                          str (concat str (match-string-no-properties 1))))
                  (when (and var
                             (string-match (nth 2 var) str))
                    (set (nth 1 var)
                         (funcall (nth 4 var)
                                  (match-string-no-properties
                                   (nth 3 var)
                                   str))))))))
            (when (null parsestate)
              (error "LaTeX found no preview images")))
        (unwind-protect
            (save-excursion
              (setq parsestate (nreverse parsestate))
              (condition-case err
                  (dolist (fun fast-hook)
                    (setq parsestate
                          (save-excursion (funcall fun parsestate))))
                (error (preview-log-error err "Fast translation hook")))
              (setq snippet 0)
              (dolist (state parsestate)
                (setq lsnippet snippet
                      file (aref state 0)
                      line (aref state 1)
                      string (aref state 2)
                      after-string (aref state 3)
                      snippet (aref state 4)
                      box (aref state 5)
                      counters (aref state 6))
                (unless (string= lfile file)
                  ;; TODO: we had deleted the above "unless"
                  ;; condition.  Should we keep it out, or was that
                  ;; accidental?
                  ;;
                  ;; TODO: document the next bit.
                  (let ((buffer (or (get-buffer (file-name-nondirectory file))
                                    (find-buffer-visiting file))))
		    (when buffer
		      (set-buffer buffer)))
		  (setq lfile file))
                (save-excursion
                  (save-restriction
                    (widen)
                    ;; a fast hook might have positioned us already:
                    (if (number-or-marker-p string)
                        (progn
                          (goto-char string)
                          (setq lpoint
                                (if (number-or-marker-p after-string)
                                    after-string
                                  (line-beginning-position))))
                      (if (and (eq (current-buffer) lbuffer)
                               (<= lline line))
                          ;; while Emacs does the perfectly correct
                          ;; thing even when when the line differences
                          ;; get zero or negative, I don't trust this
                          ;; to be universally the case across other
                          ;; implementations.  Besides, if the line
                          ;; number gets smaller again, we are probably
                          ;; rereading the file, and restarting from
                          ;; the beginning will probably be faster.
                          (progn
                            (goto-char lpoint)
                            (if (/= lline line)
                                (if (eq selective-display t)
                                    (re-search-forward "[\n\C-m]" nil
                                                       'end
                                                       (- line lline))
                                  (forward-line (- line lline)))))
                        (goto-char (point-min))
                        (forward-line (1- line)))
                      (setq lpoint (point))

                      ;; OVERRIDE DIFFERENCE: We insert the following
                      ;; block, which fixes a subtle bug; see
                      ;; https://www.mail-archive.com/bug-auctex@gnu.org/msg04327.html
		      (and
                       czm-preview--region-begin
		       (< (point) czm-preview--region-begin)
		       (goto-char czm-preview--region-begin))
                      
                      (cond
                       ((search-forward (concat string after-string)
                                        (line-end-position) t)
                        (backward-char (length after-string)))
                       ;;ok, transform ^^ sequences
                       ((search-forward-regexp
                         (concat "\\("
                                 (setq string
                                       (preview-error-quote
                                        string))
                                 "\\)"
                                 (setq after-string
                                       (preview-error-quote
                                        after-string)))
                         (line-end-position) t)
                        (goto-char (match-end 1)))
                       ((search-forward-regexp
                         (concat "\\("
                                 (if (string-match
                                      "^[^\0-\177]\\{1,6\\}" string)
                                     (setq string
                                           (substring string (match-end 0)))
                                   string)
                                 "\\)"
                                 (if (string-match
                                      "[^\0-\177]\\{1,6\\}$" after-string)
                                     (setq after-string
                                           (substring after-string
                                                      0 (match-beginning 0)))))
                         (line-end-position) t)
                        (goto-char (match-end 1)))
                       (t (search-forward-regexp
                           string
                           (line-end-position) t))))
                    (setq lline line
                          lbuffer (current-buffer))
                    (if box
                        (progn
                          (if (and lstart (= snippet lsnippet))
                              (setq close-data
                                    (nconc
                                     (preview-place-preview
                                      snippet
                                      (save-excursion
                                        (preview-back-command
                                         (= (prog1 (point)
                                              (goto-char lstart))
                                            lstart))
                                        (point))
                                      (point)
                                      (preview-TeX-bb box)
                                      (cons lcounters counters)
                                      tempdir
                                      (cdr open-data))
                                     close-data))
                            (with-current-buffer run-buffer
                              (preview-log-error
                               (list 'error
                                     (format
                                      "End of Preview snippet %d unexpected"
                                      snippet)) "Parser")))
                          (setq lstart nil))
                      ;; else-part of if box
                      (setq lstart (point) lcounters counters)
                      ;; >= because snippets in between might have
                      ;; been ignored because of TeX-default-extension
                      (unless (>= snippet (1+ lsnippet))
                        (with-current-buffer run-buffer
                          (preview-log-error
                           (list 'error
                                 (format
                                  "Preview snippet %d out of sequence"
                                  snippet)) "Parser"))))))))
          (preview-call-hook 'close (car open-data) close-data))))))





(defun czm-preview-override-kill-buffer-cleanup (&optional buf)
  "This is a cleanup function just for use in hooks.
Cleans BUF or current buffer.  The difference to
`preview-clearout-buffer' is that previews
associated with the last buffer modification time are
kept."
  (with-current-buffer (or buf (current-buffer))
    (unless (buffer-base-buffer)    ; do not clearout indirect buffers
      (save-restriction
	(widen)
	(preview-clearout (point-min) (point-max) (visited-file-modtime))))))

























(defvar czm-preview--active-environment-start nil)

(defvar czm-preview--timer nil)

(defvar-local czm-preview--style-hooks-applied nil
  "Has `TeX-update-style' been run in this buffer?")

(defvar-local czm-preview--timer-enabled nil
  "Is the preview timer is enabled in this buffer?
We want the preview timer to be active only in the current
buffer.  For this reason, it is a global object.  This local
variable keeps track of the buffers in which the timer should do
anything.")




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
  (when-let ((env-start (czm-preview--valid-environment-start)))
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
	   (_line-numbers         ; only for stuff occuring on one line
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
			 (- (point) 2))))))
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
  "Set `czm-preview--style-hooks-applied' to t.
This is a hook function for `TeX-update-style-hook'.  Used to
make sure we don't run `preview-region' before the style hooks
have been applied.  This is needed to be able to call `texmathp',
which in turn calls `LaTeX-verbatim-p', which in turn calls
`syntax-propertize'."
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
   (or (not czm-preview--region-time)
       (> (float-time) (+ czm-preview--region-time 0.25)))
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
  (advice-remove 'TeX-region-create #'czm-preview-override-TeX-region-create)
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

                             




;;; ------------------------------ THE END ------------------------------

(provide 'czm-preview)
;;; czm-preview.el ends here
