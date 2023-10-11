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

;; This package supplies a minor mode `czm-preview-mode' that augments
;; `preview.el' from AUCTeX in the following ways:
;;
;; - Automatic previewing of the visible part of the buffer.
;;
;; - Previews remain visible during edits.
;;
;; - Equation numbers are extracted from the .aux file, when possible.
;;
;; - Previews work in indirect and non-file buffers (e.g., indirect
;; org mode source blocks) -- for this feature to work, you must
;; customize the variable `czm-preview-TeX-master' to point to a valid
;; TeX file with a suitable preamble.
;;
;; To use this package, first install AUCTeX, then
;;
;;   M-x czm-preview-mode
;;
;; in a LaTeX-mode buffer visiting a file.  For indirect and non-file
;; buffers, first customize as described above, e.g., by including
;; 
;;   (setq czm-preview-TeX-master "path/to/master.tex")
;;
;; in your init file.
;;
;; ISSUE: if `czm-preview-TeX-master' is non-nil and
;; `czm-preview-mode' is activated, then ordinary LaTeX compilation
;; via \\[TeX-command-master] probably won't work correctly.  One
;; workaround is to disable `czm-preview-mode' when compiling.
;; Another approach (what I do) is to avoid \\[TeX-command-master]
;; altogether and just have a latexmk process running in the
;; background for each open TeX file.
;;
;; TIP: dvi files generate faster than pdf, so for a snappier preview,
;; use (TeX-PDF-mode 0).
;;
;; CAUTION: this minor mode is implemented via ADVICE to the packages
;; tex.el/preview.el, and so might be incompatible with future
;; versions of those.

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

(defcustom czm-preview-max-region-radius 20000
  "Maximum radius of region to be previewed."
  :type 'number
  :group 'czm-preview)

(defcustom czm-preview-timer-interval 0.3
  "Interval for preview timer.
For this to have any effect, it must be set before
czm-preview-mode is activated for the first time."
  :type 'number
  :group 'czm-preview)

(defcustom czm-preview-enable-automatic-previews t
  "Enable automatic previews?
Set this to nil to turn off the automatic previews (while still
retaining the other features of the provided minor mode)."
  :type 'boolean
  :group 'czm-preview)

(defcustom czm-preview-enable-preview-face nil
  "If non-nil, enable `preview-face' for previews.
I prefer to leave this disabled, so that transient mark mode
works when editing inside a preview."
  :type 'boolean
  :group 'czm-preview)

(defcustom czm-preview-characters-above-to-preview 5000
  "Controls how many characters above point to preview."
  :type 'integer
  :group 'czm-preview)

(defcustom czm-preview-characters-below-to-preview 10000
  "Controls how many characters below poitn to preview."
  :type 'integer
  :group 'czm-preview)

;;; ------------------------------ INTERNAL VARIABLES ------------------------------

(defvar czm-preview--debug nil
  "If non-nil, print debug messages.")

(defvar-local czm-preview--region-time nil)

(defvar-local czm-preview--region-begin nil)

(defvar-local czm-preview--region-end nil)

(defvar-local czm-preview--active-region nil
  "Stores the region currently being processed by `preview-region'.")

(defvar czm-preview--active-environment-start nil)

(defvar czm-preview--timer nil)

(defvar-local czm-preview--style-hooks-applied nil
  "Has `TeX-update-style' been run in this buffer?")
;; Could consider instead trying to use `font-lock-ensure'?

(defvar-local czm-preview--timer-enabled nil
  "Is the preview timer is enabled in this buffer?
We want the preview timer to be active only in the current
buffer.  For this reason, it is a global object.  This local
variable keeps track of the buffers in which the timer should do
anything.")

(defvar-local czm-preview--disabled-image nil
  "The preview image that was most recently disabled.
This is used to avoid flickering or construction signs when a new
preview is generated that replaces an old one.")

(defvar-local czm-preview--disabled-region-begin nil
  "Beginning of the preview region that was most recently disabled.")

(defvar-local czm-preview--keepalive t
  "Used to keep track of when we should preview some more.")

(defvar czm-preview--internal-tmp-files nil
  "List of auxiliary temporary files, used to reduce flickering.")

(defvar-local czm-preview--preview-auto-cache-preamble-orig nil
  "Original value of `preview-auto-cache-preamble'.")

(defvar-local czm-preview--TeX-master-orig nil
  "Original value of `TeX-master'.")

;;; ------------------------------ OVERRIDES ------------------------------

;; These overrides are copy/pasted from tex.el/preview.el and edited
;; to suit our needs.

(defun czm-preview-override-TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command.

OVERRIDE DIFFERENCES: We never kill an ongoing process.  If there
is a conflict, we always abort the process that we are about to
run.  We do not ask the user to make a decision."
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

(defun czm-preview-override-region (begin end)
  "Run preview on region between BEGIN and END.

OVERRIDE DIFFERENCES:

1. We add a preprocessing step to add label numbers as tags.

2. When we call `TeX-region-create', the third argument is the
name of the buffer (rather than either <none> or the name of the
file).  This is used for TODO

3. We store information about the region being processed (BEGIN,
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

1. We add support for indirect buffers and org-mode source
blocks.  This requires our modifications to `preview-region'.

2. We fix a subtle bug that shows up when `preview-region' is
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
                   (let ((point-current (point)))
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
                                 (let* ((region-beg (save-excursion
                                                      (preview-back-command
                                                       (= (prog1 (point)
                                                            (goto-char lstart))
                                                          lstart))
                                                      (point)))
                                        (region-end (point))
                                        (ovl (preview-place-preview
                                             snippet
                                             region-beg
                                             region-end
                                             (preview-TeX-bb box)
                                             (cons lcounters counters)
                                             tempdir
                                             (cdr open-data)))
                                        (ov (car ovl)))
                                   (setq close-data
                                         (nconc ovl close-data))
                                   (when (and
                                          (<= region-beg point-current)
                                          (< point-current region-end))
                                    (preview-toggle ov)
                                    (push ov preview-temporary-opened)))
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
                                     snippet)) "Parser")))))))))
          (preview-call-hook 'close (car open-data) close-data))))))

(defun czm-preview-override-kill-buffer-cleanup (&optional buf)
  "This is a cleanup function just for use in hooks.
Cleans BUF or current buffer.  The difference to
`preview-clearout-buffer' is that previews
associated with the last buffer modification time are
kept.

OVERRIDE DIFFERENCE: we add \"unless (buffer-base-buffer)\" in
the appropriate place, so that this function is not called when
an *indirect* buffer is called.  This is one of many changes that
allow us to work with previews in indirect buffers."
  (with-current-buffer (or buf (current-buffer))
    (unless (buffer-base-buffer)    ; do not clearout indirect buffers
      (save-restriction
	(widen)
	(preview-clearout (point-min) (point-max) (visited-file-modtime))))))

(defun czm-preview--valid-environment-start ()
  "Return start of current equation-like environment.
Return nil if not currently in such an environment."
  (and
   (texmathp)
   (member (car texmathp-why) czm-preview-valid-environments)
   (cdr texmathp-why)))

(defun czm-preview--around-place-preview (orig-func &rest args)
  "Used as :around advice for the function `preview-place-preview'.
The purpose of this wrapper is to conditionally modify the
overlay positioning.  If `czm-preview--active-environment-start'
is non-nil and equal to the starting position of the current
environment, then the start and end positions for the overlay are
both adjusted to be at the start of the environment.  The result
is that the overlay does not obscure the environment itself.

ORIG-FUNC is the original function being advised, and ARGS are
its argument."
  (setq czm-preview--active-region nil)
  (cl-destructuring-bind (snippet start end box counters tempdir place-opts) args
    ;; maybe we should do this more aggressively? that is, we include
    ;; in our timer function a step that, if we've already previewed
    ;; everything else nearby, looks at the current environment, and
    ;; previews that if it's been edited, replacing the previous
    ;; preview?  maybe?  would be nice to get rid of the additional
    ;; function.
    ;;
    ;; Maybe we can just check here if the start of the current
    ;; environment coincides with the start of the active environment?
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


(defun czm-preview-override-inactive-string (ov)
  "Generate before-string for an inactive preview overlay OV.
This is for overlays where the source text has been clicked
visible.  For efficiency reasons it is expected that the buffer
is already selected and unnarrowed.

OVERRIDE DIFFERENCE: we take `(overlay-get ov \='preview-image)'
rather than `preview-icon'."
  (concat
   (preview-make-clickable (overlay-get ov 'preview-map)
                           (overlay-get ov 'preview-image)
                           ;; preview-icon
                           "\
%s redisplays preview
%s more options")
   ;; icon on separate line only for stuff starting on its own line
   (with-current-buffer (overlay-buffer ov)
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (overlay-start ov))
         (if (bolp) "\n" ""))))))

(defun czm-preview-override-preview-disable (ovr)
  "Change overlay behaviour of OVR after source edits.

OVERRIDE DIFFERENCES: commented out two lines, and added a
\"copy\".  The commenting has the effect of leaving previews
visible during edits.  The copy does TODO"
  (overlay-put ovr 'queued nil)
  (preview-remove-urgentization ovr)
  ;; (overlay-put ovr 'preview-image nil)
  (overlay-put ovr 'timestamp nil)
  (setcdr (overlay-get ovr 'strings) (preview-disabled-string ovr))
  ;; (preview-toggle ovr)
  (overlay-put ovr 'preview-state 'disabled)
  (dolist (filename (overlay-get ovr 'filenames))
    (condition-case nil
        (let* ((src (car filename))
               (dst (expand-file-name
                     (concat
                      "test"
                      (format-time-string "%Y%m%dT%H%M%S%6N"))
                     (file-name-parent-directory
                      (file-name-directory (car filename))))))
          (copy-file src dst t)
          (set-file-times dst (current-time))
          (push dst czm-preview--internal-tmp-files)
          (setq czm-preview--disabled-image
                (create-image dst 'png nil
                              :ascent
                              (plist-get
                               (cdar (overlay-get ovr 'preview-image))
                               ':ascent)))
          (setq czm-preview--disabled-region-begin (overlay-start ovr))

          ;; (setq czm-preview--disabled-image (preview-make-image 'test))
          (preview-delete-file filename))
      (file-error nil))
    (overlay-put ovr 'filenames nil)))

(defun czm-preview-override-gs-place (ov snippet box run-buffer tempdir ps-file _imagetype)
  "Generate an image placeholder rendered over by Ghostscript.
This enters OV into all proper queues in order to make it render
this image for real later, and returns the overlay after setting
a placeholder image.  SNIPPET gives the number of the
snippet in question for the file to be generated.
BOX is a bounding box if we already know one via TeX.
RUN-BUFFER is the buffer of the TeX process,
TEMPDIR is the correct copy of `TeX-active-tempdir',
PS-FILE is a copy of `preview-ps-file', IMAGETYPE is the image type
for the file extension."
  (overlay-put ov 'filenames
               (unless (eq ps-file t)
                 (list
                  (preview-make-filename
                   (or ps-file
                       (format "preview.%03d" snippet))
                   tempdir))))
  (overlay-put ov 'queued
               (vector box nil snippet))
  ;; (overlay-put ov 'preview-image
  ;;              (list (cons 'image (cdr preview-nonready-icon))))
  (if (and
        czm-preview--disabled-image
        (= czm-preview--disabled-region-begin (overlay-start ov)))
      (progn
        (overlay-put ov 'preview-image
                     (list (cons 'image (cdr czm-preview--disabled-image))))
        (setq czm-preview--disabled-image nil))
    (overlay-put ov 'preview-image
                 (list (preview-icon-copy preview-nonready-icon))))
  ;; (when czm-preview--disabled-image
  ;;   (overlay-put ov 'preview-image czm-preview--disabled-image))
  (preview-add-urgentization #'preview-gs-urgentize ov run-buffer)
  (list ov))

(defun czm-preview-override-toggle (ov &optional arg event)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t displays the overlay,
nil displays the underlying text, and `toggle' toggles.
If EVENT is given, it indicates the window where the event
occured, either by being a mouse event or by directly being
the window in question.  This may be used for cursor restoration
purposes.

OVERRIDE DIFFERENCE: \"when czm-preview-enable-preview-face\"."
  (let ((old-urgent (preview-remove-urgentization ov))
        (preview-state
         (if (if (eq arg 'toggle)
                 (null (eq (overlay-get ov 'preview-state) 'active))
               arg)
             'active
           'inactive))
        (strings (overlay-get ov 'strings)))
    (unless (eq (overlay-get ov 'preview-state) 'disabled)
      (overlay-put ov 'preview-state preview-state)
      (if (eq preview-state 'active)
          (progn
            (overlay-put ov 'category 'preview-overlay)
            (if (eq (overlay-start ov) (overlay-end ov))
                (overlay-put ov 'before-string (car strings))
              (dolist (prop '(display keymap mouse-face help-echo))
                (overlay-put ov prop
                             (get-text-property 0 prop (car strings))))
              (overlay-put ov 'before-string nil))
            (overlay-put ov 'face nil))
        (dolist (prop '(display keymap mouse-face help-echo))
          (overlay-put ov prop nil))
        (when czm-preview-enable-preview-face
          (overlay-put ov 'face 'preview-face))
        (unless (cdr strings)
          (setcdr strings (preview-inactive-string ov)))
        (overlay-put ov 'before-string (cdr strings)))
      (if old-urgent
          (apply #'preview-add-urgentization old-urgent))))
  (if event
      (preview-restore-position
       ov
       (if (windowp event)
           event
         (posn-window (event-start event))))))


;;; ------------------------------ HOOKS ------------------------------

(defun czm-preview--after-change-function (beg end _length)
  "Hook function for `czm-preview-mode'.
Checks if the modification affects the active preview region and,
if so, kills the current preview job.

BEG is the start of the modified region, END is the end of the
 region, and LENGTH is the length of the modification."
  (when (eq major-mode 'latex-mode)
    ;; If a region is currently being previewed...
    (when-let ((active-region-end (cdr czm-preview--active-region)))
      (with-current-buffer (get-buffer-create "*DebugPreview*")
	(goto-char (point-max))
	(insert (format-time-string "%Y-%m-%d %T.%3N\n"))
	(insert (format "region being modified: (%s %s)\n" beg end))
	(insert (format "active preview region: (%s %s)\n"
			(car czm-preview--active-region)
			(cdr czm-preview--active-region))))
      ;; ...and the edit occurs before that region, then cancel the
      ;; preview.
      (when (< beg active-region-end)
	(ignore-errors (TeX-kill-job))))))

(defun czm-preview--post-command-function ()
  "Function called after each command in `czm-preview-mode'.
If a region is currently being previewed, and it's not a
\"current environment\" preview, and we execute any command
whatsoever with point inside that region, then kill the preview."
  (setq-local czm-preview--keepalive t)
  (and
   nil
   (eq major-mode 'latex-mode)
   ;; a region is currently being previewed
   czm-preview--active-region
   ;; it's not a "current environment" preview.  kinda hacky.
   ;; maybe eliminate this, if you end up eliminating that?  I
   ;; mean, why not just always position the preview at the start
   ;; of the environment?
   (not (eq czm-preview--active-environment-start
	    (car czm-preview--active-region)))
   (texmathp)
   (<= (car czm-preview--active-region) (point))
   (< (point) (cdr czm-preview--active-region))
   (ignore-errors (TeX-kill-job))))



(defun czm-preview--flag-style-hooks-applied ()
  "Set `czm-preview--style-hooks-applied' to t.
This is a hook function for `TeX-update-style-hook'.  It is used
to make sure we don't run `preview-region' before the style hooks
have been applied.  This is needed to be able to call `texmathp',
which in turn calls `LaTeX-verbatim-p', which in turn calls
`syntax-propertize'."
  (setq-local czm-preview--style-hooks-applied t))

;;; ------------------------------ HELPERS ------------------------------

(defun czm-preview--processes ()
  "Return list of preview processes for current LaTeX document."
  (or
   (TeX-process
    (concat default-directory "_region_"))
   ;; (get-buffer-process (TeX-process-buffer-name (concat (TeX-master-directory) (TeX-active-master))))
   (get-buffer-process (TeX-process-buffer-name (concat (TeX-active-master))))))


(defun czm-preview--find-end-of-block (why bound)
  "Find end of LaTeX math block starting with WHY before BOUND."
  (catch 'found
    (let ((end-regexp
           (cond
             ((string= why "$") "\\$")
             ((string= why "$$") "\\$\\$")
             ((string= why "\\[") "\\\\\\]")
             ((string= why "\\(") "\\\\)")
             ((member why texmathp-environments)
              (concat "\\\\end{" (regexp-quote why) "}")))))
      (while (re-search-forward end-regexp bound t)
        (when (and (not (TeX-in-comment))
                   ;; (not (LaTeX-verbatim-p))
                   )
          (let ((inner-end (match-beginning 0))
                (end (point)))
            (throw 'found
              (cons inner-end end))))))))

(defun czm-preview--find-next-math-block (&optional bound)
  "Find next LaTeX math block before BOUND."
  (interactive)
  (unless bound (setq bound (point-max)))
  (catch 'found
    (while (re-search-forward
            (concat "\\(" "\\\\begin{\\([^{}]*\\)}"
                    "\\|" "\\$\\$?"
                    "\\|" "\\\\\\["
                    "\\|" "\\\\(" "\\)")
            bound t)
      (let ((begin (match-beginning 0))
            (inner-begin (match-end 0))
            (env-name (match-string 2))
            (match (match-string 0)))
        (when (and (not (TeX-in-comment))
                   (texmathp)
                   ;; (not (LaTeX-verbatim-p))
                   (or (null env-name)
                       (member env-name texmathp-environments)))
          (when-let* ((why (or env-name match))
                      (block-bound (save-excursion
                                     (if (re-search-forward
                                          "[\n\r][ \t]*[\n\r]"
                                          bound t)
                                         (match-beginning 0)
                                       bound)))
                      (end-cons (czm-preview--find-end-of-block why block-bound))
                      (inner-end (car end-cons))
                      (end (cdr end-cons)))
            (throw 'found
                   (list begin inner-begin inner-end end))))))))

(defun czm-preview--find-next-math-block-show-contents (&optional bound)
  "Find next LaTeX math block before BOUND, and show its contents."
  (interactive)
  (when-let ((block (czm-preview--find-next-math-block bound)))
    (cl-destructuring-bind (_begin inner-begin inner-end _end) block
      (message (buffer-substring-no-properties inner-begin inner-end)))))

(defun czm-preview--find-top-level-math-intervals (beg end)
  "Find top-level LaTeX math envs between BEG and END.
Return list of cons cells containing beg and end positions."
  (save-excursion
    (let ((math-intervals '()))
      (goto-char beg)
      (while-let
          ((block (czm-preview--find-next-math-block end)))
        (cl-destructuring-bind (begin inner-begin inner-end end) block
          ;; check that contents are non-empty
          (when (string-match-p "[^[:space:]\n\r]"
                                (buffer-substring-no-properties
				 inner-begin inner-end))
	    (push (cons begin end) math-intervals))))
      (nreverse math-intervals))))

(defun czm-preview--active-or-inactive-preview-at-point-p (&optional pos)
  "Is there a non-disabled preview overlay at POS?
POS defaults to (point)."
  (cl-intersection (mapcar
		    (lambda (ov) (overlay-get ov 'preview-state))
		    (overlays-at (or pos (point))))
		   '(active inactive)))

(defun czm-preview--non-nil-intervals (input-list)
  "Return list of intervals of non-nil values in INPUT-LIST."
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

;; this is either a string (regexp), or a list of strings (to be
;; passed to regexp-opt), or a function (a predicate that accepts a
;; string and returns t if it matches)
(defcustom czm-preview-regions-not-to-preview nil
  "Describes which regions not to be previewed automatically.
This is either a string (regexp), or a list of strings (to be
passed to `regexp-opt'), or a function (a predicate that accepts
a string and returns t if it matches)."
  :type '(choice (string :tag "Regexp")
                 (repeat :tag "List of strings" string)
                 (function :tag "Predicate"))
  :group 'czm-preview)





(defun czm-preview--get-stale-chunk (beg end first)
  "Get convex hull of some stale envs between BEG and END.
Return cons cell of beginning and ending positions.  FIRST
determines whether to take from the beginning or the end."
  (when czm-preview--debug
    (message "czm-preview--first-stale-chunk: %s %s" beg end))
  ;; (message "end - beg : %s" (- end beg))
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
                          (not
                           (cond
                            ((stringp czm-preview-regions-not-to-preview)
                             (string-match-p czm-preview-regions-not-to-preview region))
                            ((listp czm-preview-regions-not-to-preview)
                             (string-match-p (regexp-opt czm-preview-regions-not-to-preview) region))
                            ((functionp czm-preview-regions-not-to-preview)
                             (funcall czm-preview-regions-not-to-preview region))
                            (t t)))
                          ;; (not (string-match-p (regexp-opt '("<++>" "<+++>"))
		          ;; 	             region))
		          ))
		       top-level-math-intervals regions))
           (_line-numbers        ; only for stuff occuring on one line
            (mapcar (lambda (interval)
		      (let ((line-beg (line-number-at-pos (car interval) ))
		            (line-end (line-number-at-pos (cdr interval) )))
		        (when (equal line-beg line-end)
		          line-beg)))
	            top-level-math-intervals)))
      (when-let* ((non-nil-intervals (czm-preview--non-nil-intervals staleness))
                  (first-visible-chunk
                   (if first
                       (car non-nil-intervals)
                     (car (last non-nil-intervals)))))
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

(defun czm-preview--first-stale-chunk (beg end)
  "Get convex hull of initial stale envs between BEG and END.
Return cons cell of beginning and ending positions."
  (czm-preview--get-stale-chunk beg end t))

(defun czm-preview--last-stale-chunk (beg end)
  "Get convex hull of final stale envs between BEG and END.
Return cons cell of beginning and ending positions."
  (czm-preview--get-stale-chunk beg end nil))

(defun czm-preview--preview-some-chunk ()
  "Run `preview-region' on an appropriate region.
Identify top level math intervals in the window.  Find the first
contiguous group of intervals at which there are no active or
inactive previews at point.  Call `preview-region' on the
smallest interval that contains this group."
  (interactive)
  (when czm-preview--debug
    (message "czm-preview--preview-some-chunk"))
  (unless (czm-preview--processes)
    (let*
	((above-window-beg (max (point-min) (- (point) czm-preview-characters-above-to-preview)))
         (below-window-end (min (point-max) (+ (point) czm-preview-characters-below-to-preview)))
         (action
          (lambda (interval)
            (let ((inhibit-message t)
                  (beg (car interval))
                  (end (cdr interval)))
              (when czm-preview--debug
                (message (format "previewing region (%s %s)" beg end)))
              (preview-region beg end)))))
      ;; TODO.  Okay, sometimes this functions calls
      ;; czm-preview--first-stale-chunk with reversed
      ;; arguments.  You're not really sure what's up with that.  For
      ;; now you'll just "plug the hole" by having
      ;; czm-preview--first-stale-chunk do nothing in such cases.
      (let (interval
            (begin-document
             (or
              ;; if buffer contains \begin{document},
              ;; then this is the beginning of that.
              ;; otherwise it's the beginning of the
              ;; buffer.
              (save-excursion
                (goto-char (point-min))
                (when (search-forward "\\begin{document}" nil t)
                  (match-beginning 0)))
              (point-min))))
        (setq-local czm-preview--keepalive t)
        (cond
         ((setq interval (czm-preview--last-stale-chunk
                          (max begin-document above-window-beg)
			  (point)))
          (when czm-preview--debug
            (message "above-window-beg: %s, point: %s" above-window-beg (point)))
	  (funcall action interval))
         ((setq interval (czm-preview--first-stale-chunk
                          (max begin-document (point))
                          below-window-end))
          (when czm-preview--debug
            (message "point: %s, below-window-end: %s" (point) below-window-end))
	  (funcall action interval))
         ((and
           (> (point) begin-document)
           (texmathp)
           (let ((why (car texmathp-why))
                 (beg (cdr texmathp-why)))
             (unless (czm-preview--active-or-inactive-preview-at-point-p
                      beg)
               (when-let* ((bound (save-excursion
                                    (if (re-search-forward
                                         "[\n\r][ \t]*[\n\r]"
                                         (point-max) t)
                                        (match-beginning 0)
                                      (point-max))))
                           (end (cdr (save-excursion
                                       (czm-preview--find-end-of-block why bound)))))
                 (unless (and (string= why "$")
                              (string-match "[\n\r]"
                                            (buffer-substring-no-properties beg end)))
                   (funcall action (cons beg end))))))))
         (t
          (setq-local czm-preview--keepalive nil)))))))


(defun czm-preview-mode-conditionally-enable ()
  "Enable `czm-preview-mode' if appropriate.
Check that we are not visiting a bbl file."
  (when
      (not
       (and
        (buffer-file-name)
        (string-match-p "\\.bbl\\'" (buffer-file-name))))
    (czm-preview-mode 1)))

(defun czm-preview--timer-function ()
  "Function called by the preview timer to update LaTeX previews."
  (interactive)

  ;; for each file in czm-preview--internal-tmp-files more than 10
  ;; seconds old, delete and remove from list
  (let (newlist)
    (dolist (filename czm-preview--internal-tmp-files)
      (when (> (- (float-time) (float-time (nth 5 (file-attributes filename))))
               10)
        (delete-file filename))
      (unless (file-exists-p filename)
        (push filename newlist)))
    (setq czm-preview--internal-tmp-files newlist))

  (and
   (eq major-mode 'latex-mode)
   ;; file extension is not
   czm-preview-enable-automatic-previews
   czm-preview--timer
   czm-preview--timer-enabled
   TeX-style-hook-applied-p
   ;; czm-preview--style-hooks-applied
   ;; font-lock-set-defaults ;; This is key.
   czm-preview--keepalive
   (or (not czm-preview--region-time)
       (> (float-time) (+ czm-preview--region-time 0.25)))
   ;; (not czm-preview--active-region)
   (czm-preview--preview-some-chunk)))

(defun czm-preview--init ()
  "Initialize advice and hooks for `czm-preview'."
  (add-hook 'after-change-functions #'czm-preview--after-change-function nil t)
  (add-hook 'post-command-hook #'czm-preview--post-command-function nil t)
  (add-hook 'TeX-update-style-hook #'czm-preview--flag-style-hooks-applied nil t)
  ;; (add-hook 'before-save-hook #'czm-preview-current-environment nil t)
  (advice-add 'preview-region :override #'czm-preview-override-region)
  (advice-add 'preview-place-preview :around #'czm-preview--around-place-preview)
  (advice-add 'preview-parse-messages :override #'czm-preview-override-parse-messages)
  (advice-add 'preview-kill-buffer-cleanup :override #'czm-preview-override-kill-buffer-cleanup)
  (advice-add 'TeX-region-create :override #'czm-preview-override-TeX-region-create)
  (advice-add 'TeX-process-check :override #'czm-preview-override-TeX-process-check)
  (advice-add 'preview-inactive-string :override #'czm-preview-override-inactive-string)
  (advice-add 'preview-disable :override #'czm-preview-override-preview-disable)
  (advice-add 'preview-gs-place :override #'czm-preview-override-gs-place)
  (advice-add 'preview-toggle :override #'czm-preview-override-toggle))

(defun czm-preview--close ()
  "Remove advice and hooks for `czm-preview'."
  (remove-hook 'after-change-functions #'czm-preview--after-change-function t)
  (remove-hook 'post-command-hook #'czm-preview--post-command-function t)
  (remove-hook 'TeX-update-style-hook #'czm-preview--flag-style-hooks-applied t)
  ;; (remove-hook 'before-save-hook #'czm-preview-current-environment t)
  (advice-remove 'preview-region #'czm-preview-override-region)
  (advice-remove 'preview-place-preview #'czm-preview--around-place-preview)
  (advice-remove 'preview-parse-messages #'czm-preview-override-parse-messages)
  (advice-remove 'preview-kill-buffer-cleanup #'czm-preview-override-kill-buffer-cleanup)
  (advice-remove 'TeX-region-create #'czm-preview-override-TeX-region-create)
  (advice-remove 'TeX-process-check #'czm-preview-override-TeX-process-check)
  (advice-remove 'preview-inactive-string #'czm-preview-override-inactive-string)
  (advice-remove 'preview-disable #'czm-preview-override-preview-disable)
  (advice-remove 'preview-gs-place #'czm-preview-override-gs-place)
  (advice-remove 'preview-toggle #'czm-preview-override-toggle))

(defun czm-preview--reset-timer ()
  "Reset the preview timer."
  (interactive)
  (when czm-preview--timer
    (cancel-timer czm-preview--timer)
    (setq czm-preview--timer nil))
  (setq czm-preview--timer
        (run-with-timer czm-preview-timer-interval czm-preview-timer-interval #'czm-preview--timer-function)))

;;; --------------------------------- COMMANDS ---------------------------------

;; TODO: it should not be possible to activate the mode in a non-file
;; buffer unless czm-preview-TeX-master is non-nil.
(define-minor-mode czm-preview-mode
  "Minor mode for running LaTeX preview on a timer."
  :lighter nil
  :global nil
  :group 'czm-preview
  (if czm-preview-mode
    (progn
      ;; TODO: is there some way to get these checks to "abort" the
      ;; activation of the mode?
      (unless (eq major-mode 'latex-mode)
        (user-error "czm-preview-mode can only be activated in LaTeX buffers"))
      (unless (buffer-file-name)
        (unless czm-preview-TeX-master
          (user-error "czm-preview-mode can only be activated in file buffers, unless
you point `czm-preview-TeX-master' to a file -- see the README"))
        (unless (file-exists-p czm-preview-TeX-master)
          (user-error
           (format "czm-preview-TeX-master (%s) does not exist" czm-preview-TeX-master))))

      (czm-preview--init)
      
      (czm-preview--reset-timer)
      (setq-local czm-preview--timer-enabled t)
      
      (setq-local czm-preview--keepalive t)
      
      (setq-local czm-preview--TeX-master-orig TeX-master)
      (when czm-preview-TeX-master
        (setq-local TeX-master czm-preview-TeX-master))
      (setq-local czm-preview--preview-auto-cache-preamble-orig preview-auto-cache-preamble)
      (setq-local preview-auto-cache-preamble t)

      (add-hook 'kill-emacs-hook
                (lambda ()
                  (dolist (file czm-preview--internal-tmp-files)
                    (ignore-errors (delete-file file))))))
    
    ;; Disable the timer.
    (setq-local czm-preview--timer-enabled nil)
    ;; Hacky:
    (setq-local czm-preview--active-region nil)
    

    ;; I think the hooks add a benefit anyway, right?  Maybe just want a separate function
    (czm-preview--close)

    (setq-local TeX-master czm-preview--TeX-master-orig)
    (setq-local preview-auto-cache-preamble czm-preview--preview-auto-cache-preamble-orig)))



;; TODO: sort the rest of this stuff out.

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

;; profiling: (let ((time (current-time))) (czm-preview--preview-some-chunk) (let ((time2 (current-time))) (message "time: %s msec" (* 1000 (float-time (time-subtract time2 time))))))

;;; ------------------------------ THE END ------------------------------

(provide 'czm-preview)
;;; czm-preview.el ends here
