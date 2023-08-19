;;; czm-preview-override.el --- Overrides for tex.el/preview.el used in czm-preview.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
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

;; This file contains overrides for some functions defined in
;; preview.el.  The code is copy-pasted from tex.el/preview.el and modified
;; to suit my needs.

;;; Code:

(defun czm-preview-override-TeX-process-check (name)
  "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
  (let (process)
    (while (and (setq process (TeX-process name))
                (eq (process-status process) 'run))
      (cond
       ((eq (process-status process) 'run)
           (error "Cannot have two processes for the same document"))))))


(defun czm-preview-override-TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION.

Modification of TeX-region-create.  The only changed is that
three lines are commented out (see the source).

The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file."
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

(defun czm-preview-override-region (begin end)
  "Run preview on region between BEGIN and END."
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
  (setq-local preview-region--begin begin)
  (setq-local preview-region--end end)
  (setq-local preview-region--last-time (float-time))
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
stuff for the placement hook."
  (with-temp-message nil
    (let (TeX-error-file TeX-error-offset snippet box counters
			 file line
			 (lsnippet 0) lstart (lfile "") lline lbuffer lpoint
			 lcounters
			 string after-string
			 offset
			 parsestate (case-fold-search nil)
			 (run-buffer (current-buffer))
			 (run-directory default-directory)
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
;;;   Ok, here is a line by line breakdown:
;;;   match-alternative 1:
;;;   error indicator for TeX error, either style.
;;;   match-alternative 2:
;;;   The same, but file-line-error-style, matching on file name.
;;;   match-alternative 3:
;;;   Too ugly to describe in detail.  In short, we try to catch file
;;;   names built from path components that don't contain spaces or
;;;   other special characters once the file extension has started.
;;;
;;;   Position for searching immediately after the file name so as to
;;;   not miss closing parens or something.
;;;   (match-string 3) is the file name.
;;;   match-alternative 4:
;;;   )+\( \|$\)
;;;   a closing paren followed by the end of line or a space: a just
;;;   closed file.
;;;   match-alternative 5 (wrapped into one shy group with
;;;   match-alternative 6, so that the match on first char is slightly
;;;   faster):
;;;   !offset(\([---0-9]+\))
;;;   an AUCTeX offset message. (match-string 5) is the offset itself
;;;   !name(\([^)]+\))
;;;   an AUCTeX file name message.  (match-string 6) is the file name
;;;   TODO: Actually, the latter two should probably again match only
;;;   after a space or newline, since that it what \message produces.
;;;  disabled in prauctex.def:
;;;  \(?:Ov\|Und\)erfull \\.*[0-9]*--[0-9]*
;;;  \(?:.\{79\}
;;;  \)*.*$\)\|
;;;   This would have caught overfull box messages that consist of
;;;   several lines of context all with 79 characters in length except
;;;   of the last one.  prauctex.def kills all such messages.
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
;;;  variant 1: profiling seems to indicate the regexp-heavy solution
;;;  to be favorable.  Removing incomplete characters from the error
;;;  context is an absolute nuisance.
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
                (let ((buffer (or (get-buffer (file-name-nondirectory file)) (find-buffer-visiting file))))
		   (when buffer
		     (set-buffer buffer)))
		;; (set-buffer file)
		 ;; (set-buffer (if (string= file "<none>")
		 ;;                 (with-current-buffer run-buffer
		 ;;                   TeX-command-buffer)
		 ;;               (find-file-noselect
		 ;;                (expand-file-name file run-directory))))
		 (setq lfile file)
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
		      (unless preview-region--begin
			(error "The variable preview-region--begin is nil, but shouldn't be!"))
		      (when
			  (< (point) preview-region--begin)
			(goto-char preview-region--begin))
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






(provide 'czm-preview-override)
;;; czm-preview-override.el ends here
