;;; nodejs-repl.el --- Run Node.js REPL

;; Copyright (C) 2012-2015  Takeshi Arabiki

;; Author: Takeshi Arabiki
;; Version: 0.1.0

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program is derived from comint-mode and provides the following features.
;;
;;  * TAB completion same as Node.js REPL
;;  * file name completion in string
;;  * incremental history search
;;
;;
;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:
;;
;;    (require 'nodejs-repl)
;;
;; Type M-x nodejs-repl to run Node.js REPL.
;; See also `comint-mode' to check key bindings.
;;

(require 'cc-mode)
(require 'comint)
(require 'ansi-color)

(defgroup nodejs-repl nil
  "Run Node.js REPL and communicate the process."
  :group 'processes)

(defconst nodejs-repl-version "0.1.0"
  "Node.js mode Version.")

(defcustom nodejs-repl-command "node"
  "Node.js command used in `nodejs-repl-mode'."
  :group 'nodejs-repl
  :type 'string)

(defcustom nodejs-repl-arguments '()
  "Command line parameters forwarded to `nodejs-repl-command'."
  :group 'nodejs-repl
  :type '(repeat string))

(defcustom nodejs-repl-prompt "> "
  "Node.js prompt used in `nodejs-repl-mode'."
  :group 'nodejs-repl
  :type 'string)


(defcustom nodejs-repl-input-ignoredups t
  "If non-nil, don't add input matching the last on the input ring.

See also `comint-input-ignoredups'"
  :group 'nodejs-repl
  :type 'boolean)

(defcustom nodejs-repl-process-echoes t
  "If non-nil, Node.js does not echo any input.

See also `comint-process-echoes'"
  :group 'nodejs-repl
  :type 'boolean)

(defvar nodejs-repl-mode-hook nil
  "Functions runafter `nodejs-repl' is started.")

(defvar nodejs-repl-process-name "nodejs"
  "process name of Node.js REPL.")

(defvar nodejs-repl-temp-buffer-name "*nodejs-repl-command-output*")

(defvar nodejs-repl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar nodejs-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'comint-dynamic-complete)
    (define-key map (kbd "C-c C-c") 'nodejs-repl-quit-or-cancel)
    map))

;; process.stdout.columns should be set.
;; Node.js 0.8 and 0.10 uses this value as the maximum number of columns,
;; but process.stdout.columns in Emacs is infinity because Emacs returns 0 as winsize.ws_col.
;; The completion candidates won't be displayed if process.stdout.columns is infinity.
;; see also `handleGroup` function in readline.js
(defvar nodejs-repl-code-format
  (concat
   "process.stdout.columns = %d;"
   "require('repl').start('%s', null, null, true, false, "
   "require('repl')['REPL_MODE_' + '%s'.toUpperCase()])"))

(defvar nodejs-repl-extra-espace-sequence-re "\\(\x1b\\[[0-9]+[GJK]\\)")

(defvar nodejs-repl-ansi-color-sequence-re "\\(\x1b\\[[0-9]+m\\)")

;;; if send string like "a; Ma\t", return a; Math\x1b[1G> a; Math\x1b[0K\x1b[10G
(defvar nodejs-repl-prompt-re-format
  (concat
   "\x1b\\[1G"
   "\\("
   "\x1b\\[0J%s.*\x1b\\[[0-9]+G.*"  ; for Node.js 0.8
   "\\|"
   "%s.*\x1b\\[0K\x1b\\[[0-9]+G.*"  ; for Node.js 0.4 or 0.6
   "\\)"
   "$"))
(defvar nodejs-repl-prompt-re
  (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
;;; not support Unicode characters
(defvar nodejs-repl-require-re
  (concat
   "\\(?:^\\|\\s-\\|[-+*/%&|><!;{}()[]\\|\\]\\)"  ; delimiter
   "require\\s-*(\\s-*"
   "\\("
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*"             ; double quote
   "\\|"
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*"                ; single quote
   "\\)"
   "$"))

(defvar nodejs-repl-cache-token "")
(defvar nodejs-repl-cache-candidates ())


;;;--------------------------
;;; Private functions
;;;--------------------------
(defun nodejs-repl--in-string-p (&optional pos)
  "Return non-nil if point is inside string"
  (nth 3 (syntax-ppss pos)))

(defun nodejs-repl--extract-require-argument (string)
  (if (string-match nodejs-repl-require-re string)
      (match-string 1 string)))

(defun nodejs-repl--get-last-token (string)
  "Return the last token in the string."
  (if (string-match "\\([._$]\\|\\w\\)+$" string)
      (match-string 0 string)))

;;; TODO:
;;; * the case that a command is sent while another command is being prossesed
;;; * the case that incomplete commands are sent like "1 +\n"
;;; * support commands which output a string without CR-LF like process.stdout.write("a")
;;;   while being processed
(defun nodejs-repl--send-string (string)
  "Send string to Node.js process and return the output."
  (with-temp-buffer
    (let* ((proc (get-process nodejs-repl-process-name))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (unwind-protect
          (progn
            (set-process-buffer proc (current-buffer))
            (set-process-filter proc 'nodejs-repl--insert-and-update-status)
            (set-marker (process-mark proc) (point-min))
            (process-send-string proc string)
            (nodejs-repl--wait-for-process proc string 0.01))
        (set-process-buffer proc orig-buf)
        (set-process-filter proc orig-filter)
        (set-marker (process-mark proc) orig-marker orig-buf))
      (buffer-string))))

(defun nodejs-repl--wait-for-process (proc string interval)
  "Wait for Node.js process to output all results."
  (process-put proc 'last-line "")
  (process-put proc 'running-p t)
  ;; trim trailing whitespaces
  (setq string (replace-regexp-in-string "[ \t\r\n]*\\'" "" string))
  ;; TODO: write unit test for the case that the process returns 'foo' when string is 'foo\t'
  (while (or (process-get proc 'running-p)
             (not
              (let ((last-line (process-get proc 'last-line)))
                (or (string-match-p nodejs-repl-prompt-re last-line)
                    (string-match-p "^\x1b[[0-9]+D$" last-line)  ; for Node.js 0.8
                    (string= last-line string)))))
    (process-put proc 'running-p nil)
    (accept-process-output proc interval)))

(defun nodejs-repl--insert-and-update-status (proc string)
  "Insert the output string and update the process status (properties)
when receive the output string"
  (process-put proc 'running-p t)
  (with-current-buffer (process-buffer proc)
    (insert string)
    (goto-char (point-max))
    (process-put proc 'last-line (buffer-substring (point-at-bol) (point)))))

(defun nodejs-repl--get-candidates-from-process (token)
  "Get completion candidates sending TAB to Node.js process."
  (let ((ret (nodejs-repl--send-string (concat token "\t")))
         candidates)
    (nodejs-repl-clear-line)
    (when (not (equal ret token))
      (if (string-match-p "\n" ret)
          (progn
            ;; remove extra substrings
            (setq ret (replace-regexp-in-string "\r" "" ret))
            ;; remove LF
            (setq ret (replace-regexp-in-string "\n\\{2,\\}" "\n" ret))
            ;; trim trailing whitespaces
            (setq ret (replace-regexp-in-string "[ \t\r\n]*\\'" "" ret))
            ;; don't split by whitespaces because the prompt may has whitespaces!!
            (setq candidates (split-string ret "\n"))
            ;; remove the first element (input) and the last element (prompt)
            (setq candidates (reverse (cdr (reverse (cdr candidates)))))
            ;; split by whitespaces
            ;; '("encodeURI     encodeURIComponent") -> '("encodeURI" "encodeURIComponent")
            (setq candidates (split-string (mapconcat 'identity candidates " ") "[ \t\r\n]+"))
)
          (setq ret (replace-regexp-in-string nodejs-repl-extra-espace-sequence-re "" ret))
          (let ((candidate-token (nodejs-repl--get-last-token ret)))
            (setq candidates (if (equal candidate-token token)
                                 nil
                               (list candidate-token))))))
    candidates))

(defun nodejs-repl--get-or-create-process ()
  (let ((proc (get-process nodejs-repl-process-name)))
    (unless (processp proc)
      (save-excursion (nodejs-repl))
      (setq proc (get-process nodejs-repl-process-name)))
    proc))

(defun nodejs-repl--filter-escape-sequnces (string)
  "Filter extra escape sequences from output."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; Remove ansi escape sequences used in readline.js
      (while (re-search-forward nodejs-repl-extra-espace-sequence-re end t)
        (replace-match "")))))

(defun nodejs-repl--clear-cache (string)
  "Clear caches when outputting the result."
  (setq nodejs-repl-cache-token "")
  (setq nodejs-repl-cache-candidates ()))

(defun nodejs-repl--remove-duplicated-prompt (string)
  ;; `.load` command of Node.js repl outputs a duplicated prompt
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      (when (re-search-forward (concat nodejs-repl-prompt nodejs-repl-prompt) end t)
        (replace-match nodejs-repl-prompt)))))


;;;--------------------------
;;; Public functions
;;;--------------------------
(defun nodejs-repl-quit-or-cancel ()
  "Send ^C to Node.js process."
  (interactive)
  (process-send-string (get-process nodejs-repl-process-name) "\x03"))

(defun nodejs-repl-clear-line ()
  "Send ^U to Node.js process."
  (nodejs-repl--send-string "\x15"))

(defun nodejs-repl-send-region (start end)
  "Send the current region to the `nodejs-repl-process'"
  (interactive "r")
  (let ((proc (nodejs-repl--get-or-create-process)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")))

(defun nodejs-repl-send-buffer ()
  "Send the current buffer to the `nodejs-repl-process'"
  (interactive)
  (nodejs-repl-send-region (point-min) (point-max)))

(defun nodejs-repl-load-file (file)
  "Load the file to the `nodejs-repl-process'"
  (interactive (list (read-file-name "Load file: " nil nil 'lambda)))
  (let ((proc (nodejs-repl--get-or-create-process)))
    (comint-send-string proc (format ".load %s" file))))

(defun nodejs-repl-send-last-sexp ()
  "Send the expression before point to the `nodejs-repl-process'"
  (interactive)
  (nodejs-repl-send-region (save-excursion (backward-sexp)
                             (point))
                           (point)))

(defun nodejs-repl-switch-to-repl ()
  "If there is a `nodejs-repl-process' running switch to it,
otherwise spawn one."
  (interactive)
  (switch-to-buffer-other-window
   (process-buffer (nodejs-repl--get-or-create-process))))

(defun nodejs-repl-execute (command &optional buf)
  "Execute a command and output the result to the temporary buffer."
  (let ((ret (nodejs-repl--send-string (concat command "\n"))))
    (with-current-buffer (get-buffer-create nodejs-repl-temp-buffer-name)
      (erase-buffer)
      (setq ret (replace-regexp-in-string nodejs-repl-ansi-color-sequence-re "" ret))
      ;; delete inputs
      (setq ret (replace-regexp-in-string "\\(\\w\\|\\W\\)+\r\r\n" "" ret))
      (setq ret (replace-regexp-in-string "\r" "" ret))
      (insert ret)
      ;; delete last line (prompt)
      (goto-char (point-max))
      (delete-region (point-at-bol) (point)))))

(defun nodejs-repl-complete-from-process ()
  "Dynamically complete tokens at the point."
  (when (comint-after-pmark-p)
    (let* ((input (buffer-substring (comint-line-beginning-position) (point)))
           require-arg
           token
           candidates
           ret)
      (if (nodejs-repl--in-string-p)
          (progn
            (setq require-arg (nodejs-repl--extract-require-argument input))
            (if (and require-arg
                     (or (= (length require-arg) 1)
                         (not (string-match-p "[./]" (substring require-arg 1 2)))))
                (setq token (concat "require(" require-arg))
              (setq ret (comint-dynamic-complete-as-filename))))
        (setq token (nodejs-repl--get-last-token input)))
      (when token
        (setq candidates (nodejs-repl-get-candidates token))
        ;; TODO: write unit test
        (setq token (replace-regexp-in-string "^require(['\"]" "" token))
        (setq ret (comint-dynamic-simple-complete token candidates)))
      (if (eq ret 'sole)
          (delete-char -1))
      ret)))

(defun nodejs-repl-get-candidates (token)
  "Get completion candidates."
  (let (candidates)
    (if (and (not (equal nodejs-repl-cache-token ""))
             (string-match-p (concat "^" nodejs-repl-cache-token) token)
             (not (string-match-p (concat "^" nodejs-repl-cache-token ".*?[.(/'\"]") token)))
        (setq candidates nodejs-repl-cache-candidates)
      (if (equal token "require(")  ; a bug occurs when press TAB after "require(" in node 0.6
          (setq candidates nil)
        (setq candidates (nodejs-repl--get-candidates-from-process token)))
      (setq nodejs-repl-cache-token token)
      (setq nodejs-repl-cache-candidates candidates))
    candidates))


(define-derived-mode nodejs-repl-mode comint-mode "Node.js REPL"
  "Major mode for Node.js REPL"
  :syntax-table nodejs-repl-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(nil nil t))
  (add-hook 'comint-output-filter-functions 'nodejs-repl--remove-duplicated-prompt nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-repl--filter-escape-sequnces nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-repl--clear-cache nil t)
  (setq comint-input-ignoredups nodejs-repl-input-ignoredups)
  (setq comint-process-echoes nodejs-repl-process-echoes)
  ;; delq seems to change global variables if called this phase
  (set (make-local-variable 'comint-dynamic-complete-functions)
       (delete 'comint-dynamic-complete-filename comint-dynamic-complete-functions))
  (add-hook 'comint-dynamic-complete-functions 'nodejs-repl-complete-from-process nil t)
  (ansi-color-for-comint-mode-on))

;;;###autoload
(defun nodejs-repl ()
  "Run Node.js REPL."
  (interactive)
  (setq nodejs-repl-prompt-re
        (format nodejs-repl-prompt-re-format nodejs-repl-prompt nodejs-repl-prompt))
  (let* ((repl-mode (or (getenv "NODE_REPL_MODE") "magic"))
         (nodejs-repl-code (format nodejs-repl-code-format
                                   (window-width) nodejs-repl-prompt repl-mode )))
    (switch-to-buffer-other-window
     (apply 'make-comint nodejs-repl-process-name nodejs-repl-command nil
            `(,@nodejs-repl-arguments "-e" ,nodejs-repl-code)))
    (nodejs-repl-mode)))

(provide 'nodejs-repl)
;;; nodejs-repl.el ends here
