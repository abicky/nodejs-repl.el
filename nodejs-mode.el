;;; nodejs-mode.el --- Run Node.js REPL and communicate the process

;; Copyright (C) 2012  Takeshi Arabiki

;; Author: Takeshi Arabiki
;; Version: See `nodejs-version'

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
;; Run Node.js REPL and communicate the process.
;; This program is derived from comint-mode and provides the below features.
;;
;;  * TAB completion, same as Node.js REPL
;;  * file name completion in string
;;  * incremental history search
;;
;;
;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add to the following lines to your .emacs:
;;
;;    (require 'nodejs-mode)
;;
;; In order to run Node.js REPL, type M-x nodejs.
;; See also `comint-mode' to check key bindings, typing M-x help f comint-mode.
;;

(require 'cc-mode)
(require 'comint)
(require 'ansi-color)

(defgroup nodejs nil
  "Run Node.js REPL and communicate the process."
  :group 'processes)

(defconst nodejs-version "0.0.1"
  "Node.js mode Version.")

(defcustom nodejs-command "node"
  "Node.js command used in `nodejs-mode'."
  :group 'nodejs
  :type 'string)

(defvar nodejs-process-name "nodejs"
  "process name of Node.js REPL.")

(defvar nodejs-temp-buffer-name "*nodejs-command-output*")

(defvar nodejs-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar nodejs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'comint-dynamic-complete)
    (define-key map (kbd "C-c C-c") 'nodejs-quit-or-cancel)
    map))

(defvar nodejs-input-ignoredups t
  "If non-nil, don't add input matching the last on the input ring.

See also `comint-input-ignoredups'")

(defvar nodejs-process-echoes t
  "If non-nil, Node.js does not echo any input.

See also `comint-process-echoes'")

(defvar nodejs-extra-espace-sequence-re "\\(\x1b\\[[0-9]+[GK]\\)")
(defvar nodejs-ansi-color-sequence-re "\\(\x1b\\[[0-9]+m\\)")
;;; if send string like "a; Ma\t", return a; Math\x1b[1G> a; Math\x1b[0K\x1b[10G
(defvar nodejs-prompt-re "\x1b\\[1G> .*\x1b\\[0K\x1b\\[[0-9]+G$")
;;; not support Unicode characters
(defvar nodejs-require-re
  (concat
   "\\(?:^\\|\\s-\\|[-+*/%&|><!;{}()[]\\|\\]\\)"  ; delimiter
   "require\\s-*(\\s-*"
   "\\("
   "\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*"             ; single quote
   "\\|"
   "'[^'\\]*\\(?:\\\\.[^'\\]*\\)*"                ; double quote
   "\\)"
   "$"))

(defvar nodejs-cache-token "")
(defvar nodejs-cache-candidates ())


;;;--------------------------
;;; Private functions
;;;--------------------------
(defun nodejs--in-string-p (&optional pos)
  "Return non-nil if point is inside string"
  (nth 3 (syntax-ppss pos)))

(defun nodejs--extract-require-argument (string)
  (if (string-match nodejs-require-re string)
      (match-string 1 string)))

(defun nodejs--get-last-token (string)
  "Return the last token in the string."
  (if (string-match "\\([._$]\\|\\w\\)+$" string)
      (match-string 0 string)))

;;; TODO:
;;; * the case that a command is sent while another command is being prossesed
;;; * the case that incomplete commands are sent like "1 +\n"
;;; * support commands which output a string without CR-LF like process.stdout.write("a")
;;;   while being processed
(defun nodejs--send-string (string)
  "Send string to Node.js process and return the output."
  (with-temp-buffer
    (let* ((proc (get-process nodejs-process-name))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (unwind-protect
          (progn
            (set-process-buffer proc (current-buffer))
            (set-process-filter proc 'nodejs--insert-and-update-status)
            (set-marker (process-mark proc) (point-min))
            (process-send-string proc string)
            (nodejs--wait-for-process proc string 0.01))
        (set-process-buffer proc orig-buf)
        (set-process-filter proc orig-filter)
        (set-marker (process-mark proc) orig-marker orig-buf))
      (buffer-string))))

(defun nodejs--wait-for-process (proc string interval)
  "Wait for Node.js process to output all results."
  (process-put proc 'last-line "")
  (process-put proc 'running-p t)
  ;; TODO: write unit test for the case that the process returns 'foo' when string is 'foo\t'
  (while (or (process-get proc 'running-p)
             (string-match-p "^require\\s-*(\\s-*['\"]" (process-get proc 'last-line))
             (not
              (let ((last-line (process-get proc 'last-line)))
                (or (string-match-p nodejs-prompt-re last-line)
                    (string-match-p (concat "^" last-line "\\s-$") string)))))
    (process-put proc 'running-p nil)
    (accept-process-output proc interval)))

(defun nodejs--insert-and-update-status (proc string)
  "Insert the output string and update the process status (properties)
when receive the output string"
  (process-put proc 'running-p t)
  (with-current-buffer (process-buffer proc)
    (insert string)
    (goto-char (point-max))
    (process-put proc 'last-line (buffer-substring (point-at-bol) (point)))))

(defun nodejs--get-candidates-from-process (token)
  "Get copmletion candidates sending TAB to Node.js process."
  (let ((ret (nodejs--send-string (concat token "\t")))
         candidates)
    (nodejs-clear-line)
    (when (not (equal ret token))
      (if (string-match-p "\n" ret)
          (progn
            ;; remove extra substrings
            (setq ret (replace-regexp-in-string "\r" "" ret))
            (setq ret (replace-regexp-in-string "\n\\{2,\\}" "\n" ret))

            (setq candidates (split-string (replace-regexp-in-string "\r" "" ret) "\n"))
            ;; remove the first element (input) and the last element (prompt)
            (setq candidates (reverse (cdr (reverse (cdr candidates))))))
        (setq ret (replace-regexp-in-string nodejs-extra-espace-sequence-re "" ret))
        (setq candidates (list (nodejs--get-last-token ret)))))
    candidates))


;;;--------------------------
;;; Public functions
;;;--------------------------
(defun nodejs-quit-or-cancel ()
  "Send ^C to Node.js process."
  (interactive)
  (process-send-string (get-process "node") "\x03"))

(defun nodejs-clear-line ()
  "Send ^A^K to Node.js process."
  (nodejs--send-string "\x01")
  (nodejs--send-string "\x0b"))

(defun nodejs-execute (command &optional buf)
  "Execute a command and output the result to the temporary buffer."
  (let ((ret (nodejs--send-string (concat command "\n"))))
    (with-current-buffer (get-buffer-create nodejs-temp-buffer-name)
      (erase-buffer)
      (setq ret (replace-regexp-in-string nodejs-ansi-color-sequence-re "" ret))
      ;; delete inputs
      (setq ret (replace-regexp-in-string "\\(\\w\\|\\W\\)+\r\r\n" "" ret))
      (setq ret (replace-regexp-in-string "\r" "" ret))
      (insert ret)
      ;; delete last line (prompt)
      (goto-char (point-max))
      (delete-region (point-at-bol) (point)))))

(defun nodejs-complete-from-process ()
  "Dynamically complete tokens at the point."
  (when (comint-after-pmark-p)
    (let* ((input (buffer-substring (comint-line-beginning-position) (point)))
           require-arg
           token
           candidates
           ret)
      (if (nodejs--in-string-p)
          (progn
            (setq require-arg (nodejs--extract-require-argument input))
            (if (and require-arg
                     (or (= (length require-arg) 1)
                         (not (string-match-p "[./]" (substring require-arg 1 2)))))
                (setq token (concat "require(" require-arg))
              (setq ret (comint-dynamic-complete-as-filename))))
        (setq token (nodejs--get-last-token input)))
      (when token
        (setq candidates (nodejs-get-candidates token))
        ;; TODO: write unit test
        (setq token (replace-regexp-in-string "^require(['\"]" "" token))
        (setq ret (comint-dynamic-simple-complete token candidates)))
      (if (eq ret 'sole)
          (delete-char -1))
      ret)))

(defun nodejs-get-candidates (token)
  "Get copmletion candidates."
  (let (candidates)
    (if (and (not (equal nodejs-cache-token ""))
             (string-match-p (concat "^" nodejs-cache-token) token)
             (not (string-match-p (concat "^" nodejs-cache-token ".*?[.(/'\"]") token)))
        (setq candidates nodejs-cache-candidates)
      (if (equal token "require(")  ; a bug occurs when press TAB after "require(" in node 0.6
          (setq candidates nil)
        (setq candidates (nodejs--get-candidates-from-process token)))
      (setq nodejs-cache-token token)
      (setq nodejs-cache-candidates candidates))
    candidates))

;;; a function belong to comint-output-filter-functions must have one argument
(defun nodejs-filter-escape-sequnces (string)
  "Filter extra escape sequences from output."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; remove ansi escape sequences used in readline.js
      (while (re-search-forward nodejs-extra-espace-sequence-re end t)
        (replace-match "")))))

;;; a function belong to comint-output-filter-functions must have one argument
(defun nodejs-clear-cache (string)
  "Clear caches when outputting the result."
  (setq nodejs-cache-token "")
  (setq nodejs-cache-candidates ()))


(define-derived-mode nodejs-mode comint-mode "Node.js REPL"
  "Major mode for Node.js REPL"
  :syntax-table nodejs-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(nil nil t))
  (add-hook 'comint-output-filter-functions 'nodejs-filter-escape-sequnces nil t)
  (add-hook 'comint-output-filter-functions 'nodejs-clear-cache nil t)
  (setq comint-input-ignoredups nodejs-input-ignoredups)
  (setq comint-process-echoes nodejs-process-echoes)
  ;; delq seems to change global variables if called this phase
  (set (make-local-variable 'comint-dynamic-complete-functions)
       (delete 'comint-dynamic-complete-filename comint-dynamic-complete-functions))
  (add-hook 'comint-dynamic-complete-functions 'nodejs-complete-from-process nil t)
  (ansi-color-for-comint-mode-on))

(defun nodejs ()
  "Run Node.js REPL."
  (interactive)
  (switch-to-buffer-other-window
   (apply 'make-comint nodejs-process-name nodejs-command nil))
  (nodejs-mode))

(provide 'nodejs-mode)
