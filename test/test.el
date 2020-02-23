(require 'nodejs-repl)
(require 'ert-expectations)

(nodejs-repl)

(expectations
  (desc "run Node.js REPL")
  (expect nodejs-repl-prompt
    ;; TODO: set adjust window width (candidates will change according to the width)
    ;;(adjust-window-trailing-edge (selected-window) (- 100 (window-width)) t)
    (with-current-buffer (process-buffer (nodejs-repl--get-or-create-process))
      (with-timeout (10 (error "timeout"))
        (while (equal (buffer-string) "")
            (sleep-for 0.1))
        (buffer-string))))

  (desc "nodejs-repl--get-last-token")
  (expect "$._foo0"
    (nodejs-repl--get-last-token " $._foo0"))
  (expect "$._foo0"
    (nodejs-repl--get-last-token " console.log($._foo0"))
  (expect nil
    (nodejs-repl--get-last-token " console.log($._foo0("))

  (desc "nodejs-repl--send-string")
  (expect '(t t t)  ; process properties not changed
    (let* ((proc (get-process nodejs-repl-process-name))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (nodejs-repl--send-string "1 +\n1\n")
      (list
       (eq orig-marker (marker-position (process-mark proc)))
       (eq orig-filter (process-filter proc))
       (eq orig-buf (process-buffer proc)))))
  (expect '("2" "undefined")  ; waits for finishing commands
    (let* ((ret (nodejs-repl--send-string "s = Date.now(); while ((d = (Date.now() - s) / 1000 | 0) < 2); console.log(d)\n"))
           (i-value (nth 1 (split-string ret "\r\n")))
           (return-value (nth 2 (split-string ret "\r\n"))))
      (list
       (ansi-color-filter-apply i-value)
       (ansi-color-filter-apply return-value))))

  (desc "nodejs-repl-send-region")
  (expect t
    (let* ((proc (nodejs-repl--get-or-create-process))
           (buf (process-buffer proc)))
      ;; Emulate the case of https://github.com/abicky/nodejs-repl.el/issues/31
      (comint-send-string proc "1\n")
      (with-current-buffer buf
        (with-timeout (10 (error "timeout"))
          (while (not (string-suffix-p "> 1\n1\n> " (ansi-color-filter-apply (buffer-string))))
            (sleep-for 0.1))))
      (with-current-buffer (process-buffer (nodejs-repl--get-or-create-process))
        (goto-char (point-min)))

      (with-temp-buffer
        (insert "2")
        (nodejs-repl-send-region (point-min) (point-max)))
      (with-current-buffer buf
        (with-timeout (10 (error "timeout"))
          (while (string-suffix-p "> 1\n1\n> " (ansi-color-filter-apply (buffer-string)))
            (sleep-for 0.1)))
        ;; > .editor
        ;; // Entering editor mode (^D to finish, ^C to cancel)
        ;; 2
        ;;
        ;; 2
        ;; >
        (string-suffix-p "2\n\n2\n> " (ansi-color-filter-apply (buffer-string))))))

  (desc "nodejs-repl-execute")
  (expect "2\n"
    (nodejs-repl-execute "1 +\n1")
    (with-current-buffer nodejs-repl-temp-buffer-name
      (buffer-string)))
  (expect 0
    (nodejs-repl-execute "console.log(\"a\\n\\nb\")")
    (with-current-buffer nodejs-repl-temp-buffer-name
      ;; node 0.6.17 outputs "undefined"
      (string-match-p "a\n\nb\n\\(undefined\\)?" (buffer-string))))

  (desc "nodejs-repl--get-completions")
  (expect '("Error")
    (delete-dups (nodejs-repl--get-completions "Err")))
  (expect '("Error")
    (delete-dups (nodejs-repl--get-completions "Erro")))
  (expect "Err"  ; use cache?
    nodejs-repl-cache-token)
  (expect '("Math.max" "Math.min")
    (nodejs-repl--get-completions "Math.m"))
  ;; FIXME: this test is meaningless if window width is not set
  (expect '("encodeURI" "encodeURIComponent")
    (delete-dups (nodejs-repl--get-completions "encode")))

  (desc "nodejs-repl--get-completions for require")
  (expect nil
    (nodejs-repl--get-completions "foo"))
  (expect '("require")
    (nodejs-repl--get-completions "requi"))
  (expect t
    (> (length (let ((nodejs-repl-get-completions-for-require-p t))
                 (nodejs-repl--get-completions ""))) 1))
  (expect "require('"  ; update cache?
    nodejs-repl-cache-token)
  (expect "require('"  ; use cache?
    (let ((nodejs-repl-get-completions-for-require-p t))
      (nodejs-repl--get-completions "f"))
    nodejs-repl-cache-token)
  (expect "require('npm/"  ; update cache?
    (let ((nodejs-repl-get-completions-for-require-p t))
      (nodejs-repl--get-completions "npm/"))
    nodejs-repl-cache-token)

  (desc "nodejs-repl--extract-require-argument")
  (expect '("\"a" "\"a" "\"a" nil nil nil)
    (list
     (nodejs-repl--extract-require-argument "require(\"a")
     (nodejs-repl--extract-require-argument " require(\"a")
     (nodejs-repl--extract-require-argument ";require(\"a")
     (nodejs-repl--extract-require-argument "$require(\"a")
     (nodejs-repl--extract-require-argument ".require(\"a")
     (nodejs-repl--extract-require-argument "require(\"a\"")))

  ;; TODO: send stirng and check the result (but process won't respond)
  (desc "nodejs-repl-prompt")
  (expect nil
    (setq nodejs-repl-prompt "node> ")
    (kill-process nodejs-repl-process-name)
    (nodejs-repl))

  (desc "nodejs-repl--beginning-of-expression")
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; foo\n(bar)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 11
    (with-temp-buffer
      (js-mode)
      (insert "bob; foo;\n(bar)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 7
    (with-temp-buffer
      (js-mode)
      (insert "return(foo)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; function foo(a) { }")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; (function foo(a) { })(1)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; !function foo(a) { }(1)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; void function foo(a) { }(1)")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 18
    (with-temp-buffer
      (js-mode)
      (insert "bob; const foo = (x) => { return () => x }")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 18
    (with-temp-buffer
      (js-mode)
      (insert "bob; const foo = x  => { return x }")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 18
    (with-temp-buffer
      (js-mode)
      (insert "bob; const foo = x=>{ return x }")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 25
    (with-temp-buffer
      (js-mode)
      (insert "bob; const foo = (x) => x ")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; [1,2,3].map(function(number) { return number * 2 })")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 6
    (with-temp-buffer
      (js-mode)
      (insert "bob; [1,2,3] . map(function(number) { return number * 2 })")
      (nodejs-repl--beginning-of-expression)
      ))
  (expect 16
    (with-temp-buffer
      (js-mode)
      (insert "bob; var foo = 1;")
      (nodejs-repl--beginning-of-expression)
      ))
  )
