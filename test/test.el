(require 'ert-expectations)

(expectations
  (desc "run Node.js REPL")
  (expect nil
    (require 'nodejs-repl)
    ;; TODO: set adjust window width (candidates will change according to the width)
    ;;(adjust-window-trailing-edge (selected-window) (- 100 (window-width)) t)
    (nodejs-repl))

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
  (expect "100000000"  ; waits for finishing commands
          (let ((ret (nodejs-repl--send-string "for (i = 0; i < 100000000; i++) {} console.log(i)\n")))
            (nth 1 (split-string ret "\r\n"))))
  ;; TODO
  ;; (expect "100000000"  ; waits for finishing commands
  ;;   (let ((ret (nodejs-repl-send-string "for (i = 0; i < 20000000; i++) if (!(i % 10000000))process.stdout.write(\"\"+i)\n")))
  ;;           (nth 1 (split-string ret "\r\n"))))

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

  (desc "nodejs-repl-get-candidates")
  (expect '("Error")
    (delete-dups (nodejs-repl-get-candidates "Err")))
  (expect '("Error")
    (delete-dups (nodejs-repl-get-candidates "Erro")))
  (expect "Err"  ; use cache?
    nodejs-repl-cache-token)
  (expect '("Math.max" "Math.min")
    (nodejs-repl-get-candidates "Math.m"))
  ;; FIXME: this test is meaningless if window width is not set
  (expect '("encodeURI" "encodeURIComponent")
    (delete-dups (nodejs-repl-get-candidates "encode")))

  (desc "nodejs-repl-get-candidates for require")
  (expect nil
    (nodejs-repl-get-candidates "foo"))
  (expect '("require")
    (nodejs-repl-get-candidates "requi"))
  (expect t
    (nodejs-repl-get-candidates "require(")  ; update cache
    (> (length (nodejs-repl-get-candidates "require(\"")) 1))
  (expect "require(\""  ; update cache?
    nodejs-repl-cache-token)
  (expect "require(\"npm/"  ; update cache?
    (nodejs-repl-get-candidates "require(\"npm/")
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
  )

