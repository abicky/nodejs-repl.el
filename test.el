(require 'el-expectations)

(expectations
  (desc "run Node.js REPL")
  (expect nil
    (require 'nodejs-mode)
    (nodejs))

  (desc "nodejs--get-last-token")
  (expect "$._foo0"
    (nodejs--get-last-token " $._foo0"))
  (expect "$._foo0"
    (nodejs--get-last-token " console.log($._foo0"))
  (expect nil
    (nodejs--get-last-token " console.log($._foo0("))

  (desc "nodejs--send-string")
  (expect '(t t t)  ; process properties not changed
    (let* ((proc (get-process nodejs-process-name))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (nodejs--send-string "1 +\n1\n")
      (list
       (eq orig-marker (marker-position (process-mark proc)))
       (eq orig-filter (process-filter proc))
       (eq orig-buf (process-buffer proc)))))
  (expect "100000000"  ; waits for finishing commands
          (let ((ret (nodejs--send-string "for (i = 0; i < 100000000; i++) {} console.log(i)\n")))
            (nth 1 (split-string ret "\r\n"))))
  ;; TODO
  ;; (expect "100000000"  ; waits for finishing commands
  ;;   (let ((ret (nodejs-send-string "for (i = 0; i < 20000000; i++) if (!(i % 10000000))process.stdout.write(\"\"+i)\n")))
  ;;           (nth 1 (split-string ret "\r\n"))))

  (desc "nodejs-execute")
  (expect "2\n"
    (nodejs-execute "1 +\n1")
    (with-current-buffer nodejs-temp-buffer-name
      (buffer-string)))
  (expect 0
    (nodejs-execute "console.log(\"a\\n\\nb\")")
    (with-current-buffer nodejs-temp-buffer-name
      ;; node 0.6.17 outputs "undefined"
      (string-match-p "a\n\nb\n\\(undefined\\)?" (buffer-string))))

  (desc "nodejs-get-candidates")
  (expect t
    (let ((ret (nodejs-get-candidates "Ma")))
      (or (equal ret '("Math"))         ; node 0.4.12
          (equal ret '("Math" "Math"))  ; node 0.6.17
          )))
  (expect t
    (let ((ret (nodejs-get-candidates "Mat")))
      (or (equal ret '("Math"))         ; node 0.4.12
          (equal ret '("Math" "Math"))  ; node 0.6.17
          )))
  (expect "Ma"  ; use cache?
    nodejs-cache-token)
  (expect '("Math.max" "Math.min")
    (nodejs-get-candidates "Math.m"))

  (desc "nodejs-get-candidates for require")
  (expect nil
    (nodejs-get-candidates "foo"))
  (expect '("require")
    (nodejs-get-candidates "requi"))
  (expect t
    (nodejs-get-candidates "require(")  ; update cache
    (> (length (nodejs-get-candidates "require(\"")) 1))
  (expect "require(\""  ; update cache?
    nodejs-cache-token)
  (expect "require(\"npm/"  ; update cache?
    (nodejs-get-candidates "require(\"npm/")
    nodejs-cache-token)

  (desc "nodejs--extract-require-argument")
  (expect '("\"a" "\"a" "\"a" nil nil nil)
    (list
     (nodejs--extract-require-argument "require(\"a")
     (nodejs--extract-require-argument " require(\"a")
     (nodejs--extract-require-argument ";require(\"a")
     (nodejs--extract-require-argument "$require(\"a")
     (nodejs--extract-require-argument ".require(\"a")
     (nodejs--extract-require-argument "require(\"a\"")))
  )
