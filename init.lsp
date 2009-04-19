;;; -*- encoding: utf-8 -*-
;;;
;;; init.lsp --- newLISP initialization file
;;;

(let ((e (env "NEWLISPDIR")))
  (unless (directory? e)
    (println (format "warning: directory %s not found." e))))

(define (find-symbol str (cxt (context)))
  (sym str cxt nil))

;; (protected? 'define) => true
;; (primitive? define) => true
(define (builtin? symbol)
  (or (primitive? (eval symbol))
      (starts-with (string symbol) "$") ; $0 ... $15
      ;; or more symbols
      (member symbol '(nil true ostype MAIN Tree Class @))
      ))

(define (user-symbols)
  (filter (lambda (s) (not (builtin? s)))
          (symbols)))

(define (apropos str (do-print? nil))
  "指定した正規表現STRに一致するシンボルを返します. "
  (let (acc)
    (dolist (symbol (symbols))
      (if (find str (string symbol) 1)
          (push symbol acc -1)))
    (if (and acc do-print?)
        (silent (dolist (i acc)
                  (println i)))
        acc)))

(define (utf8?)
  (primitive? MAIN:utf8))

(define (newlisp-version)
  (let ((version (map int (explode (string (sys-info 7))))))
    (format "newLISP v%d.%d.%d on %s"
            (version 0)
            (version 1)
            (+ (version 2) (version 3))
            ostype)))

;; newlisp.h 参照
(define (type-of x)
  (let (types '("bool" "bool" "integer" "float" "string"
                "symbol" "context" "primitive" "cdecl" "stdcall"
                "quote" "list" "lambda" "macro" "array"))
    (types (& 0xf ((dump x) 1)))))

(define (load-guiserver)
  (silent
    (print "loading guiser...")
    (load (merge-pathnames "guiserver.lsp" (env "NEWLISPDIR")))
    (gs:init)
    (print "done.")))

(define (load-init.lsp)
  (load (real-path "init.lsp" (env "NEWLISPDIR"))))

(define (xml-parse-file file parse-dtd parse-ns)
  (xml-parse (read-file file)))

; (global 'progn 't 'null)
; (constant (global 'cdr) rest)   ; 全ての名前空間で使えるように
(define-macro (define-cl-function)
  (constant (global (args 0)) (eval (args 1))))
(define (null x) (not (true? x)))
(define t true)                 ; or (constant 't true)
(define car first)
(define cdr rest)
(define defconstant
  (lambda-macro ()
    (constant (args 0) (eval (args 1)))))
(define export global)
(define progn begin)
(define (funcall f) (apply f (args)))
(define let* letn)
(define lexical-let letex)              ; Emacs cl-package
(define intern sym)                     ; or make-symbol
(define symbol-name name)
(define char-code char)                 ; (char "A") => 65
(define code-char char)                 ; (char 65)  => "A"
(define rplaca                          ; (rplaca x y)
  (lambda-macro ()
    (setf (first (eval (args 0))) (eval (args 1)))
    (eval (args 0))))
;; (subseq nil 0) => nil [CL]
(define rotatef swap)
(define complement
  (lambda-macro ()
    (letex ((f (args 0)))
      (lambda ()
        (not (apply f (args)))))))
(define-macro (identity) (eval (args 0)))

(define read-from-string read-expr)

;;; @@filesystem, pathname
(define (merge-pathnames pathname (defaults "."))
  (real-path (cond ((file? pathname) pathname)
                   ((regex "^[\\|/]" pathname) pathname)
                   (true (append defaults "/" pathname)))))
(define (user-homedir-pathname) (real-path))
(define (pwd) (real-path))
(define (namestring pathname) (real-path pathname))
(define set-default-directory change-dir)
(define cd change-dir)
(define cat read-file)
(define file-exist-p file?)
(define (probe-file pathname)
  (or (file? pathname)
      (real-path pathname)))

(define getenv env)

;;; @@number
(constant 'most-positive-fixnum 0x7fffffffffffffff) ; 63bit?
(constant 'most-negative-fixnum 0x8000000000000000)
(defconstant pi (mul (atan 1) 4))       ; 3.141592654
(define equal =)
(define incf inc)
(define decf dec)
(define (plusp number) (< 0 number)) ; or (> number) , (sgn number nil nil true)
(define (minusp number) (< number 0)) ; or (< number) , (sgn number true nil nil)
(define (ash i cnt) (sgn cnt (>> i (abs cnt)) i (<< i cnt)))
(define logand &)
(define logxor ^)
(define logior |)
(define lognot ~)
(define expt pow)
(define (/= number)
  "全ての数が異なればtrue."
  (for-all (lambda (x) (not (= x number))) (args)))
;; 引数2つしか見てないんじゃないの？
;; (!= 2 3 4 2)                    ; true

;;; @@list
(define intersection intersect)
(define set-difference difference)
(define butlast chop)
(define (nthcdr n lst) (slice lst n))
(define (common-lisp:last lst (n 1))
  ((- n) lst))
(define every for-all)
(define (some f lst)
  (dolist (obj lst (f obj))))
(define position find)
(define find-if exists)
(define remove-duplicates unique)
(define (remove item seq)
  (if (string? seq)
      (replace item seq "")
      (replace item seq)))
(define (remove-if f seq)
  (filter (lambda (x) (not (f x))) seq))
(define remove-if-not filter)
(define common-lisp:delete              ; 破壊的な意味で
  (lambda-macro ()
    (replace (eval (args 0)) (eval (args 1)))))
(define (mapcar f lst)          ; (mapcar function list &rest more-lists)
  (letn ((lists (cons lst (args)))
         (minlength (apply min (map length lists))))
    (apply map (cons f (map (lambda (x)
                              (slice x 0 minlength))
                            lists)))))
;; (mapcar list '(1 2 3 4) '(10 nil 30) '(100 200 300 400 500 600))
;;=> ((1 10 100) (2 nil 200) (3 30 300))

;;; @@sequence, regexp
(define split-string parse)
(define concat string)
(define copy-seq copy)
(define string-upcase upper-case)
(define string-downcase lower-case)
(define string-capitalize title-case)
(define compile-regexp regex-comp)

(define (subseq seq start end)
  (cond (end (slice seq start (- end start)))
        (true (slice seq start))))

(define (string-match regexp str (start 0) end)
  (regex regexp (subseq str start end)))

(define (substitute-string str pattern replacement)
  (replace pattern str replacement))

(define (regex-quote regexp (extended nil)) ; regexp-quote
  (let (acc)
    (dolist (x (explode regexp))
      (if (member x '("$" "^" "." "*" "[" "]" "\\" "+" "?"))
          (push "\\" acc -1))
      (push x acc -1))
    (apply string acc)))

;; 大文字小文字の区別をしない文字列比較
(define (string-equal string1 string2)
  (if (regex (string "^" (regex-quote string1) "$") string2 1) true nil))

(define (string-left-trim char-bag str)
  (if (string? char-bag)
      (setq char-bag (map char (explode char-bag))))
  (catch
      (dostring (c str)
        (unless (member c char-bag)
          (throw (slice str $idx))))))

(define (string-right-trim char-bag str)
  (if (string? char-bag)
      (setq char-bag (map char (explode char-bag))))
  (catch
      (dostring (c (reverse (copy str)))
        (unless (member c char-bag)
          (throw (slice str 0 (- (length str) $idx)))))))

(define (string-trim char-bag str)
  (string-right-trim char-bag (string-left-trim char-bag str)))

;; (define (string-trim char-bag str) (trim str char-bag char-bag))
;; (define (string-left-trim char-bag str) (trim str char-bag ""))
;; (define (string-right-trim char-bag str) (trim str "" char-bag))

(define (trim-space str) (trim str))
(define (trim-whitespace str)
  (string-trim " \t\r\n" str))

(define (elt seq idx)
  (char (seq idx)))

;;; @@error
(define error throw-error)

(context 'ignore-errors)
(define-macro (ignore-errors:ignore-errors)
  (letex ((body (cons 'begin (args))))
    (let (result)
      (if (catch body 'result) result nil))))
(context MAIN)

;; 再度エラーを投げるとユーザ定義のエラーになってしまうな
;; (error-number) の値も変わってしまう
(context 'unwind-protect)
(define-macro (unwind-protect:unwind-protect)
  (letex ((body (args 0))
          (cleanup-form* (cons 'begin (rest (args)))))
    (local (*result*)           ; letと何が違う？
      (if (catch body '*result*)
          (begin cleanup-form* *result*)
          (begin cleanup-form* (throw-error *result*))))))
(context MAIN)

(when (= ostype "Win32")
  (import "user32" "MessageBoxA")
  (define (message-box text (title "newLISP"))
    (let ((MB_OK 0))
      (MessageBoxA 0 text title MB_OK 1)))
  )

(define (one-line str) (replace "[\r|\n]" str " " 0))


;; (prompt-event (fn (ctx) (string ctx ":" (real-path) "> ")))

(context MAIN)
;;; init.lsp ends here
