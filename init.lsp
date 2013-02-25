;;; init.lsp    -*- encoding: utf-8 -*-

;; TODO:
;; * newLISPコテコテの関数を使うべきか、なるべくLISP準拠にするべきか

;; NOTE:
;; 他のライブラリが再定義する可能性があるので
;; 以下は極力ここでは利用しないこと
;; * 定数
;; * Context

(define (apropos str (do-print true))
  "Return symbols that matches the regexp."
  (let ((acc (find-all str (symbols) $it
                       (lambda (x y)
                         (regex x (name y))))))
    (when (and acc do-print)
      (dolist (item acc)
        (cond
          ((primitive? (eval item))
           (println item "\t" "<primitive>"))
          ((lambda? (eval item))
           (println item "\t" "<lambda>"))
          ((macro? (eval item))
           (println item "\t" "<macro>"))
          ("else"
           (println item)))))
    acc))

(define (use libname)
  (load (or (exists file?
                    (list
                     libname
                     (append (env "NEWLISPDIR") "/modules/" libname)
                     (append (env "HOME") "/share/newlisp/modules/" libname)
                     ))
            (throw-error (list "No such module" libname)))))

(define (load-guiserver (nomsg nil))
  (unless nomsg (print "loading guiserver..."))
  (load (append (env "NEWLISPDIR") "/guiserver.lsp"))
  (gs:init)
  (unless nomsg (print "done."))
  true)

;; @syntax (aif test then [else])
(define-macro (aif)
  "anaphoric if"
  (let (it (eval (args 0)))
    (if it
        (eval (args 1))
        (eval (cons 'begin (2 (args)))))))

(define top-level (lambda () (reset nil)))
(define restart (lambda () (reset true)))

(define (utf8?)
  "Non-nil means newLISP is UTF-8 eoncoding are supported."
  (primitive? MAIN:utf8))

(define (newlisp-version)
  "Return newLISP version as integer."
  (sys-info -2))

(define (getpid) (sys-info -3))        ; Return the Process ID of newLISP.
(define (getppid) (sys-info -4))
;; (import "libc.so.6" "getpid")
;; こっちは[parent-pid]sh or emacs -> [child-pid]newlisp
;; (import "libc.so.6" "getppid")

;; see newlisp.h
(define COMPARE_TYPE_MASK 0x000F)
(define type-of:types
  '("bool" "bool" "integer" "float" "string"
    "symbol" "context" "primitive" "cdecl" "stdcall"
    "quote" "list" "lambda" "macro" "array"))
(define (type-of:type-of x)
  (type-of:types (& COMPARE_TYPE_MASK ((dump x) 1))))

;; simple loop
(define-macro (loop)
  (let ((return throw))
    (catch (while true (map eval (args))))))

;; (define (printf) (print (apply format (args))))
(case ostype
  ("Win32"
   (import "msvcrt.dll" "printf")
   (import "msvcrt.dll" "fflush"))
  ("Linux"
   (import "libc.so.6" "printf")
   (import "libc.so.6" "fflush")))

;; treat integer operators (+-*/) as float operators (add sub mul div).
;(constant '+ add '- sub '* mul '/ div)

(define array->list array-list)

(define (perm seq n)
  "SEQから重複なしでランダムにN個の要素を選択する."
  (let ((len (length seq)))
    (slice (if (empty? seq)
               seq
               (select seq (randomize (sequence 0 (- len 1)))))
           0 (or n len))))
;(perm "newLISP") => "LSwePIn"

(define (error)
  (throw-error (apply format (args))))

(define (errno)
  (nth 0 (sys-error)))

(define (curl url) (print (get-url url)) true)
(define (curl--head url) (print (get-url url "header")) true)
(define curl-I curl--head)
;; (curl--head "http://www.newlisp.org/")

(define nslookup net-lookup)

(define (shell-command-to-string str-process)
  (join (exec str-process) "\n"))

;; destructuring-bind like
;; syntax: (mapset <lambda-list> <expr> <form*>)
(define-macro (mapset)
  (eval (list 'local (args 0)
              (list 'map 'set (list 'quote (args 0)) (args 1))
              (cons 'begin (2 (args))))))
;; (setf (get 'mapset 'lisp-indent-function) 2)

(define (xml-parse-file file xml-option)
  (xml-type-tags nil nil nil nil)       ; or (nil cdata !-- nil)
  (xml-parse (read-file file) (or xml-option (+ 1 2 4))))

(define (%get-string addr (n 100))
  "ADDRを配列のポインタとみなしてNバイト分読み込む."
  (first (unpack (string "s" n) addr)))

;; (expand-env "HOME=$HOME") => "HOME=/home/username"
;; FIXME: "${HOME}"
(define (expand-env str)
  (dolist (e (env))
    (replace (string "$" (e 0)) str (e 1)))
  str)

;; see `man ascii'
(setq escape-char-name
      '("NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT"
        "LF" "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4"
        "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "FSGS" "RS" "US" "SPACE"))


(use "io.lsp")
(use "files.lsp")
(use "regex.lsp")
(use "net.lsp")
(use "argv.lsp")
(use "arglist.lsp")
;(use "legacy.lsp")
;(use "iconv.lsp")
(use "cl.lsp")
(if (= ostype "Win32")
    (use "winapi.lsp"))

;; (prompt-event (fn (ctx) (string ctx ":" (real-path) "> ")))

(unless (getopt "-q")
  (println "init.lsp loading...done")
  (println "current working directory: " (pwd)))

(let ((e (env "NEWLISPDIR")))
  (when (and e (not (directory? e)))
    (println "warning: directory " e " not found.")))

;; 起動時に引数指定でモジュールを読み込む
;; $ newlisp -use FILENAME.lsp
(aif (getopt "-use" true) (use it))

(context MAIN)

;;; init.lsp ends here
