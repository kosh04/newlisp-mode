;;; -*- encoding: utf-8 -*-
;;;
;;; init.lsp --- newLISP initialization file
;;;

(let ((e (env "NEWLISPDIR")))
  (when (and e (not (directory? e)))
    (println "warning: directory " e " not found.")))

(constant 'stdin 0 'stdout 1 'stderr 2)

(define (find-symbol str (cxt (context)))
  (sym str cxt nil))

;; (protected? 'define) => true
;; (primitive? 'define) => nil
(define (builtin? symbol)
  (or (primitive? (eval symbol))
      (starts-with (string symbol) "$") ; $0 ... $15
      ;; or more symbols
      (member symbol '(nil true ostype MAIN Tree Class @))
      ))
;; (define builtin? protected?)

(define (user-symbols)
  (filter (lambda (s) (not (builtin? s)))
          (symbols)))

(define (apropos str (do-print nil))
  "Return symbols that matches the regexp."
  (let (acc)
    (dolist (symbol (symbols))
      (if (find str (string symbol) 1)
          (push symbol acc -1)))
    (if (and acc do-print)
        (silent (dolist (i acc)
                  (println i)))
        acc)))

(define top-level reset)
(define restart reset)

(define (utf8?)
  "Non-nil means newLISP is UTF-8 eoncoding are supported."
  (primitive? MAIN:utf8))

(define (newlisp-version)
  "Return newLISP version as integer."
  (sys-info -2))

(define (getpid) (sys-info -3))        ; Return the Process ID of newLISP.
(define (getppid) (sys-info -4))
(define newlisp-pid getpid)
;; (import "libc.so.6" "getpid")
;; こちらは[parent]newlisp->[child]newlispではなくbash(emacs)->newlisp
;; (import "libc.so.6" "getppid")

;; see newlisp.h
(define COMPARE_TYPE_MASK 0x000F)
(define Cell:types
  '("bool" "bool" "integer" "float" "string"
    "symbol" "context" "primitive" "cdecl" "stdcall"
    "quote" "list" "lambda" "macro" "array"))
(define (type-of x)
  (Cell:types (& COMPARE_TYPE_MASK ((dump x) 1))))

(define (load-guiserver)
  (silent
    (print "loading guiserver...")
    (load (real-path (append (env "NEWLISPDIR") "/guiserver.lsp")))
    (gs:init)
    (print "done.")))

(define (load-init)
  (load (real-path (string (env "NEWLISPDIR") "/init.lsp"))))

(define declare (lambda-macro () nil))

(define (xml-parse-file file parse-dtd parse-ns)
  (declare (ignore parse-dtd parse-dtd))
  (let ((tags (xml-type-tags)))
    (local (e)
      (xml-type-tags nil 'cdata '!-- nil)
      (if (catch (xml-parse (read-file file) (+ 1 2 8)) 'e)
          (begin (apply xml-type-tags tags) e)
          (begin (apply xml-type-tags tags) (throw-error e))))))

;; (define (xml-parse-file file parse-dtd parse-ns)
;;   (let ((tags (xml-type-tags)))
;;     (unwind-protect
;;          (xml-parse (read-file file) (+ 1 2 8))
;;       (apply xml-type-tags tags))))

(setq default-xml-type-tags (xml-type-tags))
;; ファイルから読み込むと効かない？
;; (xml-type-tags nil 'cdata '!-- nil)

; (constant (global 'cdr) rest)   ; 全ての名前空間で使えるように
(define-macro (define-cl-function)
  (constant (global (args 0)) (eval (args 1))))
(define (null x) (not (true? x)))
;; (constant (global 't) true)
;; (define car first)
(define (car seq)
  (if (member seq '(nil ())) nil (first seq)))
(define cdr rest)
(define defconstant
  (lambda-macro ()
    (constant (args 0) (eval (args 1)))))
(define export global)
(define progn begin)
(define (funcall f) (apply f (args)))
(define let* letn)
(define lexical-let letex)              ; from Emacs cl-package
(define intern sym)                     ; or make-symbol
(define symbol-name name)
(define char-code char)                 ; (char "A") => 65
(define code-char char)                 ; (char 65)  => "A"
(define rplaca                          ; (rplaca x y)
  (lambda-macro ()
    (setf (first (eval (args 0))) (eval (args 1)))
    (eval (args 0))))
(define rotatef swap)
(define complement
  (lambda-macro ()
    (letex ((f (args 0)))
      (lambda ()
        (not (apply f (args)))))))
(define identity
  (lambda-macro ()
    (eval (args 0))))

(define read-from-string read-expr)

;; Simple LOOP
(define-macro (loop)
  (let ((return throw))
    (catch (while true
             (map eval (args))))))

(define printf format)

;;; @@filesystem, pathname
(define (merge-pathnames pathname (defaults "."))
  (real-path (cond ((file? pathname) pathname)
                   ((starts-with pathname "~/")
                    (append (env "HOME") (1 pathname)))
                   ((regex "^[\\|/]" pathname) pathname)
                   (true (append defaults "/" pathname)))))
(define (user-homedir-pathname) (real-path))
(define (pwd) (real-path))
(define (namestring pathname) (merge-pathnames pathname))
(define set-default-directory change-dir)
;; (define cd change-dir)
(define (cd path)
  (change-dir (or path (env "HOME") "/")))
(define cat read-file)
(define (file-exist-p pathname)
  (or (file? pathname)
      (directory? pathname)))
(define (probe-file pathname)
  (and (file? pathname)
       (real-path pathname)))

(define (file-length pathname)
  "Retun PATHNAMEs file size as byte."
  (nth 0 (file-info pathname)))

(define (getenv variable) (env variable))
(define (setenv variable value) (env variable value))
;; (define getenv env)
;; (define setenv env)

;;; @@number
(constant 'most-positive-fixnum 0x7fffffffffffffff)
(constant 'most-negative-fixnum 0x8000000000000000)
(defconstant pi (mul (atan 1) 4))       ; 3.141592654 (mul (acos 0) 2)
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
;; 全ての数が異なればtrue.
(define (/= number)
  "true if NUMBER and rest numbers are different all. otherwise nil."
  (for-all (lambda (x) (not (= x number))) (args)))
;; (/= 1 2 3 1)                            ; nil
;; (!= 1 2 3 1)                            ; true ?

;; treat integer operators (+-*/) as float operators (add sub mul div).
;; (constant '+ add '- sub '* mul '/ div)

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
;; (define (remove-if f seq) (filter (lambda (x) (not (f x))) seq))
(define remove-if clean)
(define remove-if-not filter)
(define common-lisp:delete              ; 破壊的な意味で
  (lambda-macro ()
    (if (string? (eval (args 1)))
        (replace (eval (args 0)) (eval (args 1)) "")
        (replace (eval (args 0)) (eval (args 1))))))
(define (mapcar f lst)
  "syntax: (mapcar function list &rest more-lists)"
  (letn ((lists (cons lst (args)))
         (minlength (apply min (map length lists))))
    (apply map (cons f (map (lambda (x)
                              (slice x 0 minlength))
                            lists)))))
;; (mapcar list '(1 2 3 4) '(10 nil 30) '(100 200 300 400 500 600))
;; => ((1 10 100) (2 nil 200) (3 30 300))
;; (map list '(1 2 3 4) '(10 nil 30) '(100 200 300 400 500 600))
;; => ((1 10 100) (2 nil 200) (3 30 300) (4 nil 400))

;;; @@sequence, regexp
(define split-string parse)
(define concat string)
(define copy-seq copy)
(define string-upcase upper-case)
(define string-downcase lower-case)
(define string-capitalize title-case)
(define compile-regexp regex-comp)

;; (subseq nil 0) => nil [CL]
(define (subseq seq start end)
  (cond (end (slice seq start (- end start)))
        (true (slice seq start))))

(define (string-match regexp str (start 0) end)
  (regex regexp (subseq str start end)))

(define (substitute-string str pattern replacement)
  (replace pattern str replacement))

(define (regex-quote regexp (extended nil))
  (let (acc)
    (dolist (x (explode regexp))
      (if (member x '("$" "^" "." "*" "[" "]" "\\" "+" "?"))
          (push "\\" acc -1))
      (push x acc -1))
    (apply string acc)))
(define regexp-quote regex-quote)

(define (string-equal string1 string2)
  "Compare two strings ignore case."
  (let ((PCRE_CASELESS 1))
    (list? (regex (string "^" (regex-quote string1) "$")
                  string2
                  PCRE_CASELESS))))

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

(define (elt seq idx)
  (cond ((string? seq) (char seq idx))
        (true (seq idx))))

;;; @@error
(define error throw-error)

;; (context 'ignore-errors)
;; (define-macro (ignore-errors:ignore-errors)
;;   (letex ((body (cons 'begin (args))))
;;     (local (result)
;;       (if (catch body 'result) result nil))))
;; (context MAIN)

(define (prin1-to-string obj)
  (cond ((string? obj) (format"\"%s\"" (replace "\\" obj "\\\\")))
        ("else" (string obj))))

(define-macro (ignore-errors)
  (eval-string (prin1-to-string (args 0)) (context) nil))

(context 'unwind-protect)
(define-macro (unwind-protect:unwind-protect)
  "syntax: (unwind-protext protected-form cleanup-form*)"
  (letex ((body (first (args)))
          (cleanup-form* (cons 'begin (rest (args)))))
    (local (*result*)
      (if (catch body '*result*)
          (begin cleanup-form* *result*)
          (begin cleanup-form* (print "\n" *result* "\n") nil))))) ; ? throw-error
(context MAIN)

(define (pathname? str)
  (or (file? str)
      (directory? str)))

(define (curl url) (silent) (print (get-url url)))
(define (curl--head url) (print (get-url url "header")) (silent))
(define curl-I curl--head)
;; (curl--head "http://www.newlisp.org/")

(define *html-manual*
  (or (exists file?
              (list (string (env "NEWLISPDIR") "/newlisp_manual.html")
                    "/usr/share/doc/newlisp/newlisp_manual.html"))
      "http://www.newlisp.org/newlisp_manual.html"))

(define (arglist fname)
  (let ((def (eval fname)))
    (cond ((primitive? def)
           (setq fname (name fname))
           (if (find fname "|+*-")
               (push "\\" fname))       ; ex: "*" => "\\*"
           ;; 置換の順番間違えると s/&lt;/&amp;lt; になるので注意 (`&' は最初に置換)
           (replace "&" fname "&amp;")
           (replace "<" fname "&lt;")
           (replace ">" fname "&gt;")
           ;; 複数行だと見つからないな(xml-type-tags)
           (let ((html (join (find-all (format {<b>(syntax: \(%s[\) ].*?)</b>} fname)
                                       (read-file *html-manual*))
                             "\n")))
             (replace "<.*?>" html "" 0)
             (replace "&lt;" html "<")
             (replace "&gt;" html ">")
             (replace "&amp;" html "&")
             (println html)
             ;; 見つかった？
             (not (empty? html))))
          ((or (lambda? def)
               (macro? def))
           ;; ユーザ定義の関数、マクロ
           ;; (args)が使われていて、引数が少ない可能性もあるので注意。特にマクロ
           (cons fname (first def))))))

;; (setq
;;  PCRE_CASELESS        1
;;  PCRE_MULTILINE       2
;;  PCRE_DOTALL          4
;;  PCRE_EXTENDED        8
;;  PCRE_ANCHORED       16
;;  PCRE_DOLLAR_ENDONLY 32
;;  PCRE_EXTRA          64
;;  PCRE_NOTBOL        128
;;  PCRE_NOTEOL        256
;;  PCRE_UNGREEDY      512
;;  PCRE_NOTEMPTY     1024
;;  PCRE_UTF8         2048
;;  REPLACE_ONCE    0x8000
;;  PRECOMPILED    0x10000
;;  )

(define (shell-command-to-string str-process)
  (join (exec str-process) "\n"))

(define nslookup net-lookup)

(define (getopt optstring (has-arg nil))
  "オプション引数の解析."
  (let ((pos (find optstring $main-args)))
    (if (and pos has-arg)
        (main-args (+ pos 1))
        (integer? pos))))


(when (= ostype "Win32")

  (import "user32.dll" "MessageBoxA")
  (define (message-box text (title "newLISP"))
    (let ((MB_OK 0))
      (MessageBoxA 0 text title MB_OK 1)))

  (import "kernel32.dll" "GetShortPathNameA")
  (define (get-short-path-name pathname)
    (unless (file-exist-p pathname)
      (throw-error (format "Pathname not found: %s" pathname)))
    (setq pathname (real-path pathname)) ; フルパスに正規化
    (letn ((len 512) (buf (dup "\000" len)))
      ;; 戻り値を有効活用するならこれ (ただし評価順序を間違えると落ちるので注意)
      ;; (0 (GetShortPathNameA pathname buf len) buf)
      (GetShortPathNameA pathname buf len)
      (trim buf)))
  )                                     ; end of (when (= ostype "Win32")


;; (prompt-event (fn (ctx) (string ctx ":" (real-path) "> ")))

(println "init.lsp loading...done")

(context MAIN)
;;; init.lsp ends here
