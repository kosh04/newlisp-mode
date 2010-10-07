# swank-newlisp.lsp -- Swank server for newLISP.    -*- coding:utf-8 -*-
#
# Copyright (C) 2009-2010 KOBAYASHI Shigeru (kosh)
#
# This file is licensed under the terms of the GNU General Public
# License as distributed with newLISP.

;;; Commentary:
;;
;; This is tiny Swank server written by newLISP [1]. It has been
;; tested with newLISP v.10.1.2 on Ubuntu UTF-8.
;;
;; [1] newlISP is a Lisp-like, general-purpose scripting language.

;;; Installation
;;
;; 1. Install newlisp binary (or compile from source).
;;    <http://www.newlisp.org/downloads/>
;;
;; 2. Add something like this to your .emacs:
;;
;; (defun swank-newlisp-init (port-filename coding-system)
;;   (format "%S\n" `(swank:start-server ,port-filename)))

;; (setq slime-protocol-version nil) ; ignore version query (if need)
;; (defvar swank-newlisp-filename "./swank-newlisp.lsp") ; This file
;; (defun slime-newlisp ()
;;   (interactive)
;;   (let ((slime-lisp-implementations
;;          `((newlisp ("newlisp" "-n" ,swank-newlisp-filename)
;;                     :init swank-newlisp-init
;;                     :coding-system utf-8-unix))))
;;     (slime 'newlisp)))
;;
;; 3. Use `M-x slime-newlisp' to start it.
;;
;; 4. If you want to kill swank process,
;;    use `M-x slime-repl-sayoonara' (or `slime-quit-lisp')

;;; ChangeLog:
;;
;; 2010-04-11	newLISPのバージョンアップに対応 (v10.1.12: s/name/term/)
;;              win32にてstdoutの出力を拾えるように修正(add win32-peek)
;; 2009-09-12	初版作成。REPLが動くくらい

;;; Code:

(context MAIN)

(define-macro (defglobal symbol var)
  (set (global symbol) (eval var)))

(defglobal t true)
(defglobal *stdin 0)
(defglobal *stdout 1)
(defglobal *stderr 2)

(context 'swank)                        ; (in-package :swank)


;;;; Common Lisp like functions

(define let* letn)
(define defparameter define)
(define-macro (defvar symbol value doc)
  "Define variable if SYMBOL value is nil."
  (if (nil? (eval symbol))
      (set symbol (eval value)))
  symbol)

;; @syntax (defun fname arglist body...)
(define-macro (defun)
  (set (args 0) (append (lambda ) (cons (args 1) (2 (args))))))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via RPC.
(define defslimefun defun)

(define progn begin)
(define-macro (prog1 form1)
  (let ((result (eval form1)))
    (map eval (args))
    result))
(define-macro (prog2 form1 form2)
  (letex ((_body (cons 'begin (args))))
    (progn
      (eval form1)
      (prog1 (eval form2) _body))))

(define (make-string n (init "\000"))
  (dup init n))

(define-macro (loop)                    ; simple loop
  (let ((return throw))
    (catch (while true (map eval (args))))))

;; @syntax (read-sequence sequence stream)
(define-macro (read-sequence)
  (net-receive (eval (args 1))
               (eval (args 0))
               (string-length (eval (args 0)))))

(define (write-string str (stream *stdout))
  (net-send stream str))

;; ちょっと適当 
(define (listen stream)
  (< 0 (or (peek stream)
           (net-peek stream)
           -1)))

;; @syntax (unwind-protect protected-form cleanup-form*) => result
(define-macro (unwind-protect form)
  (local (result)
    (if (catch (eval form) 'result)
        (begin (map eval (args)) result)
        (begin (map eval (args)) (throw-error result)))))

;; (define read-from-string read-expr)
;; (define-macro (simple-restart restart-designer)
;;   ;;(mapset (restart-name format-control format-arguments) restart-designer
;;   (local (restart-handler)
;;     (cond ((catch 'restart-handler (last (map eval (args))))
;;            restart-handler)
;;           ("else"
;;            (println (apply format (rest restart-designer)))
;;            nil))
;;     ))
;; (def)

(define (find-symbol str (ctx (context)))
  (sym str ctx nil))


;;;; newLISP Utility

(if (primitive? utf8len)
    (define string-length utf8len)
    (define string-length length))

;; (or (primitive? peek) (define peek net-peek))
;; for "Win32"
(unless (primitive? peek)
(define (win32-peek fd)
  (or (net-peek fd)
      (let ((ptr (seek fd)))
        (when ptr
          (- (seek fd -1) (seek fd ptr))))))
(define peek win32-peek)
)

(define (utf8?)
  "Non-nil means newLISP is UTF-8 eoncoding are supported."
  (primitive? MAIN:utf8))

;; to use `sys-error' `last-error' `net-error'
(define (error-number err) (if (list? err) (nth 0 err) -1))
(define (error-text err) (if (list? err) (nth 1 err) "What error?"))

(define (find-context obj)
  "Return context named OBJ. If not found, then return nil."
  (let ((x (cond ((string? obj) (eval-string obj MAIN nil))
                 ((symbol? obj) (eval-string (term obj) MAIN nil))
                 ("else" obj))))
    (if (context? x) x nil)))

(define symbol-name term)
(define context-name prefix)
(define (symbol-context x) (find-context (context-name x)))

;; (define even? (lambda (n) (= (& n 0x01) 0)))


;;;; TCP Server

(define (create-socket host port)
  (or (net-listen port host)
      (throw-error (error-text (net-error)))))
(define (local-port socket) (nth 1 (net-local socket)))
(define (close-socket socket) (net-close socket))
(define (accept-connection socket) (net-accept socket))

(define (getpid) (sys-info -3))

(defvar *coding-system* (if (utf8?) "UTF-8" "Shift_JIS"))

(defvar *loopback-interface* "127.0.0.1") ; "localhost"
(defvar default-server-port 4005)

(defvar *emacs-connection*)

(define (start-server port-file)
  (setup-server default-server-port
                (lambda (port)
                  (announce-server-port port-file port)
                  (simple-announce-function port))))

(define (create-server (port default-server-port))
  (setup-server port simple-announce-function))

(define (setup-server port announce-fn)
  (let ((socket (create-socket *loopback-interface* port)))
    (unwind-protect
         (progn
           (announce-fn (local-port socket))
           (let ((accept (accept-connection socket)))
             (unwind-protect
                  (serve-requests accept)
               (close-socket accept))))
      (close-socket socket))))

(define (serve-requests connection)
  (let ((*emacs-connection* connection))
    (loop
       (dispatch-event (read-from-emacs))) ; dispatch-loop
    ))

;; (define (stop-server port) (close-socket *listener-sockets*))
;; (define (restart-server port dont-close) (stop-server port) (sleep 500) (create-server port dont-close))

;; write-file ?
(define (announce-server-port file port)
  (append-file file (format "%d\n" port)))

(define (simple-announce-function port)
  (log-event ";; Listening on port: %d\n" port))

(define (read-from-emacs)
  (decode-message *emacs-connection*))

(define (decode-message stream)
  (let* ((len (decode-message-length stream))
         (str (make-string len "@"))
         (pos (read-sequence str stream)))
    (if (!= len pos) (log-event ";; Short read: %s\n" str))
    (string-to-rpc str)))

(define (decode-message-length stream)
  (let ((buffer (make-string 6 "@")))
    (read-sequence buffer stream)
    (integer buffer 0 16)))             ; read integer as hex (0x10)

;;; *** enable multibyte char [1]
(when (and (!= ostype "Win32")
           (utf8?))
(define (decode-message stream)
  (let* ((len (decode-message-length stream))
         (str (make-string len "@")))
    (dotimes (i len)
      (setf (str i) (char (read-utf8 stream))))
    (if (!= len (string-length str)) (log-event ";; Short read: %s\n" str))
    ;; (log-event "READ: %s\n" (to-string str))
    (string-to-rpc str)))
)

;;; *** enable multibyte char [2]
;; (define (decode-message stream)
;;   (local (header str)
;;     (net-receive stream header 6)
;;     (net-receive stream str 1024) ; read buffer, but HEADER length ignored
;;     (if (!= (int header 0 0x10) (utf8len str))
;;         (log-event ";; Short read: %s\n" str))
;;     (string-to-rpc str)) )

(define (send-to-emacs object)
  (encode-message object *emacs-connection*))

(define (emacs-write-string str)
  (send-to-emacs (list ":write-string" str)))

(define (emacs-new-package ctx)
  (send-to-emacs (list ":new-package"
                       (context-name ctx)
                       (context-string-for-prompt ctx))))

(define (encode-message message stream)
  (let* ((str (rpc-to-string message))
         (len (string-length str)))
    (log-event "WRITE: %s\n" str)
    (write-string (format "%06x" len) stream)
    (write-string str stream)))

;; Normal RPC  -> (:emacs-rex ...)
;; newLISP RPC -> (":emacs-rex" ...)
(define (dispatch-event event)
  (log-event "DISPATCHING: %s\n" (string event))
  (case (event 0)
    (":emacs-rex"                 ; (:emacs-rex form package thread-id id)
     (apply emacs-rex (rest event)))
    (true
     (letex ((_id (event -1)))
       (log-event "Unhandled event: %s\n" (string event))
       ;; (send-to-emacs '(":write-string" "; Evaluation aborted.\n" ":repl-result"))
       (send-to-emacs '(":return" (":abort") _id))
       ;; (throw-to-toplevel)
       ))))

;; (define (simple-repl)
;;   (loop
;;      (print (format "%s> " (context-name (context))))
;;      (let ((expr (read-expr (read-line))))
;;        (cond ((null? (current-line)) (println "; No value"))
;;              ("else" (println (eval expr)))))))

;;;; Logging

(defvar *log-events* true)
(defvar *log-output* *stderr)

;; @syntax (log-event format-string args+)
(define (log-event)
  (when *log-events*
    (write-buffer *log-output* (apply format (args)))))

;;;; IO to Emacs

;; (string-to-rpc "(:emacs-rex (swank:swank-require :swank-presentations) \"COMMON-LISP-USER\" :repl-thread 3)" )
;; => (":emacs-rex" (swank:swank-require ":swank-presentations") "COMMON-LISP-USER" ":repl-thread" 3)
(define (string-to-rpc str)
  (let ((to-rpc
         (lambda (lst)
           (let ((rpc '()) (kwd? nil))
             (dolist (x lst)
               (cond ((and (symbol? x) (= (symbol-name x) ":"))
                      (setq kwd? true))
                     (kwd?
                      (push (format ":%s" (symbol-name x)) rpc -1) ; x -> ":x"
                      (setq kwd? nil))
                     ((list? x)
                      (push (to-rpc x) rpc -1))
                     ("else"
                      (push x rpc -1))))
             rpc))))
    (to-rpc (read-expr str))))

;; (rpc-to-string '(":emacs-rex" (":return" expr) id))
;; => "(:emacs-rex (:return expr) id)"
(define (rpc-to-string form)
  (let ((create-rpc
         (lambda (x)
           (cond ((and (string? x)
                       ;; (even? $idx)
                       (= (x 0) ":"))
                  (sym x))              ; ":x" -> :x
                 ((and (list? x) (not (lambda? x)) (not (macro? x)))
                  (map create-rpc x))
                 ("else" x)))))
    (string (map create-rpc form))))

;; or prin1-to-string
;; (read-expr (to-string STRING)) === STRING
(define (to-string obj)
  (cond ((string? obj) (format "\"%s\"" (replace "\\" obj "\\\\")))
        ("else" (string obj))))

;; (define (pseudo-debug exc)
;;   (let ((level 1))
;;     (send-to-emacs `(":debug" 0 ,level ,@(sldb-info exc 0 20)))
;;     (unwind-protect
;;         (sldb-loop exc)
;;       (send-to-emacs `(":debug-return" 0 ,level nil)))))

;; (define (sldb-loop exc) (while true (dispatch (read-packet @io))))
;; (define (sldb-info exc start _end) )
;; (define (sldb-restarts exc) '(("Quit" "SLIME top-level.")))
;; (define (sldb-backtrace exc start _end) )
;; (define (frame-src-loc exc frame) )

;; SWANK側のプロトコルをでっち上げる  
(define (slime-changelog-date)
  (if (file? slime-changelog)
      (let ((fp (open slime-changelog "r")))
        (unwind-protect
             (first (parse (read-line fp) " "))
          (close fp)))))
(defvar slime-changelog "/usr/share/doc/slime/changelog")
(defvar slime-protocol-version (slime-changelog-date))

(defslimefun connection-info ()
  (letex ((_pid (getpid))
          (_version (sys-info -2))
          (_pv (or (slime-changelog-date) "2009-09-28")))
    '(":pid" _pid
      ":style" nil
      ":package" (":name" "MAIN"
                  ":prompt" "")
      ":lisp-implementation" (":type" "newLISP"
                              ":name" "newlisp"
                              ":version" _version)
      ":machine" (":instance" "" ":type" "" ":version" "")
      ":features" nil
      ":version" _pv
      )))

(defslimefun create-repl (target)
  (set-context "MAIN"))

(defslimefun swank-require (modules filename)
  nil)

(defslimefun default-directory ()
  (real-path))

(defslimefun set-default-directory (dir)
  (unless (change-dir dir)
    (log-event "change-dir: `%s' %s\n" dir (error-text (sys-error))))
  dir)

(defslimefun load-file (filename)
  (load filename))

(defslimefun quit-lisp ()
  (exit))

;;;; Evaluation

(defvar *buffer-context* MAIN)          ; *buffer-package*

(defvar *error-object* (sym "#:ERR"))
(define (error? obj) (= obj *error-object*))

;; FIXME: 'MAIN:foo をコンテキスト表記なしで表示したい
;; 'MAIN:foo -> 'foo
(define (eval-string-for-emacs str)
  (let ((value
         ;; DO EVAL
         (eval-string str *buffer-context* *error-object*)))
    (prog1
        (cond
          ((error? value)
           (log-event "EVAL-STRING ERROR [%s]: %s\n"
                      (string *buffer-context*)
                      (error-text (last-error)))
           (error-text (eval-error)))
          ("else"
           (to-string value)))
      (set-context-maybe str))))

(define (set-context cname)
  "Set context named str-context.
Return the context-name and the string to use in the prompt."
  (let ((ctx (or (find-context cname)
                 (progn
                   (log-event (string "set-context: " cname " not found\n"))
                   *buffer-context*))))
    (setf *buffer-context* ctx)
    (list (term ctx) (context-string-for-prompt ctx))))

(define (set-context-maybe str)
  (when (regex "\\(context '?(\\w+)\\)" str)
    (let (cname (find-context (last (read-expr $0))))
      (set-context cname)
      (emacs-new-package cname))))

(define (context-string-for-prompt ctx)
  (or (context? ctx)
      (setq ctx (find-context ctx)))
  (if (= ctx MAIN) "" (string ctx)))

;; > (+ 1 2 3 nil) => #:ERR
;; > (println (error-text (last-error)))
;; ERR: value expected in function + : nil
;; called from user defined function swank:eval-string-for-emacs
;; called from user defined function swank:listener-eval
;; called from user defined function swank:dispatch-event
;; called from user defined function swank:serve-requests
;; called from user defined function swank:setup-server
;; called from user defined function swank:start-server
;; > (println (error-text (eval-error)))
;; ERR: invalid function
(define (eval-error)
  "Reports the last error without swank-server error."
  (let ((err (last-error)))
    (when err
      (list (error-number err)
            (first (parse (error-text err) "\n"))))))

;; or eval-for-emacs
(define (emacs-rex form ctx thread-id id)
  (local (error-handler)
    (or (catch
            (letex ((_expr (eval form))
                    (_id id))
              (send-to-emacs '(":return" (":ok" _expr) _id)))
          'error-handler)
        (letex ((_id id))               ; error occurred at (EVAL FORM)
          (log-event "ABORT: %s\n" error-handler)
          ;; (send-to-emacs '(":write-string" "; Evaluation aborted.\n" ":repl-result")) ; 重複っぽい 
          (send-to-emacs '(":return" (":abort") _id)))
        )))

(defslimefun interactive-eval (str)
  (eval-region str))

(defslimefun interactive-eval-region (str)
  (interactive-eval str))

(define (eval-region str)
  (with-output-to-repl
      (track-context
       (lambda ()
         (eval-string-for-emacs str)))))

(map set '(repl-input repl-output) (pipe))

(define-macro (with-output-to-repl)
  (letex ((_body (cons 'begin (args))))  
    (let ((src (device)))
      (device repl-output)           ; printなどの出力先を擬似的に変更する
      (unwind-protect
           (prog1
               _body
             (write-repl-output repl-input))
        (device src)))))

(define (write-repl-output stream)
  (let ((buf ""))
    (while (listen stream)
      (write buf (char (read-utf8 stream))))
    (unless (= buf "")
      (emacs-write-string buf))))

(define (track-context func)
  (let ((old *buffer-context*))
    (unwind-protect
         (func)
      (if (!= old *buffer-context*)
          (emacs-new-package *buffer-context*)))))

;; C-u C-x C-e
(defslimefun eval-and-grab-output (str)
  (listener-eval str))

(defslimefun throw-to-toplevel ()
  (throw 'swank-toplevel))

;;;; Listener eval

(defslimefun listener-eval (str)
  (letex ((_value (eval-region str)))
    '(":values" _value)))

(defslimefun buffer-first-change (filename) nil)

;;;; Compilation

;; (define (swank-compile-string str &key buffer position directory) nil)
(define (swank-compile-file filename load-p external-format) nil)
(define (find-external-format coding-system) :default)
(define (guess-external-format filename) nil)

(defslimefun compile-string-for-emacs (str buffer pos dir)
  (eval-string-for-emacs str)
  '("T" "0.01"))                        ; ?

;; *** This function Removed at 2008-07-16 (see SLIME Changelog)
(defslimefun compiler-notes-for-emacs () nil)

;; snarf-string
;; call-compiler

(defslimefun simple-completions (str package) '(nil nil))
(defslimefun list-all-package-names (str)
  (map name (list-all-contexts)))

(define (list-all-contexts)
  (filter context? (map eval (symbols MAIN))))

(defslimefun set-package (str-context)
  (set-context str-context))

;;;; Streams

(define (make-fn-streams input-fn output-fn) nil)
(define (make-stream-interactive stream) nil)

;;; Arglist

(defslimefun operator-arglist (fname ctx)
  (let* ((*buffer-context* (or (find-context ctx)
                               *buffer-context*))
         (symbol (read-expr fname *buffer-context* nil))
         (alist (arglist symbol)))
    (cond ((list? alist) (to-string (cons symbol alist)))
          ("else" "nil"))))

;; @syntax (raw-specs &key arg-indices print-right-margin print-lines)
(defslimefun arglist-for-echo-area (raw-specs)
  (let ((lst (match '((?) *) raw-specs)))
    (and lst
         (let* ((symbol (read-expr (first lst) *buffer-context* nil))
                (alist (arglist symbol)))
           (and (!= alist nil)
                (to-string (cons symbol alist))))
         )))

;;;; Documentation

(define (arglist fname)
  "Return function arguments list."
  (let ((def (eval fname)))
    (cond ((or (lambda? def) (macro? def))
           (first def))
          ("else" nil))))               ; ":not-available"

;; (load "arglist.lsp")
;; (load "http://github.com/kosh04/nl-modules/raw/master/arglist.lsp")
;; (define swank:arglist MAIN:arglist)

(define (function-name function) nil)
(define (macroexpand-all form) nil)
(define (compiler-macroexpand-1 form &optional e) nil)
(define (compiler-macroexpand form &optional e) nil)
(define (describe-symbol-for-emacs symbol) nil)
(define (describe-definition def-name type) nil)
(define (variable-desc-for-echo-area) nil)

;; (defslimefun find-definitions-for-emacs (str) "./swank-newlisp.lsp")

;;;; Fuzzy Symbol Completion

;; see: contrib/swank-fuzzy.lisp
(defslimefun fuzzy-completions
    (str default-package-name &key limit time-limit-in-msec)
  '(() nil) )

;;;; Macroexpansion

(defslimefun swank-macroexpand-1 (str) nil)

;;;; Debugging

;;;; XREF

(define (who-calls function-name) nil)
(define (who-references variable-name) nil)
(define (who-binds variable-name) nil)
(define (who-sets variable-name) nil)
(define (who-macroexpands macro-name) nil)
(define (who-specializes class-name) nil)
(define (list-callers function-name) nil)
(define (list-callees function-name) nil)

;;;; Profiling

(define (profile fname) nil)
(define (profiled-functions) nil)
(define (unprofile fname) nil)
(define (unprofile-all) nil)
(define (profile-report) nil)
(define (profile-reset) nil)
(define (profile-package package callers-p methods) nil)

;;;; Apropos

(defslimefun apropos-list-for-emacs
  (pattern only-external? case-sensitive? package)
  nil)

;;;; Inspector

;;;; Multithreading

;;;; Auxilary functions

(define (swank:io-speed-test (n 1000) (m 1)) nil)

(define (save-image filename (restart-function nil))
  (save filename))

(define ignore (lambda () nil))

(define cl:defparameter define)
(define cl:compile ignore)
(define cl:foobar ignore)
(define cl:m-v-l ignore)
(define (cl:lisp-implementation-type) "newLISP")
(define (cl:class-name class) nil)
(define cl:aref ignore)
(define cl:loop ignore)

;;  (signal (constant 'SIGINT 2) exit)


(context MAIN)

;;; swank-newlisp.lsp ends here.
