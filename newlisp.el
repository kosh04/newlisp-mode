;;; -*- mode:emacs-lisp; coding:utf-8 -*-
;;;
;;; newlisp.el --- newLISP editing mode for Emacs

;;; Time-stamp: <2009-06- 9T07:17:51>

;; Author: Shigeru Kobayashi <shigeru.kb@gmail.com>
;; Version: 0.1a
;; Keywords: language,lisp

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; LISP風軽量スクリプト言語`newLISP'を編集するための簡単なメジャーモードです。
;; 
;; newLISP Home - http://www.newlisp.org/
;; 
;; 最新バージョンはこちらにあります:
;; http://github.com/kosh04/newlisp-files/tree/master

;;; Usage:
;; (require 'newlisp)
;; (push '("\\.lsp$" . newlisp-mode) auto-mode-alist)
;; (newlisp-mode-setup)

;;; ChangeLog:
;; 2009-06-05 version 0.1a
;; - font-lock 若干修正
;; - newlisp-mode-syntax-table 追加
;; 2009-04-19 version 0.1
;; - newlisp-mode, font-lock 追加
;; 2008-12-15 version 0.01
;; - 初版作成 (newlisp-mode)

;;; Known bugs/problems:
;; - 初回起動時の評価が表示されずに溜まることがある(ubuntu)

;;; Todo:
;; - シンボル補完 (etags, complete-symbol, [d]abbrev)
;; - pop-to-buffer は縦分割を好む人もいるかもしれない
;; - elisp の書式チェック (checkdoc)
;; - defcustom
;; - 出力だけでなく入力も*newlisp*バッファに送るべきかもしれない
;; - 全ては気の赴くままに

;;; Code:
(eval-when-compile (require 'cl))
(require 'comint)                       ; comint-send-string
;; (require 'inf-lisp)

(defvar *newlisp-command* "newlisp"
  "newLISP execute binary filename.")

;; (defvar *newlisp-command-option* "")

(defvar *newlisp-process-coding-system* '(utf-8 . utf-8)
  "Cons of coding systems used for process newLISP (input . output).
If you use newLISP version UTF-8 support, Its value is '(utf-8 . utf-8).
Otherwise maybe '(sjis . sjis).")

(defun newlisp-process ()
  (let ((default-process-coding-system *newlisp-process-coding-system*))
    (get-buffer-process
     (make-comint "newlisp" *newlisp-command* nil
                  ;; newlisp側では`~/'をホームディレクトリとして認識しないので
                  ;; emacs側で展開しておく
                  "-C" "-w" (expand-file-name default-directory)))))

(defun newlisp-show-repl (&optional no-focus)
  (interactive "P")
  (let ((obuf (current-buffer)))
    (pop-to-buffer (process-buffer (newlisp-process)))
    (if no-focus (pop-to-buffer obuf))))

(defalias 'run-newlisp 'newlisp-show-repl)

(defun newlisp-eval (str-sexp)
  "Eval newlisp s-expression."
  (interactive "snewLISP eval: ")
  (let ((proc (newlisp-process)))
    ;; (sit-for 0.2)                   ; 同期のやり方がわからないので適当に誤魔化す
    '(with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert str-sexp ?\n)
;;       (set-marker comint-last-input-start (point))
;;       (set-marker comint-last-input-end (point))
;;       (set-marker comint-last-output-start (point))
;;       (set-marker comint-accum-marker nil)
      (set-marker (process-mark proc) (point))
      ;; (goto-char (point-max))
      )
    (labels ((sendln (str)
               (comint-send-string proc (concat str "\n"))))
      (sendln "[cmd]")
      (sendln str-sexp)
      (sendln "[/cmd]"))
    (newlisp-show-repl t)))

(defun newlisp-eval-region (from to)
  (interactive "r")
  (newlisp-eval (buffer-substring-no-properties from to)))

(defun newlisp-eval-last-sexp ()
  (interactive)
  (let ((opoint (point)))
    (unwind-protect
         (newlisp-eval-region (progn
                                ;; 'hoge
                                (unless (looking-at "\\_<")
                                  (backward-sexp))
                                (point))
                              (progn
                                (forward-sexp)
                                (point)))
      (goto-char (max (point) opoint)))))

(defun newlisp-eval-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (newlisp-eval-region (region-beginning) (region-end))))

(defun newlisp-kill-process ()
  (interactive)
  (newlisp-eval "(exit)"))              ; or (kill-process (newlisp-process))

;; eval sync
(defun newlisp-eval-buffer (arg)
  (interactive "P")
  (setq arg (if arg (read-string "newLISP args: ") ""))
  (shell-command (format "%s \"%s\" %s"
                         *newlisp-command*
                         (buffer-file-name)
                         arg)
                 "*newLISP output*"))

;; eval async
(defun newlisp-execute-file (&optional cmd-args)
  (interactive (list (if current-prefix-arg
                         (read-string "execute args: " )
                         "")))
  ;; (setq arg (if arg (read-string "args: ") ""))
  (lexical-let ((outbuf (get-buffer-create "*newLISP output*")))
    (with-current-buffer outbuf (erase-buffer))
    (set-process-sentinel (start-process-shell-command "newlisp" outbuf
                                                       *newlisp-command*
                                                       (buffer-file-name)
                                                       cmd-args)
                          #'(lambda (process event)
                              (cond ((zerop (buffer-size outbuf))
                                     (kill-buffer outbuf)
                                     (message "(no output)"))
                                    (t
                                     (with-current-buffer outbuf
                                       (goto-char (point-min))
                                       (if (< (line-number-at-pos (point-max)) 5)
                                           (message "%s" (replace-regexp-in-string
                                                          "\n+$" "" (buffer-string)))
                                           (pop-to-buffer (process-buffer process))))))))
    ))

;; lisp.el:571
(defun newlisp-complete-symbol (&optional predicate)
  "Perform completion on newLISP symbol preceding point."
  (interactive)
  (error "Undefined"))

(eval-when (compile load eval)
  (defvar *newlisp-primitives*
    ;; newLISP v.10.0.0 on Win32 IPv4 UTF-8
    '("!" "!=" "$" "%" "&" "*" "+" "-" "/" ":" "<" "<<" "<=" "=" ">" ">=" ">>" "NaN?" 
      "^" "abort" "abs" "acos" "acosh" "add" "address" "amb" "and" "append" "append-file" 
      "apply" "args" "array" "array-list" "array?" "asin" "asinh" "assoc" "atan" "atan2" 
      "atanh" "atom?" "base64-dec" "base64-enc" "bayes-query" "bayes-train" "begin" "beta" 
      "betai" "bind" "binomial" "bits" "butlast" "callback" "car" "case" "cat" "catch" 
      "cd" "cdr" "ceil" "change-dir" "char" "char-code" "chop" "clean" "close" "code-char" 
      "command-event" "compile-regexp" "concat" "cond" "cons" "constant" "context" "context?" 
      "copy" "copy-file" "copy-seq" "cos" "cosh" "count" "cpymem" "crc32" "crit-chi2" 
      "crit-z" "current-line" "curry" "date" "date-value" "debug" "dec" "decf" "def-new" 
      "default" "delete" "delete-file" "delete-url" "destroy" 
      "det" "device" "difference" "directory" "directory?" "div" "do-until" "do-while" 
      "doargs" "dolist" "dostring" "dotimes" "dotree" "dump" "dup" "empty?" "encrypt" 
      "ends-with" "env" "equal" "erf" "error" "error-event" "error-number" "error-text" 
      "eval" "eval-string" "every" "exec" "exists" "exit" "exp" "expand" "explode" "export" 
      "expt" "factor" "fft" "file-info" "file?" "filter" "find" "find-all" "find-if" "first" 
      "flat" "float" "float?" "floor" "flt" "for" "for-all" "format" "fv" "gammai" "gammaln" 
      "gcd" "get-char" "get-float" "get-int" "get-long" "get-string" "get-url" "getenv" 
      "global" "global?" "if" "if-not" "ifft" "import" "inc" "incf" "index" "int" "integer" 
      "integer?" "intern" "intersect" "intersection" "invert" "irr" "join" "lambda?" "last" 
      "legal?" "length" "let" "let*" "letex" "letn" "lexical-let" "list" "list?" "load" 
      "local" "log" "logand" "logior" "lognot" "logxor" "lookup" "lower-case" "macro?" 
      "main-args" "make-dir" "map" "mat" "match" "max" "member" "min" "mod" "mul" "multiply" 
      "name" "net-accept" "net-close" "net-connect" "net-error" "net-eval" "net-interface" 
      "net-listen" "net-local" "net-lookup" "net-peek" "net-peer" "net-receive" "net-receive-from" 
      "net-receive-udp" "net-select" "net-send" "net-send-to" "net-send-udp" "net-service" 
      "net-sessions" "new" "nil?" "normal" "not" "now" "nper" "npv" "nth" "null?" "number?" 
      "open" "or" "pack" "parse" "pipe" "pmt" "pop" "pop-assoc" "position" "post-url" 
      "pow" "pretty-print" "primitive?" "print" "println" "prob-chi2" "prob-z" "process" 
      "progn" "prompt-event" "protected?" "push" "put-url" "pv" "quote" "quote?" "rand" 
      "random" "randomize" "read-buffer" "read-char" "read-expr" "read-file" "read-from-string" 
      "read-key" "read-line" "real-path" "ref" "ref-all" "regex" "regex-comp" "remove-dir" 
      "remove-duplicates" "remove-if-not" "rename-file" "replace" "reset" "rest" "reverse" 
      "rotate" "rotatef" "round" "save" "search" "seed" "seek" "select" "semaphore" "sequence" 
      "series" "set" "set-default-directory" "set-difference" "set-locale" "set-ref" "set-ref-all" 
      "setenv" "setf" "setq" "sgn" "share" "signal" "silent" "sin" "sinh" "sleep" "slice" 
      "sort" "source" "spawn" "split-string" "sqrt" "starts-with" "string" "string-capitalize" 
      "string-downcase" "string-upcase" "string?" "sub" "swap" "sym" "symbol-name" "symbol?" 
      "symbols" "sync" "sys-error" "sys-info" "tan" "tanh" "throw" "throw-error" "time" 
      "time-of-day" "timer" "title-case" "trace" "trace-highlight" "transpose" "trim" 
      "true?" "unicode" "unify" "unique" "unless" "unpack" "until" "upper-case" "utf8" 
      "utf8len" "uuid" "when" "while" "write-buffer" "write-char" "write-file" "write-line" 
      "xml-error" "xml-parse" "xml-type-tags" "zero?" "|" "~")
    "newLISP primitive keyword list.")
  )                                     ; eval-when

(defun newlisp-mode-setup ()
  (setq *newlisp-process-coding-system*
        (let ((res (shell-command-to-string
                    (format "%s -n -e \"%s\"" *newlisp-command* '(primitive? MAIN:utf8)))))
          (if (string-match "true" res)
              '(utf-8 . utf-8)
              '(shift_jis . shift_jis)))) ; or 'sjis ?
  (setq *newlisp-primitives*
        (car (read-from-string
              (shell-command-to-string
               (format "%s -n -e \"%s\"" *newlisp-command*
                       '(map name (filter (fn (s) (primitive? (eval s)))
                                          (symbols MAIN))))))))
  t)

(defmacro defindent (operator indentation)
  `(put ',operator 'lisp-indent-function ',indentation))

(defindent define 1)
(defindent fn 1)
(defindent begin 0)
(defindent local 1)
(defindent letex 1)
(defindent for 1)
(defindent lambda-macro 1)
(defindent define-macro 1)
(defindent until 1)
(defindent letn 1)
(defindent dostring 1)

(defvar newlisp-mode-hook nil)
(defvar newlisp-mode-map
  (let ((map (make-sparse-keymap "newlisp")))
    (set-keymap-parent map lisp-mode-shared-map)
    map))
(define-key newlisp-mode-map "\e\C-x" 'newlisp-eval-defun)
(define-key newlisp-mode-map "\C-x\C-e" 'newlisp-eval-last-sexp)
(define-key newlisp-mode-map "\C-c\C-r" 'newlisp-eval-region)
(define-key newlisp-mode-map "\C-c\C-z" 'newlisp-show-repl)
(define-key newlisp-mode-map "\e\t" 'newlisp-complete-symbol) ; ESC TAB
(define-key newlisp-mode-map [f5] 'newlisp-execute-file)
(define-key newlisp-mode-map "\C-m" 'newline-and-indent)

(defvar newlisp-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    ;; SYMBOL
    (modify-syntax-entry ?` "_   " table)
    (modify-syntax-entry ?, "_   " table)
    (modify-syntax-entry ?@ "_   " table)
    (modify-syntax-entry ?| "_   " table)
    (modify-syntax-entry ?\[ "_   " table)
    (modify-syntax-entry ?\] "_   " table)
    ;; STRING (match)
    (modify-syntax-entry ?\{ "(}   " table)
    (modify-syntax-entry ?\} "){   " table)
    ;; COMMENT
    (modify-syntax-entry ?# "<   " table)
    ;; ESCAPE
    ;; ?\\ は通常はエスケープ文字だが、{}で囲まれた文字列内の場合はリテラルになる
    table))

;;;###autoload
(defun newlisp-mode ()
  "Major mode for editing newLISP code to run in Emacs."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'newlisp-mode
        mode-name "newLISP")
  (use-local-map newlisp-mode-map)
  (lisp-mode-variables)
  (set-syntax-table newlisp-mode-syntax-table)
  ;; (setq font-lock-defaults nil)
  ;; (set (make-local-variable 'font-lock-keywords-case-fold-search) nil)
  (run-mode-hooks 'newlisp-mode-hook))

;; $ html2txt $NEWLISPDIR/newlisp_manual.html -o newlisp_manual.txt
(defvar *newlisp-manual-text* "newlisp_manual.txt")

(defun newlisp-manual-from-text (str)
  (interactive
   (list (completing-read "newLISP manual: "
                          #1=*newlisp-primitives* nil t
                          (car (member (thing-at-point 'symbol) #1#)))))
  (let ((obuf (current-buffer)))
    (pop-to-buffer (find-file-noselect *newlisp-manual-text*))
    (toggle-read-only t)
    (let ((opoint (point)))
      (goto-char (point-min))
      (unless (search-forward (concat "*syntax: (" str) nil 'noerror)
        (goto-char opoint)
        (pop-to-buffer obuf)
        (message "Function Not Found: %s" str)))))

(define-key newlisp-mode-map "\C-ch" 'newlisp-manual-from-text)

(defun newlisp-browse-manual ()
  (interactive)
  (browse-url (cond ((file-exists-p #1="/usr/share/doc/newlisp/manual_frame.html")
                     #1#)
                    (:else
                     "http://www.newlisp.org/downloads/manual_frame.html"))))

;; (put 'font-lock-add-keywords 'lisp-indent-function 1)
;; lisp-mode.el:91
(font-lock-add-keywords 'newlisp-mode
  (list
   ;; (list "\\<\\(FIXME\\):" 1 font-lock-warning-face 'prepend)
   (cons (eval-when-compile
           (regexp-opt *newlisp-primitives* 'words))
         font-lock-keyword-face)
   (cons (eval-when-compile
           (regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") 'words))
         font-lock-function-name-face)
   (cons (eval-when-compile
           (regexp-opt '("nil" "true" "ostype"
                         "$args" "$idx" "$it" "$main-args" "$prompt-event"
                         "$" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
                         "$10" "$11" "$12" "$13" "$14" "$15") 'words))
         font-lock-constant-face)
   (cons (eval-when-compile
           (regexp-opt '("Class" "MAIN" "Tree") 'words))
         font-lock-type-face)
   (cons (eval-when-compile
           (regexp-opt '("[text]" "[/text]" "[cmd]" "[/cmd]")))
         font-lock-preprocessor-face)
   (cons (eval-when-compile
           (regexp-opt '("peek" "fork" "wait-pid" "net-ping" "parse-date") 'words))
         font-lock-warning-face)
   )
  )

;; (defun newlisp-make-regexp-opt (&rest strings) (eval-when-compile (regexp-opt strings 'words)))

(provide 'newlisp)

;;; newlisp.el ends here
