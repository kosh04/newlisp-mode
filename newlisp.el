;;; newlisp.el -- newLISP editing mode for Emacs  -*- coding:utf-8 -*-

;; Copyright (C) 2008,2009 Shigeru Kobayashi

;; Author: Shigeru Kobayashi <shigeru.kb@gmail.com>
;; Version: 0.2
;; Created: 2008-12-15
;; Keywords: language,lisp
;; URL: http://github.com/kosh04/newlisp-files/raw/master/newlisp.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; LISP風軽量スクリプト言語`newLISP'を編集するための簡単なメジャーモードです。
;;
;; newLISP Home - http://www.newlisp.org/
;;
;; このファイルの最新バージョンはこちらにあります:
;;      http://github.com/kosh04/newlisp-files/tree/master

;;; Installation:
;;
;; (require 'newlisp)
;; (push '("\\.lsp$" . newlisp-mode) auto-mode-alist)
;; (newlisp-mode-setup) ; if needed

;;; ChangeLog:
;;
;; 2009-09-30 version 0.2
;; - キーワード補完が出来るように
;; 2009-07-05 version 0.1b
;; - キーワードをnewLISP v10.1.0に追従
;; - rename `*variable*' to `variable' (Emacsの命名規則に従って変数名変更)
;; 2009-06-05 version 0.1a
;; - font-lock 若干修正
;; - newlisp-mode-syntax-table 追加
;; 2009-04-19 version 0.1
;; - newlisp-mode, font-lock 追加
;; 2008-12-15 version 0.01
;; - 初版作成 (newlisp-mode)

;;; Known Bugs:
;;
;; - (newlisp-eval "(eval-string \"((define hex	0xff))\")") => ERR
;; - 初回起動時の評価が出力されずに溜まってしまう場合がある
;; - ２バイト文字を含むパスから起動することができない
;;   e.g. "c:/Documents and Settings/User/デスクトップ/"
;;   これは文字コードの違いが問題: sjis(windowsのパス名),utf-8(newlisp)
;; - newlisp.exeが$PATHにないとshell-command-to-stringを実行できない
;;
;; export PATH="$HOME/bin:$PATH"
;; - emacsのシェルからの起動に必要
;; (or (string-match #1=(expand-file-name "~/bin") #2=(getenv "PATH"))
;;     (setenv "PATH" (concat #1# ":" #2#)))
;; - emacsからの起動に必要
;; (add-to-list 'exec-path "~/bin")

;;; Todo:
;;
;; - シンボル補完 (etags, complete-symbol, [d]abbrev)
;; - pop-to-buffer は縦分割を好む人もいるかもしれない
;; - elisp の書式チェック (checkdoc)
;; - defcustom
;; - 出力だけでなく入力も*newlisp*バッファに送るべきかもしれない
;; - lisp-modeから間借りしている機能は分割するべきかも
;; - 全ては気の赴くままに


;;; Code:

(eval-when-compile
  (require 'cl))
(require 'comint)                       ; comint-send-string

(defgroup newlisp nil
  "newlisp source code editing functions."
  :group 'newlisp
  :prefix "newlisp-"                    ; or "nl-" ?
  :version "0.2")

;; (executable-find "newlisp") or "/usr/bin/newlisp"
(defcustom newlisp-command "newlisp"
  "Filename to use to run newlisp."
  :type 'string
  :group 'newlisp)

(defvar newlisp-switches "-C")

(defvar newlisp-load-init-p t)

(defvar newlisp-process-coding-system 'utf-8
  "Coding system used for process newLISP.
If you use newLISP version UTF-8 support, its value is `utf-8'.
Otherwise maybe `shift_jis'.")

(defun newlisp-process ()
  "Return newlisp process object.
If not running, then start new process."
  (let ((default-process-coding-system
         (cons #1=newlisp-process-coding-system #1#))
        (switches (split-string newlisp-switches "\s")))
    (if (null newlisp-load-init-p)
        (pushnew "-n" switches :test #'equal))
    (get-buffer-process
     (apply #'make-comint "newlisp"
            newlisp-command nil switches))))

(defun newlisp-show-repl (&optional no-focus)
  "Display newlisp process buffer."
  (interactive "P")
  (let ((obuf (current-buffer)))
    (pop-to-buffer (process-buffer (newlisp-process)))
    (if no-focus (pop-to-buffer obuf))))

(defalias 'run-newlisp 'newlisp-show-repl)

(defun newlisp-eval (str-sexp)
  "Eval newlisp s-expression."
  (interactive "snewLISP Eval: ")
  (let ((proc (newlisp-process)))
    (labels ((sendln (str)
               (comint-send-string proc (concat str "\n"))))
      (cond ((string-match "\n" str-sexp)
             (sendln "[cmd]")
             (sendln str-sexp)
             (sendln "[/cmd]"))
            (:else
             (sendln str-sexp))))
    (newlisp-show-repl t)))

;; (defun newlisp-eval (str-sexp)
;;   "Eval newlisp s-expression."
;;   (interactive "snewLISP Eval: ")
;;   (let ((proc (newlisp-process)))
;;     (dolist (str (list "[cmd]\n"
;;                        (concat str-sexp "\n")
;;                        "[/cmd]\n"))
;;       (send-string proc str))
;;     ;; (send-string proc "\n")
;;     )
;;   (newlisp-show-repl t))

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

(defun newlisp-eval-buffer ()
  (interactive)
  (newlisp-eval-region (point-min) (point-max)))

(defun newlisp-load-file (file)
  "Load and translates newLISP from a FILE."
  (interactive (list
                (read-file-name "Load file: " (buffer-file-name))))
  (newlisp-eval (format "(load {%s})" (expand-file-name file))))

(defun newlisp-restart-process ()
  "Restart a new clean newLISP process with same command-line params.
This function is not available on Win32."
  (interactive)
  (newlisp-eval "(reset true)"))

(defun newlisp-kill-process (&optional force)
  "kill running process."
  (interactive "P")
  (if force
      (delete-process (newlisp-process))
      (newlisp-eval "(exit)")))

;; eval sync
;; (defun newlisp-eval-buffer (arg)
;;   (interactive "P")
;;   (setq arg (if arg (read-string "newLISP args: ") ""))
;;   (shell-command (format "%s \"%s\" %s"
;;                          newlisp-command
;;                          (buffer-file-name)
;;                          arg)
;;                  "*newLISP output*"))

;; (newlisp-eval
;;  (concat "(eval-string \"" (buffer-string) "\")" ))

;; eval async
(defun newlisp-execute-file (&optional cmd-args)
  (interactive (list (if current-prefix-arg
                         (read-string "execute args: " )
                         "")))
  ;; (setq arg (if arg (read-string "args: ") ""))
  (lexical-let ((outbuf (get-buffer-create "*newLISP output*")))
    (with-current-buffer outbuf (erase-buffer))
    (set-process-sentinel
     (start-process-shell-command "newlisp" outbuf
                                  newlisp-command
                                  (buffer-file-name)
                                  cmd-args)
     (lambda (process event)
       (cond ((zerop (buffer-size outbuf))
              (kill-buffer outbuf)
              (message "(no output)"))
             (:else
              (with-current-buffer outbuf
                (goto-char (point-min))
                (if (< (line-number-at-pos (point-max)) 5)
                    (message "%s" (replace-regexp-in-string
                                   "\n+$" "" (buffer-string)))
                    (pop-to-buffer (process-buffer process))))))))
    ))

(defun newlisp-begin-cmd () (interactive) (insert "[cmd]") (comint-send-input))
(defun newlisp-end-cmd () (interactive) (insert "[/cmd]") (comint-send-input))
;; (define-key inferior-newlisp-mode-map "\C-c[" 'newlisp-begin-cmd)
;; (define-key inferior-newlisp-mode-map "\C-c]" 'newlisp-end-cmd)

;;
;; Keyword List
;;
(eval-when (compile load eval)
  ;; newlisp-font-lock-keywords (lisp-font-lock-keywords)
  (defvar newlisp-primitive-keywords
    ;; newLISP v.10.1.0 on Linux IPv4 UTF-8
    ;; > (map name (filter (lambda (s) (primitive? (eval s))) (symbols MAIN)))
    ;; - define define-macro
    '("!" "!=" "$" "%" "&" "*" "+" "-" "/" ":" "<" "<<" "<=" "=" ">" ">=" ">>" "NaN?"
      "^" "abort" "abs" "acos" "acosh" "add" "address" "amb" "and" "append" "append-file"
      "apply" "args" "array" "array-list" "array?" "asin" "asinh" "assoc" "atan" "atan2"
      "atanh" "atom?" "base64-dec" "base64-enc" "bayes-query" "bayes-train" "begin" "beta"
      "betai" "bind" "binomial" "bits" "callback" "case" "catch" "ceil" "change-dir" "char"
      "chop" "clean" "close" "command-event" "cond" "cons" "constant" "context" "context?"
      "copy" "copy-file" "cos" "cosh" "count" "cpymem" "crc32" "crit-chi2" "crit-z" "current-line"
      "curry" "date" "date-value" "debug" "dec" "def-new" "default" ;; "define" "define-macro"
      "delete" "delete-file" "delete-url" "destroy" "det" "device" "difference" "directory"
      "directory?" "div" "do-until" "do-while" "doargs" "dolist" "dostring" "dotimes"
      "dotree" "dump" "dup" "empty?" "encrypt" "ends-with" "env" "erf" "error-event" "estack"
      "eval" "eval-string" "exec" "exists" "exit" "exp" "expand" "explode" "factor" "fft"
      "file-info" "file?" "filter" "find" "find-all" "first" "flat" "float" "float?" "floor"
      "flt" "for" "for-all" "fork" "format" "fv" "gammai" "gammaln" "gcd" "get-char" "get-float"
      "get-int" "get-long" "get-string" "get-url" "global" "global?" "if" "if-not" "ifft"
      "import" "inc" "index" "inf?" "int" "integer" "integer?" "intersect" "invert" "irr"
      "join" "lambda?" "last" "last-error" "legal?" "length" "let" "letex" "letn" "list"
      "list?" "load" "local" "log" "lookup" "lower-case" "macro?" "main-args" "make-dir"
      "map" "mat" "match" "max" "member" "min" "mod" "mul" "multiply" "name" "net-accept"
      "net-close" "net-connect" "net-error" "net-eval" "net-interface" "net-listen" "net-local"
      "net-lookup" "net-peek" "net-peer" "net-ping" "net-receive" "net-receive-from" "net-receive-udp"
      "net-select" "net-send" "net-send-to" "net-send-udp" "net-service" "net-sessions"
      "new" "nil?" "normal" "not" "now" "nper" "npv" "nth" "null?" "number?" "open" "or"
      "pack" "parse" "parse-date" "peek" "pipe" "pmt" "pop" "pop-assoc" "post-url" "pow"
      "pretty-print" "primitive?" "print" "println" "prob-chi2" "prob-z" "process" "prompt-event"
      "protected?" "push" "put-url" "pv" "quote" "quote?" "rand" "random" "randomize"
      "read-buffer" "read-char" "read-expr" "read-file" "read-key" "read-line" "read-utf8"
      "real-path" "receive" "ref" "ref-all" "regex" "regex-comp" "remove-dir" "rename-file"
      "replace" "reset" "rest" "reverse" "rotate" "round" "save" "search" "seed" "seek"
      "select" "semaphore" "send" "sequence" "series" "set" "set-locale" "set-ref" "set-ref-all"
      "setf" "setq" "sgn" "share" "signal" "silent" "sin" "sinh" "sleep" "slice" "sort"
      "source" "spawn" "sqrt" "starts-with" "string" "string?" "sub" "swap" "sym" "symbol?"
      "symbols" "sync" "sys-error" "sys-info" "tan" "tanh" "throw" "throw-error" "time"
      "time-of-day" "timer" "title-case" "trace" "trace-highlight" "transpose" "trim"
      "true?" "unicode" "unify" "unique" "unless" "unpack" "until" "upper-case" "utf8"
      "utf8len" "uuid" "wait-pid" "when" "while" "write-buffer" "write-char" "write-file"
      "write-line" "xfer-event" "xml-error" "xml-parse" "xml-type-tags" "zero?" "|" "~")
    "newLISP primitive keyword list.")
  (defvar newlisp-lambda-keywords
    '("define" "lambda" "fn" "define-macro" "lambda-macro"))
  (defvar newlisp-variable-keyword
    '("nil" "true" "ostype"
      "$args" "$idx" "$it" "$main-args" "$prompt-event"
      "$0" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9"
      "$10" "$11" "$12" "$13" "$14" "$15"))
  (defvar newlisp-context-keyowrds
    '("Class" "MAIN" "Tree"))
  (defvar newlisp-tag-keywords
    '("[text]" "[/text]" "[cmd]" "[/cmd]"))
  (defvar newlisp-un*x-based-function-keywords
    '("peek" "fork" "wait-pid" "net-ping" "parse-date"))
  )

(defsubst newlisp-keywords ()
  "Return newLISP keyword list as string."
  (append newlisp-primitive-keywords
          newlisp-lambda-keywords
          (unless (eq system-type 'windows-nt)
            newlisp-un*x-based-function-keywords)))

(defvar newlisp-obarray
  (let ((array (make-vector 401 0)))    ; more than keyword size
    (dolist (s (newlisp-keywords))
      (intern s array))
    array)
  "newLISP symbol table.")

(defsubst newlisp-find-symbol (string)
  "Locates a symbol whose name is STRING in a newLISP symbols."
  (intern-soft string newlisp-obarray))

(defun newlisp-complete-symbol ()
  (interactive "*")
  (let ((emacs-lisp-mode-syntax-table newlisp-mode-syntax-table)
        (obarray newlisp-obarray))
    (lisp-complete-symbol (lambda (s)
                            (newlisp-find-symbol (symbol-name s))))))

(defun newlisp-mode-setup ()
  (setq newlisp-process-coding-system
        (let ((res (shell-command-to-string
                    (format "%s -n -e \"%s\"" newlisp-command
                            '(primitive? MAIN:utf8)))))
          (if (string-match "true" res)
              'utf-8 'shift_jis)))
  (setq newlisp-primitive-keywords
        (car (read-from-string
              (shell-command-to-string
               (format "%s -n -e \"%s\"" newlisp-command
                       '(map name (filter (lambda (s) (primitive? (eval s)))
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
(defindent doargs 1)
(defindent dotree 1)

(defvar newlisp-mode-hook nil)
(defvar newlisp-mode-map
  (let ((map (make-sparse-keymap "newlisp")))
    (set-keymap-parent map lisp-mode-shared-map)
    map))
(define-key newlisp-mode-map "\M-:" 'newlisp-eval)
(define-key newlisp-mode-map "\e\C-x" 'newlisp-eval-defun)
(define-key newlisp-mode-map "\C-x\C-e" 'newlisp-eval-last-sexp)
(define-key newlisp-mode-map "\C-c\C-r" 'newlisp-eval-region)
(define-key newlisp-mode-map "\C-c\C-l" 'newlisp-load-file)
(define-key newlisp-mode-map "\C-c\C-z" 'newlisp-show-repl)
(define-key newlisp-mode-map "\e\t" 'newlisp-complete-symbol) ; ESC TAB
;; (define-key newlisp-mode-map "\C-c\C-i" 'newlisp-complete-symbol)
(define-key newlisp-mode-map [f5] 'newlisp-execute-file)
(define-key newlisp-mode-map [(control c) f4] 'newlisp-kill-process) ; C-c f4
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
  "Major mode for newLISP files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'newlisp-mode
        mode-name "newLISP")
  (use-local-map newlisp-mode-map)
  (lisp-mode-variables)
  (set-syntax-table newlisp-mode-syntax-table)
  ;; (set (make-local-variable (quote font-lock-defaults)) '(fn t nil nil fn))
  ;; (set (make-local-variable 'font-lock-keywords-case-fold-search) nil)
  (run-mode-hooks 'newlisp-mode-hook))

;; $ html2txt $NEWLISPDIR/newlisp_manual.html -o newlisp_manual.txt
;; or use www-browser [File] -> [Save Page As (Text)]
(defvar newlisp-manual-text "newlisp_manual.txt")

(defvar newlisp-manual-html
  (or (dolist (path (list "/usr/share/doc/newlisp/manual_frame.html"
                          ;; When build newlisp `make install_home'
                          "~/share/doc/newlisp/manual_frame.html"
                          "C:/Program Files/newlisp/manual_frame.html"))
        (if (file-exists-p path)
            (return path)))
      "http://www.newlisp.org/downloads/manual_frame.html"))

(defun newlisp-browse-manual ()
  (interactive)
  (browse-url-of-file newlisp-manual-html))

(defun newlisp-switch-to-manual ()
  (interactive)
  (if (file-exists-p #1=newlisp-manual-text)
      (progn
        (pop-to-buffer (find-file-noselect #1#))
        (unless (eq major-mode 'newlisp-mode) (newlisp-mode))
        (toggle-read-only t))
      (error "manual %s not exist" #1#)))

(defun newlisp-browse-manual-from-text (keyword)
  (interactive
   ;; FIXME: cannot select "lambda?" -> C-q ?
   (list (let* ((s (newlisp-find-symbol (current-word))) ; (thing-at-point 'symbol)
                (default (and s (symbol-name s))))
           (completing-read (format "newLISP manual%s: "
                                    (if default
                                        (format " (default %s)" default)
                                        ""))
                            (newlisp-keywords)
                            nil t nil nil default))))
  (if (equal keyword "setf")
      (setq keyword "setq"))
  (newlisp-switch-to-manual)
  (let ((opoint (point)))
    (goto-char (point-min))
    (unless (and (not (equal keyword ""))
                 (search-forward-regexp
                  ;; (foo)
                  ;; (foo ...)
                  ;; (foo-bar-baz) is NOT NEEDED
                  ;; (concat "^\\*syntax: (" (regexp-quote keyword) "\s?")
                  ;; e.g. "    define ! <#destructive>"
                  ;; e.g. "    define-macro"
                  (format "^    %s\s?" (regexp-quote keyword))
                  nil "noerror")
                 (progn (recenter 0) t))
      (goto-char opoint))))

(define-key newlisp-mode-map "\C-ch" 'newlisp-browse-manual-from-text)

;; (setf (get 'font-lock-add-keywords 'lisp-indent-function) 1)
;; lisp-mode.el:91
(font-lock-add-keywords 'newlisp-mode
  (list
   ;; (list "\\<\\(FIXME\\):" 1 font-lock-warning-face 'prepend)
   (cons (eval-when-compile (regexp-opt newlisp-primitive-keywords 'words)) font-lock-keyword-face)
   ;; (eval-when-compile (regexp-opt newlisp-primitive-keywords 'words))
   (cons (eval-when-compile (regexp-opt newlisp-lambda-keywords 'words))
         font-lock-function-name-face)
   (cons (eval-when-compile (regexp-opt newlisp-variable-keyword 'words))
         font-lock-constant-face)
   (cons (eval-when-compile (regexp-opt newlisp-context-keyowrds 'words))
         font-lock-type-face)
   (cons (eval-when-compile (regexp-opt newlisp-tag-keywords)) ; not 'words
         font-lock-preprocessor-face)
   (cons (eval-when-compile (regexp-opt newlisp-un*x-based-function-keywords 'words))
         font-lock-warning-face))
  )

(provide 'newlisp)

;;; newlisp.el ends here
