;;; newlisp.el --- newLISP mode for Emacs

;; This file is NOT part of GNU Emacs.

;; Time-stamp: <2009-04-19T16:46:25>

;; Usage:
;; (require 'newlisp)
;; (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))

;; Todo:
;; - newlisp-eval-buffer を独立プロセスで
;; - newlisp-mode
;; - 色付け (newlisp-mode.el, elisp-font-lock.el)
;; - シンボル補完 -> etags, complete-symbol, dabbrev-expand
;; - pop-to-buffer は縦分割を好む人もいるかも
;; - elisp の書式チェック (checkdoc) -> 関数にはドキュメントを書けってさ
;; - defcustomなど
;; - 構文ステーブルを弄る (`|' はシンボル, `#' はコメント)
;; - 全ては気の赴くままに

;;; Code:
(eval-when-compile (require 'cl))
(require 'comint)
;; (require 'inf-lisp)

(defvar *newlisp-command* "newlisp")
;; (defvar *newlisp-command-option* "")

(defvar *newlisp-process-coding-system* '(utf-8 . utf-8))

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
  (interactive "snewLISP eval: ")
  (let ((proc (newlisp-process)))
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
         (with-syntax-table lisp-mode-syntax-table
           (newlisp-eval-region (progn
                                  ;; 'hoge
                                  (unless (looking-at "\\_<")
                                    (backward-sexp))
                                  (point))
                                (progn
                                  (forward-sexp)
                                  (point))))
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

(defun newlisp-complete-symbol (&optional predicate)
  (interactive)
  (error "Undefined"))

(defun newlisp-mode-setup ()
  (setq *newlisp-process-coding-system*
        (let ((res (shell-command-to-string
                    (format "%s -n -e \"(primitive? MAIN:utf8)\"" *newlisp-command*))))
          (if (string-match "true" res)
              '(utf-8 . utf-8)
              '(shift_jis . shift_jis)))) ; or 'sjis ?
  ;; $ newlisp -n -e "(builtin-symbols)"
  )

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

;;;###autoload
(defun newlisp-mode ()
  "Major mode for editing newLISP code to run in Emacs."
  (interactive)
  (kill-all-local-variables)
  (use-local-map newlisp-mode-map)
  (setq major-mode 'newlisp-mode
        mode-name "newLISP")
  (lisp-mode-variables 'and-use-lisp-syntax)
  ;; `#'もコメント扱いにしたい
  ;; (set (make-local-variable 'font-lock-keywords-case-fold-search) nil)
  (run-mode-hooks 'newlisp-mode-hook))

;; $ html2txt $NEWLISPDIR/newlisp_manual.html -o newlisp_manual.txt
(defvar *newlisp-manual-text* "C:/home/lxuser/newlisp/newlisp_manual.txt")

(defvar *newlisp-symbols*
  (eval-when-compile
    (car (read-from-string
          (shell-command-to-string
           (format "%s -n -e \"(map name (symbols MAIN))\"" *newlisp-command*))))))

(defun newlisp-manual-from-text (str)
  (interactive
   (list (completing-read "newLISP manual: "
                          *newlisp-symbols*
                          nil t
                          (car (member (current-word) *newlisp-symbols*)))))
  (let ((obuf (current-buffer)))
    (pop-to-buffer (find-file-noselect *newlisp-manual-text*))
    (toggle-read-only t)
    (let ((opoint (point)))
      (goto-char (point-min))
      (unless (search-forward (concat "*syntax: (" str) nil 'noerror)
        (goto-char opoint)
        (pop-to-buffer obuf)
        (message "Function Not Found: %s" str)))))

(put 'font-lock-add-keywords 'lisp-indent-function 1)
;; see lisp-mode.el:91
(font-lock-add-keywords 'newlisp-mode
  (list
   ;; (list "\\<\\(FIXME\\):" 1 font-lock-warning-face 'prepend)
   (cons (eval-when-compile
           (regexp-opt *newlisp-symbols* 'words))
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
         font-lock-builtin-face)
   (cons (eval-when-compile
           (regexp-opt '("peek" "fork" "wait-pid" "net-ping" "parse-date") 'words))
         font-lock-warning-face)
   ))

;; (defun newlisp-make-regexp-opt (&rest strings) (eval-when-compile (regexp-opt strings 'words)))

(provide 'newlisp)

;;; newlisp.el ends here
