newLISP utility files
=====================

LISP風軽量スクリプト言語newLISPのユーティリティファイル群です。

newLISP Home - <http://www.newlisp.org/>

> newLISP is a Lisp-like, general-purpose scripting language. It has all
the magic of traditional Lisp but is easier to learn and use. newLISP is
friendly, fast, and small. Most of the functions you will ever need are
already built in. newLISP runs on most OS platforms.


init.lsp
--------
My newlisp initialization file.

* Other snippet, see below:

  <https://github.com/kosh04/newlisp.snippet>


newlisp.el
----------
newLISP editing mode for Emacs.

- Editing newlisp code
- Syntax highlighting [*]
- Keyword completion [*]
- Run newlisp process
- eval-region, eval-last-sexp

[*] primitive keyword only.


newlisp_manual.txt (v.10.3.0rev)
-----------------------------------
Text file converted from newlisp html document.

newlisp.el と組み合わせて利用します。
ブラウザで開くと非常に重いので、一旦ローカルに落とす方が良いです。

`wget http://github.com/kosh04/newlisp-files/raw/master/newlisp_manual.txt`

- newLISP Manual and Reference

  <http://www.newlisp.org/downloads/newlisp_manual.html>


swank-newlisp.lsp
-----------------
Swank server for newLISP.

What is SLIME? - <http://common-lisp.net/project/slime/>

* swank-newlisp.sh - swank server startup wrapper
