newLISP utility files
=====================

Lisp-like な軽量スクリプト言語 newLISP 用のユーティリティファイル群です。
ただし、あまり実用的ではありません。

newLISP Home - http://www.newlisp.org/

 newLISP is a Lisp-like, general-purpose scripting language. It has all
the magic of traditional Lisp but is easier to learn and use. newLISP is
friendly, fast, and small. Most of the functions you will ever need are
already built in. newLISP runs on most OS platforms.

init.lsp
--------
雑多な関数群。


newlisp.el
----------
Emacs 用の編集メジャーモード。

.何が出来るのか
- newlispファイルの編集
- newlisp プロセスの起動、操作
- 指定したリージョンをEval (eval-region, eval-last-sexp)
- シンタックスハイライト


newlisp_manual.txt (v.10.1.0 rev 4)
-----------------------------------
HTML マニュアルをテキストに変換したもの。

newLISP Manual and Reference - http://www.newlisp.org/downloads/newlisp_manual.html

* newlisp_manual.txt.tar.gz - 上記のファイルを圧縮したもの


swank-newlisp.lsp
-----------------
Emacs と通信するための swank サーバファイル。

最低限の機能しか持たないので、こちらより newlisp.el の方がまだマシです。

SLIME  - http://common-lisp.net/project/slime/

* swank-newlisp.sh - swankサーバ起動用 wrapper
