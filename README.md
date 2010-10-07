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
雑多な関数群。

* nl-modules.git - その他モジュールはこちら

  <http://github.com/kosh04/nl-modules>


newlisp.el
----------
Emacsで編集するための簡易メジャーモード。

何が出来るのか

- ファイルの編集
- プロセスの起動、操作
- リージョンの評価 (eval-region, eval-last-sexp)
- シンタックスハイライト
- 関数名の補完 (組み込み関数のみ)


newlisp_manual.txt (v.10.2.8 rev-19)
-----------------------------------
HTMLマニュアルをテキストに変換したもの。
ブラウザで開くと非常に重いので、一旦ローカルに落とす方が良い

`wget http://github.com/kosh04/newlisp-files/raw/master/newlisp_manual.txt`

newLISP Manual and Reference
<http://www.newlisp.org/downloads/newlisp_manual.html>


swank-newlisp.lsp
-----------------
Emacsと通信するためのSwankサーバファイル。今のところ最低限の機能のみ。

SLIME - <http://common-lisp.net/project/slime/>

* swank-newlisp.sh - swankサーバ起動用 wrapper
