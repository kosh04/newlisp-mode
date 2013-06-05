newlisp-mode.el
===============

This is a newLISP editing mode for Emacs.

- Editing newlisp code
- Syntax highlighting [*]
- Keyword completion [*]
- Run newlisp process
- eval-region, eval-last-sexp

[*] primitive keyword only.

More info, See [Wiki](https://github.com/kosh04/newlisp-mode.el/wiki)


Installation
============

Clone the git repository into a local directory.
or Download [ZIP](https://github.com/kosh04/newlisp-mode.el/archive/master.zip).

	$ git clone git://github.com/kosh04/newlisp-mode.el.git

Add the following to your `.emacs`

	(add-to-list 'load-path "/path/to/newlisp-mode.el/")
	(require 'newlisp-mode)
	(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
	(add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))


License
=======

newLISP Mode uses the [GNU General Public License 3.](http://www.gnu.org/copyleft/gpl.html)
