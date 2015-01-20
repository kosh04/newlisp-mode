newlisp-mode
============

[![MELPA](http://melpa.org/packages/newlisp-mode-badge.svg)](http://melpa.org/#/newlisp-mode)

This is a newLISP editing mode for Emacs.

- Editing newlisp code
- Syntax highlighting [*]
- Keyword completion [*]
- Run inferior newlisp process

[*] primitive keyword only.

More info, See [Wiki](https://github.com/kosh04/newlisp-mode/wiki)


Installation
============

Clone the git repository into a local directory.
or Download [ZIP](https://github.com/kosh04/newlisp-mode/archive/master.zip).

	$ git clone git://github.com/kosh04/newlisp-mode.git

or Install from [MELPA](http://melpa.milkbox.net/#/newlisp-mode). (thanks @yasuyk)

    M-x package-install newlisp-mode

and Add the following to your `.emacs`

	(add-to-list 'load-path "/path/to/newlisp-mode")
	(require 'newlisp-mode)


License
=======

newLISP Mode uses the [GNU General Public License 3](http://www.gnu.org/copyleft/gpl.html).
