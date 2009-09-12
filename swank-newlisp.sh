#!/bin/sh

newlisp -n swank-newlisp.lsp -e "(swank:create-server)"

# newlisp -n <<EOF
# (load "./swank-newlisp.lsp")
# (swank:create-server)
# EOF

# and then `M-x slime-connect' Host:127.0.0.1, Port:4005
