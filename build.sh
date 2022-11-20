#!/bin/sh

# Adapted from
# https://github.com/cicakhq/potato/blob/master/tools/build_binary.sh;
# Quicklisp path hack from
# https://www.darkchestnut.com/2016/quicklisp-load-personal-projects-from-arbitrary-locations/

sbcl --non-interactive \
     --disable-debugger \
     --eval '(pushnew (truename ".") ql:*local-project-directories*)' \
     --eval '(ql:register-local-projects)' \
     --eval '(ql:quickload :steelcut)' \
     --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "steelcut" :toplevel #'"'"'steelcut:main :executable t))'
