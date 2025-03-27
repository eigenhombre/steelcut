FROM ubuntu

RUN apt-get -qq -y update
RUN apt-get -qq -y upgrade

RUN apt-get install -qq -y sbcl make curl git ecl

# Pull down Quicklisp and install it
RUN curl -s -o quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp

RUN ecl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/janice/quicklisp")' \
         --eval '(ql:quickload :1am)'

RUN echo | ecl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)'
RUN echo | sbcl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit

# Get Ultralisp (cl-oju, etc.):
RUN sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org" :prompt nil)'
# Installed once, doesn't need to be installed again:
# RUN ecl --eval '(ql-dist:install-dist "http://dist.ultralisp.org" :prompt nil)'

ENV LISP_HOME=/home/janice/quicklisp/local-projects
ENV BINDIR=/home/janice/bin
WORKDIR /home/janice/steelcut

# Run the unit tests:
COPY *.asd *.sh Makefile /home/janice/steelcut/
COPY src /home/janice/steelcut/src
COPY test /home/janice/steelcut/test
# RUN make test-ecl
RUN make clean test
RUN make
RUN mkdir /home/janice/bin
RUN make install
RUN $BINDIR/steelcut foo
WORKDIR /home/janice/quicklisp/local-projects/foo
RUN make
RUN make install
RUN $BINDIR/foo
