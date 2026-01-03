#!/bin/bash
# build-src.sh — создание оберточного файла для запуска
set -e

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
SBCL=${SBCL:-sbcl}
WRAPPER_SRC=bin/git-tree-script

clean_bin_dir() {
    # Очистить каталог bin перед сборкой    
    mkdir -p bin
    rm -fr bin/*
}

mk_wrapper_src() {
    SBCL_EXEC=`which sbcl`
    echo "#!$SBCL_EXEC --script" > $WRAPPER_SRC
    echo >> $WRAPPER_SRC
    echo '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' >> $WRAPPER_SRC
    echo ';; Тихая загрузка системы' >> $WRAPPER_SRC
    echo '(let ((*standard-output* (make-broadcast-stream)))' >> $WRAPPER_SRC
    echo '    (ql:quickload :cl-git-tree :silent t))' >> $WRAPPER_SRC
    echo >> $WRAPPER_SRC
    echo '(cl-git-tree/cli:main sb-ext:*posix-argv*)' >> $WRAPPER_SRC
    chmod +x $WRAPPER_SRC
}

makeing() {
    clean_bin_dir
    mk_wrapper_src
}

makeing
