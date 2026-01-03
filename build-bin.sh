#!/bin/bash
# build-bin.sh — сборка standalone бинарника cl-git-tree
set -e

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
SBCL=${SBCL:-sbcl}
OUT=bin/git-tree-bin.exe
WRAPPER_SRC=bin/git-tree

clean_bin_dir() {
    # Очистить каталог bin перед сборкой    
    mkdir -p bin
    rm -fr bin/*
}

quicklisp_init() {
    # Явная инициализация quicklisp, если есть
    if [ -f "$HOME/quicklisp/setup.lisp" ]; then
        QL_SETUP="--load $HOME/quicklisp/setup.lisp"
    else
        QL_SETUP=""
    fi
}

build() {
    # Сборка бинарного файла
    $SBCL $QL_SETUP --no-userinit --no-sysinit \
          --eval "(require 'asdf)" \
          --eval "(asdf:load-system :cl-git-tree)" \
          --eval "(sb-ext:save-lisp-and-die \"$OUT\" :executable t :compression t)" \
          --quit
}

mk_wrapper_src() {
    echo '#!/usr/local/bin/git-tree-bin.exe --script' > $WRAPPER_SRC
    echo >> $WRAPPER_SRC
    echo '(cl-git-tree/cli:main sb-ext:*posix-argv*)' >> $WRAPPER_SRC
    chmod +x $WRAPPER_SRC
}

makeing() {
    clean_bin_dir
    mk_wrapper_src
    quicklisp_init
    build
}

makeing
