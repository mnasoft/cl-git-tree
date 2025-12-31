#!/bin/bash
# build-bin.sh — сборка standalone бинарника cl-git-tree
set -e

SBCL=${SBCL:-sbcl}
OUT=bin/git-tree.exe
SRC=git-tree-bin.lisp
BIN_WRAPPER_SRC=bin/git-tree



# Очистить каталог bin перед сборкой
mkdir -p bin
rm -fr bin/*

# Явная инициализация quicklisp, если есть
if [ -f "$HOME/quicklisp/setup.lisp" ]; then
  QL_SETUP="--load $HOME/quicklisp/setup.lisp"
else
  QL_SETUP=""
fi

$SBCL $QL_SETUP --no-userinit --no-sysinit \
  --eval "(require 'asdf)" \
  --eval "(asdf:load-system :cl-git-tree)" \
  --load $SRC \
  --eval "(sb-ext:save-lisp-and-die \"$OUT\" :executable t :compression t :toplevel #'(lambda () (cl-git-tree/cli:main sb-ext:*posix-argv*)))" \
  --quit

# Генерация обёртки для запуска git-tree.exe
cat > "$BIN_WRAPPER_SRC" <<'EOF'
#!/bin/bash

# Запуск бинарной версии cl-git-tree через git-tree.exe
# Аналогично git-tree, но использует бинарник

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_PATH="$SCRIPT_DIR/git-tree.exe"

if [[ ! -x "$BIN_PATH" ]]; then
  echo "Ошибка: $BIN_PATH не найден или не исполняемый."
  exit 1
fi

exec "$BIN_PATH" "$@"
EOF
chmod +x "$BIN_WRAPPER_SRC"

#if command -v upx[] >/dev/null 2>&1; then
#  echo "Сжимаю бинарник через upx..."
#  upx --best --lzma "$OUT"
#  echo "Бинарник сжат: $OUT"
#else
#  echo "Бинарник собран: $OUT (upx не найден, сжатие не выполнено)"
