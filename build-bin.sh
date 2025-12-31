#!/bin/bash
# build-bin.sh — сборка standalone бинарника cl-git-tree
set -e

SBCL=${SBCL:-sbcl}
OUT=bin/git-tree-bin.exe
SRC=git-tree-bin.lisp
BIN_WRAPPER_SRC=bin/git-tree

# Очистить каталог bin перед сборкой
mkdir -p bin
rm -fr bin/*

# Генерация обёртки для запуска git-tree-bin.exe
cat > "$BIN_WRAPPER_SRC" <<'EOF'
#!/bin/bash

# Запуск бинарной версии cl-git-tree через git-tree-bin.exe
# Аналогично git-tree, но использует локальный бинарник git-tree-bin.exe

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_PATH="$SCRIPT_DIR/git-tree-bin.exe"

if [[ ! -x "$BIN_PATH" ]]; then
  echo "Ошибка: $BIN_PATH не найден или не исполняемый."
  exit 1
fi

exec "$BIN_PATH" "$@"
EOF
chmod +x "$BIN_WRAPPER_SRC"
# Информационный вывод о сборке
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
BLUE='\033[1;34m'
RESET='\033[0m'

printf "\n${GREEN}=== cl-git-tree: building standalone binary ===${RESET}\n"
printf "${YELLOW}Output:${RESET} %s\n" "$OUT"
printf "${YELLOW}Wrapper:${RESET} %s\n" "$BIN_WRAPPER_SRC"
printf "${BLUE}Started: %s${RESET}\n\n" "$(date '+%Y-%m-%d %H:%M:%S')"

# Явная инициализация quicklisp, если есть
if [ -f "$HOME/quicklisp/setup.lisp" ]; then
  QL_SETUP="--load $HOME/quicklisp/setup.lisp"
else
  QL_SETUP=""
fi

# Информационный вывод перед сборкой: показываем SBCL и использование Quicklisp
printf "${GREEN}SBCL executable:${RESET} %s\n" "$SBCL"
if [ -n "$QL_SETUP" ]; then
  printf "${GREEN}Quicklisp init:${RESET} %s\n" "$HOME/quicklisp/setup.lisp"
else
  printf "${GREEN}Quicklisp init:${RESET} (not found)\n"
fi
printf "${YELLOW}Invoking SBCL to build the binary...${RESET}\n\n"

$SBCL $QL_SETUP --no-userinit --no-sysinit \
  --eval "(require 'asdf)" \
  --eval "(asdf:load-system :cl-git-tree)" \
  --load $SRC \
  --eval "(sb-ext:save-lisp-and-die \"$OUT\" :executable t :compression t :toplevel #'(lambda () (cl-git-tree/cli:main sb-ext:*posix-argv*)))" \
  --quit

