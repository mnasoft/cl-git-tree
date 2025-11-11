#!/bin/bash
# Установочный скрипт для git-tree

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="$PROJECT_DIR/git-tree-MSYS2"
LINK="/usr/local/bin/git-tree"

install_link() {
#  if [ "$EUID" -ne 0 ]; then
#    echo "⚠️  Нужен sudo для записи в /usr/local/bin"
#    exec sudo "$0" "$@"
#  fi
  ln -sf "$TARGET" "$LINK"
  chmod +x "$TARGET"
  echo "✅ Симлинк создан: $LINK → $TARGET"
}

uninstall_link() {
#  if [ "$EUID" -ne 0 ]; then
#    echo "⚠️  Нужен sudo для удаления из /usr/local/bin"
#    exec sudo "$0" "$@"
#  fi
  rm -f "$LINK"
  echo "🗑️  Симлинк удалён: $LINK"
}

case "$1" in
  --uninstall)
    uninstall_link
    ;;
  *)
    install_link
    ;;
esac
