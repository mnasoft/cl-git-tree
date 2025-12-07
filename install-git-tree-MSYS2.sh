#!/bin/bash
# Установочный скрипт для git-tree в MSYS2

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="$PROJECT_DIR/git-tree-MSYS2"
LINK_DIR="/usr/local/bin"
LINK="$LINK_DIR/git-tree"
CLI_SRC="$PROJECT_DIR/cli-script.lisp"
CLI_DEST_DIR="/usr/local/lib/git-tree"
CLI_DEST="$CLI_DEST_DIR/cli-script.lisp"

install_link() {
  # Проверить существование исполняемого файла
  if [ ! -f "$TARGET" ]; then
    echo "❌ Ошибка: файл не найден: $TARGET"
    exit 1
  fi

  # Проверить наличие исходного cli-скрипта
  if [ ! -f "$CLI_SRC" ]; then
    echo "❌ Ошибка: cli-скрипт не найден: $CLI_SRC"
    exit 1
  fi
  
  # Создать директорию если её нет
  if [ ! -d "$LINK_DIR" ]; then
    mkdir -p "$LINK_DIR"
    echo "📁 Директория создана: $LINK_DIR"
  fi

  # Создать каталог для cli-скрипта
  mkdir -p "$CLI_DEST_DIR"

  # Копировать cli-скрипт в установленное место
  cp -f "$CLI_SRC" "$CLI_DEST"
  if [ $? -ne 0 ]; then
    echo "❌ Ошибка копирования cli-скрипта в $CLI_DEST"
    exit 1
  fi
  
  # Создать символьную ссылку
  ln -sf "$TARGET" "$LINK"
  if [ $? -eq 0 ]; then
    chmod +x "$TARGET"
    echo "✅ Установка завершена!"
    echo "   Симлинк: $LINK → $TARGET"
    echo "   Используйте: git-tree --help"
  else
    echo "❌ Ошибка создания симлинка"
    exit 1
  fi
}

uninstall_link() {
  if [ -L "$LINK" ] || [ -f "$LINK" ]; then
    rm -f "$LINK"
    echo "🗑️  Удалено: $LINK"
  else
    echo "⚠️  Ссылка не найдена: $LINK"
  fi
}

case "$1" in
  --help|-h)
    echo "Использование: $0 [--install|--uninstall|--help]"
    echo "  --install    установить"
    echo "  --uninstall  удалить симлинк"
    echo "  --help       показать эту справку"
    ;;
  --uninstall)
    uninstall_link
    ;;
  --install)
    install_link
    ;;
  *)
    echo "Неизвестная опция или пустой аргумент: ${1:-<пусто>}"
    echo "Используйте: $0 --help"
    exit 1
    ;;
esac
