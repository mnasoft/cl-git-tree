#!/bin/bash
# Установочный скрипт для git-tree (Linux)

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="$PROJECT_DIR/git-tree"
LINK_DIR="/usr/local/bin"
LINK="$LINK_DIR/git-tree"
CLI_SRC="$PROJECT_DIR/cli-script.lisp"
CLI_DEST_DIR="/usr/local/lib/git-tree"
CLI_DEST="$CLI_DEST_DIR/cli-script.lisp"

# Функция проверки sudo
ensure_sudo() {
  if [ "$EUID" -ne 0 ]; then
    echo "⚠️  Нужен sudo для записи в /usr/local/"
    # Используем bash явно если доступен, иначе sh
    if command -v bash >/dev/null 2>&1; then
      exec bash "$0" "$@"
    else
      exec sudo "$0" "$@"
    fi
  fi
}

install_link() {
  ensure_sudo "$@"
  
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
  
  # Создать директорию для бинарника если её нет
  if [ ! -d "$LINK_DIR" ]; then
    mkdir -p "$LINK_DIR"
    echo "📁 Директория создана: $LINK_DIR"
  fi

  # Создать каталог для cli-скрипта
  mkdir -p "$CLI_DEST_DIR"
  if [ $? -ne 0 ]; then
    echo "❌ Ошибка создания директории $CLI_DEST_DIR"
    exit 1
  fi

  # Копировать cli-скрипт в установленное место
  cp -f "$CLI_SRC" "$CLI_DEST"
  if [ $? -ne 0 ]; then
    echo "❌ Ошибка копирования cli-скрипта в $CLI_DEST"
    exit 1
  fi
  echo "📄 CLI-скрипт скопирован: $CLI_DEST"
  
  # Создать символьную ссылку
  ln -sf "$TARGET" "$LINK"
  if [ $? -eq 0 ]; then
    chmod +x "$TARGET"
    echo "✅ Установка завершена!"
    echo "   Симлинк: $LINK → $TARGET"
    echo "   CLI-скрипт: $CLI_DEST"
    echo "   Используйте: git-tree --help"
  else
    echo "❌ Ошибка создания симлинка"
    exit 1
  fi
}

uninstall_link() {
  ensure_sudo "$@"
  
  # Удалить симлинк
  if [ -L "$LINK" ] || [ -f "$LINK" ]; then
    rm -f "$LINK"
    echo "🗑️  Удалено: $LINK"
  else
    echo "⚠️  Ссылка не найдена: $LINK"
  fi
  
  # Удалить cli-скрипт
  if [ -f "$CLI_DEST" ]; then
    rm -f "$CLI_DEST"
    echo "🗑️  Удалён CLI-скрипт: $CLI_DEST"
  fi
  
  # Удалить директорию если она пуста
  if [ -d "$CLI_DEST_DIR" ] && [ -z "$(ls -A "$CLI_DEST_DIR")" ]; then
    rmdir "$CLI_DEST_DIR"
    echo "🗑️  Удалена директория: $CLI_DEST_DIR"
  fi
}

case "$1" in
  --help|-h)
    echo "Использование: $0 [--install|--uninstall|--help]"
    echo "  --install    установить git-tree в систему (по умолчанию)"
    echo "  --uninstall  удалить git-tree из системы"
    echo "  --help       показать эту справку"
    ;;
  --uninstall)
    uninstall_link "$@"
    ;;
  --install)
    install_link "$@"
    ;;
  "")
    install_link "$@"
    ;;
  *)
    echo "❌ Неизвестная опция: $1"
    echo "Используйте: $0 --help"
    exit 1
    ;;
esac
