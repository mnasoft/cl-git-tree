#!/bin/bash
# install-git-tree-bin.sh — установщик standalone бинарника git-tree

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN_SRC="$PROJECT_DIR/bin/git-tree-bin.exe"
BIN_DEST="/usr/local/bin/git-tree-bin.exe"
BIN_WRAPPER_SRC="$PROJECT_DIR/bin/git-tree"
BIN_WRAPPER_DEST="/usr/local/bin/git-tree"
SRC_LISP="$PROJECT_DIR/git-tree-bin.lisp"
LISP_DEST_DIR="/usr/local/lib/git-tree"
LISP_DEST="$LISP_DEST_DIR/git-tree-bin.lisp"

# По умолчанию копируем исходник; можно отключить флагом --no-source
COPY_SOURCE=1
for arg in "$@"; do
  if [ "$arg" = "--no-source" ]; then
    COPY_SOURCE=0
  fi
done

ensure_sudo() {
  # Не вызывать sudo под MSYS2/MinGW/Cygwin — установка выполняется под текущим пользователем
  if [ -n "$MSYSTEM" ] || uname -o 2>/dev/null | grep -iq "msys\|mingw\|cygwin"; then
    return 0
  fi
  if [ "$EUID" -ne 0 ]; then
    echo "⚠️  Нужен sudo для записи в /usr/local/bin"
    exec sudo "$0" "$@"
  fi
}

install_bin() {
  ensure_sudo "$@"
  if [ ! -f "$BIN_SRC" ]; then
    echo "❌ Не найден бинарник: $BIN_SRC"
    exit 1
  fi
  if [ ! -f "$BIN_WRAPPER_SRC" ]; then
    echo "❌ Не найден обёрточный скрипт: $BIN_WRAPPER_SRC"
    exit 1
  fi
  # Удалить старую ссылку или файл, если есть
  if [ -L "$BIN_DEST" ] || [ -f "$BIN_DEST" ]; then
    rm -f "$BIN_DEST"
    echo "🗑️  Удалён старый бинарник/ссылка: $BIN_DEST"
  fi
  cp -f "$BIN_SRC" "$BIN_DEST"
  chmod +x "$BIN_DEST"
  echo "✅ Установлен: $BIN_DEST"
  "$BIN_DEST" --version 2>/dev/null || true

  # Установить обёрточный скрипт
  if [ -L "$BIN_WRAPPER_DEST" ] || [ -f "$BIN_WRAPPER_DEST" ]; then
    rm -f "$BIN_WRAPPER_DEST"
    echo "🗑️  Удалён старый скрипт/ссылка: $BIN_WRAPPER_DEST"
  fi
  cp -f "$BIN_WRAPPER_SRC" "$BIN_WRAPPER_DEST"
  chmod +x "$BIN_WRAPPER_DEST"
  echo "✅ Установлен: $BIN_WRAPPER_DEST"

  # Оставляем установку обёрточного скрипта в $BIN_WRAPPER_DEST
  # (не перезаписываем его симлинком на git-tree-bin.exe)

  # Копировать lisp-исходник для воспроизводимости (опционально)
  if [ "$COPY_SOURCE" -eq 1 ]; then
    mkdir -p "$LISP_DEST_DIR"
    if [ -f "$SRC_LISP" ]; then
      cp -f "$SRC_LISP" "$LISP_DEST"
      echo "📄 Исходник git-tree-bin.lisp установлен: $LISP_DEST"
    fi
  else
    echo "ℹ️  Пропускаю копирование исходника (--no-source)"
  fi
}

uninstall_bin() {
  ensure_sudo "$@"
  if [ -f "$BIN_DEST" ]; then
    rm -f "$BIN_DEST"
    echo "🗑️  Удалён: $BIN_DEST"
  else
    echo "⚠️  Не найден: $BIN_DEST"
  fi
  if [ -f "$LISP_DEST" ]; then
    rm -f "$LISP_DEST"
    echo "🗑️  Удалён исходник: $LISP_DEST"
  fi
  if [ -d "$LISP_DEST_DIR" ] && [ -z "$(ls -A "$LISP_DEST_DIR")" ]; then
    rmdir "$LISP_DEST_DIR"
    echo "🗑️  Удалена директория: $LISP_DEST_DIR"
  fi
}

case "$1" in
  --help|-h)
    echo "Установщик standalone git-tree (бинарник)"
    echo ""
    echo "Использование: $0 [--install|--uninstall|--help] [--no-source]"
    echo "  --install    установить бинарник (по умолчанию)"
    echo "  --uninstall  удалить бинарник"
    echo "  --help       показать эту справку"
    echo "  --no-source  не копировать git-tree-bin.lisp в /usr/local/lib/git-tree"
    ;;
  --uninstall)
    uninstall_bin "$@"
    ;;
  --install|"")
    install_bin "$@"
    ;;
  *)
    echo "❌ Неизвестная опция: $1"
    echo "Используйте: $0 --help"
    exit 1
    ;;
esac
