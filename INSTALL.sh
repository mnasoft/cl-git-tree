#!/bin/bash
# Unified installer: installs/uninstalls script (non-binary) and binary variants
# Usage: INSTALL.sh [--install|--uninstall] [--script|--binary|--both] [--no-source]

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
# Paths and defaults used by embedded installers
LINK_DIR="/usr/local/bin"
LINK="$LINK_DIR/git-tree"
CLI_SRC="$PROJECT_DIR/cli-script.lisp"
CLI_DEST_DIR="/usr/local/lib/git-tree"
CLI_DEST="$CLI_DEST_DIR/cli-script.lisp"

# Binary installer paths
BIN_SRC="$PROJECT_DIR/bin/git-tree-bin.exe"
BIN_DEST="/usr/local/bin/git-tree-bin.exe"
BIN_WRAPPER_SRC="$PROJECT_DIR/bin/git-tree"
BIN_WRAPPER_DEST="/usr/local/bin/git-tree"
SRC_LISP="$PROJECT_DIR/git-tree-bin.lisp"
LISP_DEST_DIR="$CLI_DEST_DIR"
LISP_DEST="$LISP_DEST_DIR/git-tree-bin.lisp"

ACTION="install"
MODE="script"
COPY_SOURCE=1

usage(){
  cat <<EOF
Usage: $0 [--install|--uninstall] [--script|--binary] [--no-source] [--help]

Options:
  --install       Install (default)
  --uninstall     Uninstall both variants
  --script        Install script (non-binary) variant
  --binary        Install binary variant
  run_script_install(){
    install_link
  }

  run_bin_install(){
    install_bin
  }

  run_script_uninstall(){
    uninstall_link
  }

  run_bin_uninstall(){
    uninstall_bin
  }

  detect_system() {
    if [ -n "$MSYSTEM" ] || uname -o | grep -iq "msys\|mingw\|cygwin"; then
      echo "msys2"
    else
      echo "linux"
    fi
  }

  SYSTEM=$(detect_system)

  ensure_sudo() {
    if [ "$SYSTEM" = "linux" ] && [ "$EUID" -ne 0 ]; then
      echo "⚠️  Нужен sudo для записи в /usr/local/"
      if command -v bash >/dev/null 2>&1; then
        exec sudo bash "$0" "$@"
      else
        exec sudo "$0" "$@"
      fi
    fi
  }

  install_link() {
    if [ "$SYSTEM" = "linux" ]; then
      ensure_sudo "$@"
    fi
    if [ "$SYSTEM" = "msys2" ]; then
      TARGET="$PROJECT_DIR/sh/git-tree-MSYS2"
    else
      TARGET="$PROJECT_DIR/sh/git-tree"
    fi
    if [ ! -f "$TARGET" ]; then
      echo "❌ Ошибка: файл не найден: $TARGET"
      exit 1
    fi
    if [ ! -f "$CLI_SRC" ]; then
      echo "❌ Ошибка: cli-скрипт не найден: $CLI_SRC"
      exit 1
    fi
    if [ ! -d "$LINK_DIR" ]; then
      mkdir -p "$LINK_DIR"
      echo "📁 Директория создана: $LINK_DIR"
    fi
    mkdir -p "$CLI_DEST_DIR"
    if [ $? -ne 0 ]; then
      echo "❌ Ошибка создания директории $CLI_DEST_DIR"
      exit 1
    fi
    if [ "$COPY_SOURCE" -eq 1 ]; then
      cp -f "$CLI_SRC" "$CLI_DEST"
      if [ $? -ne 0 ]; then
        echo "❌ Ошибка копирования cli-скрипта в $CLI_DEST"
        exit 1
      fi
      echo "📄 CLI-скрипт скопирован: $CLI_DEST"
    else
      echo "ℹ️  Пропускаю копирование cli-скрипта (--no-source)"
    fi
    ln -sf "$TARGET" "$LINK"
    if [ $? -eq 0 ]; then
      chmod +x "$TARGET"
      echo "✅ Установка завершена!"
      echo "   Система: $SYSTEM"
      echo "   Симлинк: $LINK → $TARGET"
      echo "   CLI-скрипт: $CLI_DEST"
      echo "   Используйте: git-tree --help"
    else
      echo "❌ Ошибка создания симлинка"
      exit 1
    fi
  }

  uninstall_link() {
    if [ "$SYSTEM" = "linux" ]; then
      ensure_sudo "$@"
    fi
    if [ -L "$LINK" ] || [ -f "$LINK" ]; then
      rm -f "$LINK"
      echo "🗑️  Удалено: $LINK"
    else
      echo "⚠️  Ссылка не найдена: $LINK"
    fi
    if [ -f "$CLI_DEST" ]; then
      rm -f "$CLI_DEST"
      echo "🗑️  Удалён CLI-скрипт: $CLI_DEST"
    fi
    if [ -d "$CLI_DEST_DIR" ] && [ -z "$(ls -A "$CLI_DEST_DIR")" ]; then
      rmdir "$CLI_DEST_DIR"
      echo "🗑️  Удалена директория: $CLI_DEST_DIR"
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
    if [ -L "$BIN_DEST" ] || [ -f "$BIN_DEST" ]; then
      rm -f "$BIN_DEST"
      echo "🗑️  Удалён старый бинарник/ссылка: $BIN_DEST"
    fi
    cp -f "$BIN_SRC" "$BIN_DEST"
    chmod +x "$BIN_DEST"
    echo "✅ Установлен: $BIN_DEST"
    "$BIN_DEST" --version 2>/dev/null || true
    if [ -L "$BIN_WRAPPER_DEST" ] || [ -f "$BIN_WRAPPER_DEST" ]; then
      rm -f "$BIN_WRAPPER_DEST"
      echo "🗑️  Удалён старый скрипт/ссылка: $BIN_WRAPPER_DEST"
    fi
    cp -f "$BIN_WRAPPER_SRC" "$BIN_WRAPPER_DEST"
    chmod +x "$BIN_WRAPPER_DEST"
    echo "✅ Установлен: $BIN_WRAPPER_DEST"
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
