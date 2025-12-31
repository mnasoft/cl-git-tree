#!/bin/bash
# Unified installer: installs/uninstalls script (non-binary) and binary variants
# Usage: INSTALL.sh [--install|--uninstall] [--script|--binary|--both] [--no-source]

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
# Paths and defaults used by embedded installers
LINK_DIR="/usr/local/bin"
LINK="$LINK_DIR/git-tree"
# Примечание: cli-script.lisp не управляется этим установщиком напрямую.
# Если нужно, разместите cli-script.lisp в /usr/local/lib/git-tree вручную.

# Директория для установки lisp-скриптов (используется для бинарной установки)
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

usage(){
  cat <<EOF
Usage: $0 [--install|--uninstall] [--script|--binary] [--no-source] [--help]

Options:
  --install       Install (default)
  --uninstall     Uninstall both variants
  --script        Install script (non-binary) variant
  --binary        Install binary variant
  --no-source     Do not copy source files to /usr/local (when installing)
  --help          Show this help
EOF
}

for arg in "$@"; do
  case "$arg" in
    --install)
      ACTION="install" ;;
    --uninstall)
      ACTION="uninstall" ;;
    --script)
      MODE="script" ;;
    --binary)
      MODE="binary" ;;
    --both)
      echo "Option --both is not supported; use --script and/or --binary separately"; exit 1 ;;
    --no-source)
      COPY_SOURCE=0 ;;
    --help|-h)
      usage; exit 0 ;;
    *)
      echo "Unknown option: $arg"; usage; exit 1 ;;
  esac
done

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
  TARGET="$PROJECT_DIR/sh/git-tree"
  if [ "$SYSTEM" = "linux" ] || [ "$SYSTEM" = "msys2" ]; then
    # Генерируем lisp-обёртку для обеих систем
    cat > "$TARGET" <<'EOF'
#!/usr/bin/env sbcl --script

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Тихая загрузка системы
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-git-tree :silent t))

(cl-git-tree/cli:main sb-ext:*posix-argv*)
EOF
    chmod +x "$TARGET"
    echo "ℹ️  Сгенерирован $TARGET (sbcl wrapper)"
    # Автоматически подставляем путь к sbcl в shebang, если он доступен
    SBCL_PATH="$(command -v sbcl 2>/dev/null || true)"
    if [ -n "$SBCL_PATH" ]; then
      tmpfile="$(mktemp)"
      echo "#!$SBCL_PATH --script" > "$tmpfile"
      sed '1d' "$TARGET" >> "$tmpfile"
      mv "$tmpfile" "$TARGET"
      chmod +x "$TARGET"
      echo "ℹ️  Обновлён shebang в $TARGET → $SBCL_PATH"
    else
      echo "⚠️  SBCL не найден в PATH — shebang в $TARGET оставлен без изменений"
    fi
  fi
  if [ ! -f "$TARGET" ]; then
    echo "❌ Ошибка: файл не найден: $TARGET"
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
  # CLI-скрипт не копируется этим установщиком.
  ln -sf "$TARGET" "$LINK"
  if [ $? -eq 0 ]; then
    chmod +x "$TARGET"
    echo "✅ Установка завершена!"
    echo "   Система: $SYSTEM"
    echo "   Симлинк: $LINK → $TARGET"
    echo "   CLI-скрипт: (не установлен этим скриптом)"
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
  # CLI-скрипт не управляется этим установщиком; ничего не удаляем в /usr/local/lib/git-tree
  if [ -d "$LISP_DEST_DIR" ] && [ -z "$(ls -A "$LISP_DEST_DIR")" ]; then
    rmdir "$LISP_DEST_DIR"
    echo "🗑️  Удалена директория: $LISP_DEST_DIR"
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
  # Генерируем wrapper динамически: сначала пытаемся запустить бинарник рядом с wrapper'ом,
  # затем — lisp-скрипт через найденный sbcl (подставляем путь на момент установки)
  SBCL_PATH="$(command -v sbcl 2>/dev/null || true)"

  cat > "$BIN_WRAPPER_DEST" <<'EOF'
#!/bin/bash
# Generated wrapper for cl-git-tree (binary + fallback to SBCL)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN="$SCRIPT_DIR/$(basename "$BIN_DEST")"
if [[ -x "$BIN" ]]; then
  exec "$BIN" "$@"
fi

# SBCL detected at install time
SBCL_EXEC="$SBCL_PATH"
if [ -z "$SBCL_EXEC" ]; then
  SBCL_EXEC="$(command -v sbcl 2>/dev/null || true)"
fi
CLI_SCRIPT="/usr/local/lib/git-tree/cli-script.lisp"
if [ -n "$SBCL_EXEC" ] && [ -f "$CLI_SCRIPT" ]; then
  exec "$SBCL_EXEC" --script "$CLI_SCRIPT" "$@"
fi

echo "❌ Ошибка: ни бинарник, ни SBCL+CLI-скрипт не доступны."
exit 1
EOF

  chmod +x "$BIN_WRAPPER_DEST"
  echo "✅ Установлен: $BIN_WRAPPER_DEST"
  # Всегда копируем исходник бинарника для воспроизводимости (если он есть)
  mkdir -p "$LISP_DEST_DIR"
  if [ -f "$SRC_LISP" ]; then
    cp -f "$SRC_LISP" "$LISP_DEST"
    echo "📄 Исходник git-tree-bin.lisp установлен: $LISP_DEST"
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
  if [ -f "$BIN_WRAPPER_DEST" ]; then
    rm -f "$BIN_WRAPPER_DEST"
    echo "🗑️  Удалён wrapper: $BIN_WRAPPER_DEST"
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

# Dispatcher
if [ "$ACTION" = "install" ]; then
  if [ "$MODE" = "script" ]; then
    run_script_install
  elif [ "$MODE" = "binary" ]; then
    run_bin_install
  else
    echo "Unknown mode: $MODE"; exit 1
  fi
elif [ "$ACTION" = "uninstall" ]; then
  if [ "$MODE" = "script" ]; then
    run_script_uninstall
  elif [ "$MODE" = "binary" ]; then
    run_bin_uninstall
  else
    run_script_uninstall
    run_bin_uninstall
  fi
fi
