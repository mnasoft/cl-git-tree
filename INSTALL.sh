#!/bin/bash
# Unified installer: installs/uninstalls script (non-binary) and binary variants
# Usage: INSTALL.sh [--install [--script|--binary]|--uninstall] [--help]

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
# Paths and defaults used by embedded installers
LINK_DIR="/usr/local/bin"
LINK="$LINK_DIR/git-tree"

# Binary installer paths
BIN_SRC_DIR="$PROJECT_DIR/bin"
BIN_DEST_DIR="/usr/local/bin"

BIN_SRC="$BIN_SRC_DIR/git-tree-bin.exe"
BIN_WRAPPER_SRC="$BIN_SRC_DIR/git-tree"

BIN_DEST="$BIN_DEST_DIR/git-tree-bin.exe"
BIN_WRAPPER_DEST="$BIN_DEST_DIR/git-tree"

# Default ACTION & MODE
ACTION="install"
MODE="script"

# Test output
test_output() {
    echo "##################################################"    
    echo "PROJECT_DIR     =$PROJECT_DIR"
    echo "LINK_DIR        =$LINK_DIR"
    echo "LINK            =$LINK"
    echo "BIN_SRC_DIR     =$BIN_SRC_DIR"
    echo "BIN_DEST_DIR    =$BIN_DEST_DIR"
    echo "BIN_SRC         =$BIN_SRC"
    echo "BIN_WRAPPER_SRC =$BIN_WRAPPER_SRC"
    echo "BIN_DEST        =$BIN_DEST"
    echo "BIN_WRAPPER_DEST=$BIN_WRAPPER_DEST"
    echo "ACTION          =$ACTION"
    echo "MODE            =$MODE"
    echo "##################################################"
}

test_output

##################################################
#### functions

usage() {
  cat <<EOF
Usage: $0 [--install [--script|--binary]|--uninstall] [--help]

Options:
  --install       Install (default); specify --script or --binary
  --uninstall     Uninstall both variants
  --script        Use script (non-binary) variant (with --install)
  --binary        Use binary variant (with --install)
  --help          Show this help
EOF
}

detect_system() {
  if [ -n "$MSYSTEM" ] || uname -o | grep -iq "msys\|mingw\|cygwin"; then
    echo "msys2"
  else
    echo "linux"
  fi
}

ensure_sudo() {
    if [ "$SYSTEM" = "linux" ] && [ "$EUID" -ne 0 ]
    then
        echo "⚠️  Нужен sudo для записи в /usr/local/"
        if command -v bash >/dev/null 2>&1; then
            exec sudo bash "$0" "$@"
        else
            exec sudo "$0" "$@"
        fi
    fi
}

install_link() {
    ensure_sudo "$@"
    TARGET="$PROJECT_DIR/bin/git-tree-script"
    if [ "$SYSTEM" = "linux" ] || [ "$SYSTEM" = "msys2" ]
    then
        # Генерируем bash-обёртку для обеих систем
        sudo -u "$SUDO_USER" sh build-src.sh

#        chmod +x "$TARGET"
#        chmod +r "$TARGET"
        echo "ℹ️  Сгенерирован $TARGET (sbcl wrapper)"
    fi
    if [ ! -f "$TARGET" ]; then
        echo "❌ Ошибка: файл не найден: $TARGET"
        exit 1
    fi
    if [ ! -d "$LINK_DIR" ]
    then
        mkdir -p "$LINK_DIR"
        echo "📁 Директория создана: $LINK_DIR"
    fi
    ln -sf "$TARGET" "$LINK"
    if [ $? -eq 0 ]
    then
        chmod +x "$TARGET"
        echo "✅ Установка завершена!"
        echo "   Система: $SYSTEM"
        echo "   Симлинк: $LINK → $TARGET"
        echo "   Используйте: git-tree --help"
    else
        echo "❌ Ошибка создания симлинка"
        exit 1
    fi
}

uninstall_link() {
    if [ "$SYSTEM" = "linux" ]
    then
        ensure_sudo "$@"
    fi
    if [ -L "$LINK" ] || [ -f "$LINK" ]
    then
        rm -f "$LINK"
        echo "🗑️  Удалено: $LINK"
    else
        echo "⚠️  Ссылка не найдена: $LINK"
    fi
}

install_bin() {
    ensure_sudo "$@"
    sudo -u "$SUDO_USER" sh build-bin.sh 
    if [ ! -f "$BIN_SRC" ]
    then
        echo "❌ Не найден бинарник: $BIN_SRC"
        exit 1
    fi
    if [ ! -f "$BIN_WRAPPER_SRC" ]
    then
        echo "❌ Не найден обёрточный скрипт: $BIN_WRAPPER_SRC"
        exit 1
    fi
    if [ -L "$BIN_DEST" ] || [ -f "$BIN_DEST" ]
    then
        rm -f "$BIN_DEST"
        echo "🗑️  Удалён старый бинарник/ссылка: $BIN_DEST"
    fi
    cp -f "$BIN_SRC" "$BIN_DEST"
    chmod +x "$BIN_DEST"
    echo "✅ Установлен: $BIN_DEST"
    if [ -L "$BIN_WRAPPER_DEST" ] || [ -f "$BIN_WRAPPER_DEST" ]
    then
        rm -f "$BIN_WRAPPER_DEST"
        echo "🗑️  Удалён старый скрипт/ссылка: $BIN_WRAPPER_DEST"
    fi
    # Копируем существующий wrapper из bin/git-tree
    cp -f "$BIN_WRAPPER_SRC" "$BIN_WRAPPER_DEST"
    chmod +x "$BIN_WRAPPER_DEST"
    echo "✅ Установлен: $BIN_WRAPPER_DEST"
}

uninstall_bin() {
    ensure_sudo "$@"
    if [ -f "$BIN_DEST" ]
    then
        rm -f "$BIN_DEST"
        echo "🗑️  Удалён: $BIN_DEST"
    else
        echo "⚠️  Не найден: $BIN_DEST"
    fi
    if [ -f "$BIN_WRAPPER_DEST" ]
    then
        rm -f "$BIN_WRAPPER_DEST"
        echo "🗑️  Удалён wrapper: $BIN_WRAPPER_DEST"
    fi
}

run_script_install() {
    install_link
}

run_bin_install() {
    install_bin
}

run_script_uninstall() {
    uninstall_link
}

run_bin_uninstall() {
    uninstall_bin
}

##################################################
#### Разбор аргументов

for arg in "$@"
do
    case "$arg" in
        --install)
            ACTION="install" ;;
        --uninstall)
            ACTION="uninstall" ;;
        --script)
            MODE="script" ;;
        --binary)
            MODE="binary" ;;
        --help|-h)
            usage; exit 0 ;;
        *)
            echo "Unknown option: $arg"
            usage
            exit 1 ;;
    esac
done

SYSTEM=$(detect_system)

# Dispatcher
if [ "$ACTION" = "install" ]
then
    if [ "$MODE" = "script" ]
    then
        run_script_install
    elif [ "$MODE" = "binary" ]
    then
        run_bin_install
    else
        echo "Unknown mode: $MODE"
        exit 1
    fi
elif [ "$ACTION" = "uninstall" ]
then
    run_script_uninstall
    run_bin_uninstall
fi
