#!/bin/bash
#
# Тестирование транспортных операций (export / import).
#
# Сначала вызывает setup-linked-repos.sh для подготовки двух связанных репозиториев,
# затем проверяет:
#   1. Экспорт архива из place-2/test-cl-git-tree (ветка dev)
#   2. Импорт архива в place-1/test-cl-git-tree

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Корневой каталог тестового окружения
WORK_DIR=~/work/cl-git-tree

"$SCRIPT_DIR/setup-linked-repos.sh"

# Переходим во второй каталог и экспортируем транспортный архив
cd "$WORK_DIR/place-2/test-cl-git-tree"

git tree transport export

# Переходим в первый каталог и импортируем транспортный архив
cd "$WORK_DIR/place-1/test-cl-git-tree"

git tree transport import
