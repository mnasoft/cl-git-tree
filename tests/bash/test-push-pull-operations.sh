#!/bin/bash
#
# Тестирование push/pull операций.
#
# Сначала вызывает setup-linked-repos.sh для подготовки двух связанных репозиториев,
# затем проверяет:
#   1. Push из place-2/test-cl-git-tree (ветка dev)
#   2. Pull в place-1/test-cl-git-tree (обновление веток)
#   3. Сравнение состояния веток dev и master/main в обоих репозиториях

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Корневой каталог тестового окружения
WORK_DIR=~/work/cl-git-tree

"$SCRIPT_DIR/setup-linked-repos.sh"

echo "=== Состояние ДО push/pull операций ==="
echo "=== Place-1 ветки ==="
cd "$WORK_DIR/place-1/test-cl-git-tree"
git branch -a
git log --oneline -n 3

echo "=== Place-2 ветки ==="
cd "$WORK_DIR/place-2/test-cl-git-tree"
git branch -a
git log --oneline -n 3

# Переходим в place-2 и делаем push всех веток
echo "=== Выполняем push всех веток из place-2 ==="
cd "$WORK_DIR/place-2/test-cl-git-tree"
git tree push

# Переходим в place-1 и делаем pull всех веток
echo "=== Выполняем pull всех веток в place-1 ==="
cd "$WORK_DIR/place-1/test-cl-git-tree"
git tree pull

# Сравнение состояния после операций
echo "=== Состояние ПОСЛЕ push/pull операций ==="
echo "=== Place-1 ветки ==="
cd "$WORK_DIR/place-1/test-cl-git-tree"
git branch -a
git log --oneline -n 3

echo "=== Place-2 ветки ==="
cd "$WORK_DIR/place-2/test-cl-git-tree"
git branch -a
git log --oneline -n 3

echo "=== Сравнение: все ветки и их история должны быть синхронизированы между place-1 и place-2 ==="
