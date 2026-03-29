#!/bin/bash
#
# Подготовка окружения для тестовых операций.
#
# Создаёт два связанных git-репозитория с общей историей:
#   place-1/test-cl-git-tree  — исходный репозиторий (main-ветка)
#   place-2/test-cl-git-tree  — клон с веткой dev и новым коммитом

set -e

# Корневой каталог тестового окружения
WORK_DIR=~/work/cl-git-tree

# Удаляем каталог тестирования
rm -rf "$WORK_DIR"

# Создаем каталоги тестирования
mkdir -p "$WORK_DIR/place-1"
mkdir -p "$WORK_DIR/place-2"

# Переходим в первый каталог
cd "$WORK_DIR/place-1"

# Создаем каталог для тестового репо 1
mkdir test-cl-git-tree

# Переходим в каталог для тестового репо 1
cd test-cl-git-tree

# Создаем тестовый файл
echo "place 1" > README.org

# Инициализируем пустой репо
git init

# Добавляем файл README.org в репо 1
git add README.org

# Делаем первый коммит
git commit -am "Initial"

# Создаем отдаленный репо
git tree remote create lc
git tree remote delete lc
git tree remote create lc

# Переходим во второй каталог
cd "$WORK_DIR/place-2"

# Клонируем репо во второй каталог
git clone ~/.git-tree/git/lc/test-cl-git-tree.git/

# Переходим в каталог для тестового репо 2
cd test-cl-git-tree

# Переименовываем origin в lc
git remote rename origin lc

# Создаем файл RM.org в репо 2
echo "place 2" > RM.org

# Создаем ветку dev
git branch -c dev

# Создаем переключаемся на ветку dev
git switch dev

# Добавляем файл RM.org в репо 1
git add RM.org

# Делаем коммит на ветке dev
git commit -am "dev"
