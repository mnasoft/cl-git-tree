(defpackage :cl-git-tree/fs
  (:use :cl)
  (:export
   find-git-repos
   repo-name
   git-repo-p
   with-repo
   ;; with-each-repo
   ;; with-each-repo-simple
   )
  (:documentation
   "Подсистема работы с файловой системой для cl-git-tree.

Экспортируемые функции:

  FIND-GIT-REPOS
    Рекурсивно ищет все git‑репозитории, начиная с указанного каталога.

  REPO-NAME
    Возвращает имя каталога репозитория (basename пути).

  GIT-REPO-P
    Проверяет, является ли каталог git‑репозиторием (наличие .git/).

  WITH-EACH-REPO
    Вызывает переданную функцию для каждого найденного репозитория,
    передавая (repo-dir loc-key base-url).

  WITH-EACH-REPO-SIMPLE
    Упрощённый вариант: вызывает функцию только с (repo-dir).

Назначение:
  Модуль инкапсулирует обход файловой системы и поиск git‑репозиториев,
  предоставляя единый API для команд верхнего уровня (clone, pull, push и т.д.)."))

(in-package :cl-git-tree/fs)
