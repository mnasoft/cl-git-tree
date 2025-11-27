;;;; ./src/config/defaults.lisp

(in-package :cl-git-tree/config)

(defparameter *tracked-patterns*
  '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")
  "Шаблонные и точные паттерны файлов для включения по умолчанию.")

(defparameter *excludes-patterns*
  '("./.git" "./build")
  "Каталоги/паттерны для исключения по умолчанию.")
