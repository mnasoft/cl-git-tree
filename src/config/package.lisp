;;;; ./src/config/package.lisp

(defpackage :cl-git-tree/config
  (:use :cl)
  (:export *tracked-patterns*
           *excludes-patterns*
           *file-patterns*
           *patterns-path*
           tracked-patterns
           excluded-patterns
           load-patterns
           save-patterns
           get-tracked-patterns
           get-excluded-patterns
           add-tracked-pattern
           remove-tracked-pattern
           add-excluded-pattern
           remove-excluded-pattern
           reset-to-defaults
           list-all-patterns))

(in-package :cl-git-tree/config)
