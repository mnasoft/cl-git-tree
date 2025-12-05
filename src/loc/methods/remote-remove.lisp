;;;; ./src/loc/methods/remote-remove.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-remove ((ws <workspace>) (provider <gitlab>) &key &allow-other-keys)
  "Удалить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER.")

