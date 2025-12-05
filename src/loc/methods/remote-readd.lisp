;;;; ./src/loc/methods/remote-readd.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-readd ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Удалить и снова добавить отдаленный репозиторий для рабочего
пространства WORKSPACE, связанный с провайдером PROVIDER."
  (remote-remove ws provider)
  (remote-add ws provider)
  ws)
