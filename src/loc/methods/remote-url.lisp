;;;; ./src/loc/methods/remote-url.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-url ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WORKSPACE на провайдере PROVIDER.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url (cl-git-tree/loc:<location>-url-git provider)))
    ;; base-url уже нормализирован в add-location, просто конкатенируем с repo-name
    (format nil "~A~A.git" base-url repo-name)))
