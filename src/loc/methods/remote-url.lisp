;;;; ./src/loc/methods/remote-url.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-url ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WORKSPACE на провайдере PROVIDER.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url (cl-git-tree/loc:<location>-url-git provider)))
    ;; base-url уже нормализирован в add-location, просто конкатенируем с repo-name
    (format nil "~A~A.git" base-url repo-name)))

(defmethod remote-url ((ws <workspace-msys2>) (provider <local>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WS типа <workspace-msys2> на
провайдере PROVIDER типа <local>.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url-expanded (cl-git-tree/fs:expand-home (cl-git-tree/loc:<location>-url-git provider))))
    ;; конкатенируем развернутую базу с именем репо
    (format nil "~A~A.git" base-url-expanded repo-name)))
