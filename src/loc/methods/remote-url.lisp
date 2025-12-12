;;;; ./src/loc/methods/remote-url.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-url ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WORKSPACE на провайдере PROVIDER.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url (cl-git-tree/loc:<location>-url-git provider))
         ;; Определить нужный разделитель в зависимости от типа URL
         (separator (cond
                      ;; SSH URLs (содержат @) — используем /
                      ((search "@" base-url) "/")
                      ;; HTTP(S) — используем /
                      ((uiop:string-prefix-p "http" base-url) "/")
                      ;; Локальные пути — используем /
                      (t "/")))
         ;; Убедимся, что base-url заканчивается на разделитель
         (normalized-url (if (or (uiop:string-suffix-p "/" base-url)
                                 (uiop:string-suffix-p separator base-url))
                             base-url
                             (concatenate 'string base-url separator))))
    (format nil "~A~A.git" normalized-url repo-name)))
