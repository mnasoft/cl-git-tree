;;;; ./src/loc/methods/remote-url.lisp

(in-package :cl-git-tree/loc)

(defmethod remote-url ((ws <workspace>) (provider <provider>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WORKSPACE на провайдере PROVIDER.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url (cl-git-tree/loc:<location>-url-git provider)))
    ;; base-url уже нормализирован в add-location, просто конкатенируем с repo-name
    (format nil "~A~A.git" base-url repo-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Утилита: раскрыть путь с "~" в абсолютный путь к домашнему каталогу.
;; Пример: "~/.git-tree/git/lc/" -> "<HOME>/.git-tree/git/lc/"
(defun expand-tilde-directory-path (path)
  "Раскрывает префикс ~ в начале PATH и возвращает полный путь-строку (namestring).
Если PATH не начинается с ~, возвращает нормализованный namestring директории."
  (let* ((home-path (uiop:ensure-directory-pathname (uiop:getenv "HOME")))
         (str (if (pathnamep path) (namestring path) path)))
    (if (and str (> (length str) 0) (char= (char str 0) #\~))
        (let* ((rest (subseq str 1))
               (rel (if (and (> (length rest) 0)
                              (member (char rest 0) '(#\/ #\\)))
                         (subseq rest 1)
                         rest))
               (merged (merge-pathnames (uiop:parse-unix-namestring rel) home-path)))
          (namestring (uiop:ensure-directory-pathname merged)))
        (namestring (uiop:ensure-directory-pathname str)))))

(defmethod remote-url ((ws <workspace-msys2>) (provider <local>) &key &allow-other-keys)
  "Построить URL удалённого репозитория для WS типа <workspace-msys2> на
провайдере PROVIDER типа <local>.
Возвращает строку с полным git-URL репозитория."
  (let* ((repo-name (repo-name ws))
         (base-url-expanded (expand-tilde-directory-path (cl-git-tree/loc:<location>-url-git provider))))
    ;; конкатенируем развернутую базу с именем репо
    (format nil "~A~A.git" base-url-expanded repo-name)))
