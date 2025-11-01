(in-package :cl-git-tree)

(defparameter *config-path*
  (merge-pathnames #p".git-tree/locations.configure"
                   (user-homedir-pathname))
  "Путь к файлу конфигурации локаций git-tree.

Файл хранится в домашнем каталоге пользователя, в подкаталоге
\".git-tree/locations.configure\". В нём описываются все доступные
локации (ключи, базовые URL и прочие параметры), которые затем
используются подсистемами LOC и FS.

Примеры:
  *CONFIG-PATH*
  ;; => #P\"/home/user/.git-tree/locations.configure\"

  (probe-file *config-path*)
  ;; => #P\"/home/user/.git-tree/locations.configure\" или NIL, если файл отсутствует.")

(defparameter *config-example*
"
;; #!/bin/sbcl --script
;; Подключаем Quicklisp
;; (load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))
;; (ql:quickload :cl-git-tree)

(in-package :cl-git-tree)

;; GitHub — шаблон для публичных проектов
(add-location \"gh\"
              :name    \"GitHub projects\"
              :url-git \"git@github.com:mnasoft\")

;; pp  — шаблон для публичных проектов
(add-location \"pp\"
              :name    \"local public pp\"
              :url-git \"/home/mna/.git-tree/git/pp\"
              :url-xz  \"/home/mna/.git-tree/xz/pp\"
              :tar     \"pp.tar\")

;; pz  — шаблон для закрытых проектов
(add-location \"pp\"
              :name    \"local private pz\"
              :url-git \"/home/mna/.git-tree/git/pp\"
              :url-xz  \"/home/mna/.git-tree/xz/pp\"
              :tar     \"pz.tar\")

;; pz — шаблон для закрытых проектов
(add-location \"pz\"
              :name    \"local private pz\"
              :url-git \"/home/mna/.git-tree/git/pz\"
              :url-xz  \"/home/mna/.git-tree/xz/pz\"
              :tar     \"pz.tar\")
"
)

(defun load-config ()
  "Загружает конфигурацию из locations.conf или создаёт шаблон, если файл отсутствует."
  (unless (probe-file *config-path*)
    (ensure-directories-exist (path:dirname *config-path*))
    (with-open-file (s *config-path*
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format s "~S" *config-example*)))
  (load *config-path*))
