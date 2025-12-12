(in-package :cl-git-tree)

(defparameter *config-path*
  (merge-pathnames #p".git-tree/locations.lisp"
                   (user-homedir-pathname))
  "Путь к файлу конфигурации локаций (~/.git-tree/locations.lisp).")

(defparameter *config-example*
"(in-package :cl-git-tree/loc)

(add-location \"gh\"
    :url-git \"git@github.com:mnasoft\"
    :url-xz NIL
    :tar NIL
    :description \"GitHub — шаблон для публичных проектов\"
    :provider :GITHUB)

(add-location \"lc\"
    :url-git \"~/.git-tree/git/lc\"
    :url-xz \"~/.git-tree/xz/lc\"
    :tar \"lc.tar\"
    :description \"Local mirror\"
    :provider :LOCAL)
"
)

(defun load-config ()
  "Загружает конфигурацию из locations.lisp или создаёт шаблон, если файл отсутствует."
  (unless (probe-file *config-path*)
    (ensure-directories-exist (path:dirname *config-path*))
    (with-open-file (s *config-path*
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format s "~A~%" *config-example*)))
  (handler-case
      (load *config-path*)
    (error (e)
      (format *error-output* "⚠️  Ошибка загрузки конфигурации ~A: ~A~%" *config-path* e))))

(defun reset-config ()
  "Перезаписывает конфигурационный файл дефолтными значениями."
  (ensure-directories-exist (path:dirname *config-path*))
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~A~%" *config-example*))
  (format t "✅ Конфигурация сброшена: ~A~%" *config-path*))
