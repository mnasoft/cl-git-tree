(in-package :cl-git-tree)

(defparameter *config-path*
  (merge-pathnames #p".git-tree/locations.lisp"
                   (user-homedir-pathname))
  "Путь к файлу конфигурации локаций (~/.git-tree/locations.lisp).")

(defparameter *config-example*
"(in-package :cl-git-tree/loc)

;; GitHub — удалённые публичные репозитории
(add-location \"gh\"
    :url-git \"git@github.com:mnasoft\"
    :url-xz NIL
    :tar NIL
    :description \"GitHub remote repositories (public)\"
    :provider :GITHUB)

;; Local — локальное зеркало репозиториев
(add-location \"lc\"
    :url-git \"~/.git-tree/git/lc\"
    :url-xz \"~/.git-tree/xz/lc\"
    :tar \"lc.tar\"
    :description \"Local mirror of repositories\"
    :provider :LOCAL)
"
)

(defun load-config ()
  "Загружает конфигурацию из locations.lisp или создаёт шаблон, если файл
отсутствует."
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
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format *error-output* "~A  Ошибка загрузки конфигурации ~A: ~A~%" 
                (cl-git-tree/loc:find-emo ws "warning")
                *config-path* e)))))

(defun reset-config ()
  "Перезаписывает конфигурационный файл дефолтными значениями."
  (ensure-directories-exist (path:dirname *config-path*))
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~A~%" *config-example*))
  (let ((ws (cl-git-tree/loc:make-workspace ".")))
    (format t "~A Конфигурация сброшена: ~A~%" 
            (cl-git-tree/loc:find-emo ws "success")
            *config-path*)))
