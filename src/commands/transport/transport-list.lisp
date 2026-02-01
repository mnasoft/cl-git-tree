;;;; ./src/commands/transport/transport-list.lisp

(in-package :cl-git-tree/commands/transport)

(defun print-transport-list-help ()
  "Справка по git-tree transport list."
  (format t "Вывод список tar.xz архивов для локации.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport list <location> [--help]~%~%")
  (format t "Аргументы:~%")
  (format t "  <location>   Название локации (ключ)~%~%")
  (format t "Опции:~%")
  (format t "  --help       Показать эту справку~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree transport list local~%")
  (format t "  git-tree transport list gh~%"))

(defun list-tar-xz-archives (location-key)
  "Выводит список tar.xz архивов для локации location-key.
  
  Возвращает количество найденных архивов."
  (let* ((loc (cl-git-tree/loc:find-location location-key))
         (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc))))
    
    (unless loc
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format t "~A Локация '~A' не найдена.~%" (cl-git-tree/loc:find-emo ws "error") location-key))
      (return-from list-tar-xz-archives 0))
    
    (unless url-xz
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format t "~A У локации '~A' не задан :url-xz.~%" 
                (cl-git-tree/loc:find-emo ws "warn") location-key))
      (return-from list-tar-xz-archives 0))
    
    (let* ((xz-dir (uiop:ensure-directory-pathname (cl-git-tree/fs:expand-home url-xz)))
           (pattern (merge-pathnames #p"*.tar.xz" xz-dir))
           (archives (sort (directory pattern) #'string< :key #'namestring))
           (count (length archives)))
      
      (if archives
          (progn
            (format t "~A Архивы tar.xz для локации '~A' (~A):~%" 
                    (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "fs archive")
                    location-key count)
            (format t "   Каталог: ~A~%~%" xz-dir)
            (dolist (file archives)
              (let* ((size (with-open-file (stream file :direction :input)
                             (file-length stream)))
                     (size-mb (/ size 1048576.0)))
                (format t "   • ~A (~5,1F MB)~%" 
                        (file-namestring file) 
                        size-mb)))
            (format t "~%=== Всего архивов: ~A ===~%" count))
          (let ((ws (cl-git-tree/loc:make-workspace ".")))
            (format t "~A Архивы tar.xz для локации '~A' не найдены.~%" 
                    (cl-git-tree/loc:find-emo ws "warn")
                    location-key)
            (format t "   Каталог: ~A~%" xz-dir)))
      
      count)))

(defun transport-list (args)
  "Выводит список tar.xz-архивов для заданной локации.

ARGS — список аргументов после слова list."
  (let ((location-key (first args)))
    
    (cond
      ((or (null args) (member "--help" args :test #'string=))
       (print-transport-list-help))
      ((null location-key)
       (let ((ws (cl-git-tree/loc:make-workspace ".")))
         (format t "~A Не задана локация.~%" (cl-git-tree/loc:find-emo ws "error"))
         (format t "Справка: git-tree transport list --help~%")))
      (t
       (list-tar-xz-archives location-key)))))
