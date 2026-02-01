;;;; ./src/commands/transport/transport-list-git.lisp

(in-package :cl-git-tree/commands/transport)

(defun print-transport-list-git-help ()
  "Справка по git-tree transport list-git."
  (format t "Вывод списка git-репозиториев в каталоге :url-xz/<location>/.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport list-git <location> [--help]~%~%")
  (format t "Аргументы:~%")
  (format t "  <location>   Название локации (ключ)~%~%")
  (format t "Опции:~%")
  (format t "  --help       Показать эту справку~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree transport list-git local~%")
  (format t "  git-tree transport list-git gh~%"))

(defun list-git-repos (location-key)
  "Выводит список *.git/ каталогов для локации location-key (только имена).
  
  Возвращает количество найденных репозиториев."
  (let* ((loc (cl-git-tree/loc:find-location location-key))
         (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc))))
    
    (unless loc
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format t "~A Локация '~A' не найдена.~%" (cl-git-tree/loc:find-emo ws "error") location-key))
      (return-from list-git-repos 0))
    
    (unless url-xz
      (let ((ws (cl-git-tree/loc:make-workspace ".")))
        (format t "~A У локации '~A' не задан :url-xz.~%" 
                (cl-git-tree/loc:find-emo ws "warn") location-key))
      (return-from list-git-repos 0))
    
    (let* ((git-dir (uiop:ensure-directory-pathname (cl-git-tree/fs:expand-home url-xz)))
           (git-repos (if (probe-file git-dir)
                         (loop for entry in (cl-fad:list-directory git-dir)
                               when (and (cl-fad:directory-exists-p entry)
                                        (cl-ppcre:scan "\\.git/?$" (namestring entry)))
                               collect entry)
                         nil))
           (sorted-repos (sort git-repos #'string< :key #'namestring))
           (count (length sorted-repos)))
      
      (if sorted-repos
          (progn
            (format t "~A Git-репозитории для локации '~A' (~A):~%" 
                    (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "fs archive")
                    location-key count)
            (format t "   Каталог: ~A~%~%" git-dir)
            (dolist (repo sorted-repos)
              (let ((repo-name (file-namestring (cl-ppcre:regex-replace-all "/$" (namestring repo) ""))))
                (format t "   • ~A~%" repo-name)))
            (format t "~%=== Всего репозиториев: ~A ===~%" count))
          (let ((ws (cl-git-tree/loc:make-workspace ".")))
            (format t "~A Git-репозитории для локации '~A' не найдены.~%" 
                    (cl-git-tree/loc:find-emo ws "warn")
                    location-key)
            (format t "   Каталог: ~A~%" git-dir)))
      
      count)))

(defun transport-list-git (args)
  "Выводит список *.git/ каталогов для заданной локации (только имена).

ARGS — список аргументов после слова list-git."
  (let ((location-key (first args)))
    
    (cond
      ((or (null args) (member "--help" args :test #'string=))
       (print-transport-list-git-help))
      ((null location-key)
       (let ((ws (cl-git-tree/loc:make-workspace ".")))
         (format t "~A Не задана локация.~%" (cl-git-tree/loc:find-emo ws "error"))
         (format t "Справка: git-tree transport list-git --help~%")))
      (t
       (list-git-repos location-key)))))
