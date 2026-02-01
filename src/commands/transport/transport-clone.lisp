;;;; ./src/commands/transport/transport-clone.lisp

(in-package :cl-git-tree/commands/transport)

(defun print-transport-clone-help ()
  "Справка по git-tree transport clone."
  (format t "Клонирование репозитория из tar.xz архива.~%~%")
  (format t "Использование:~%")
  (format t "  git-tree transport clone <archive.tar.xz> <location> [--help]~%~%")
  (format t "Аргументы:~%")
  (format t "  <archive.tar.xz>  Имя архива (из вывода git-tree transport list)~%")
  (format t "  <location>        Название локации для создания remote~%~%")
  (format t "Опции:~%")
  (format t "  --help            Показать эту справку~%~%")
  (format t "Описание:~%")
  (format t "  Команда извлекает bare-репозиторий из архива, клонирует его~%")
  (format t "  в текущую директорию, удаляет origin и создаёт remote для локации.~%~%")
  (format t "Примеры:~%")
  (format t "  git-tree transport clone dep_11_site.tar.xz lc~%")
  (format t "  git-tree transport clone my-repo.tar.xz gh~%"))

(defun extract-and-clone-archive (archive-name location-key)
  "Извлекает tar.xz архив и клонирует репозиторий в текущую директорию.
  
  Возвращает T при успехе, NIL при ошибке."
  (let* ((loc (cl-git-tree/loc:find-location location-key))
         (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc)))
         (ws (cl-git-tree/loc:make-workspace ".")))
    
    (unless loc
      (format t "~A Локация '~A' не найдена.~%" (cl-git-tree/loc:find-emo ws "error") location-key)
      (return-from extract-and-clone-archive nil))
    
    (unless url-xz
      (format t "~A У локации '~A' не задан :url-xz.~%" 
              (cl-git-tree/loc:find-emo ws "warn") location-key)
      (return-from extract-and-clone-archive nil))
    
    (let* ((xz-dir (uiop:ensure-directory-pathname (cl-git-tree/fs:expand-home url-xz)))
           (archive-path (merge-pathnames archive-name xz-dir))
           (bare-repo-name (cl-ppcre:regex-replace "\\.tar\\.xz$" archive-name ".git"))
           (repo-name (cl-ppcre:regex-replace "\\.git$" bare-repo-name ""))
           (bare-repo-path (merge-pathnames bare-repo-name xz-dir))
           (target-dir (merge-pathnames repo-name (uiop:getcwd))))
      
      ;; Проверка существования архива
      (unless (probe-file archive-path)
        (format t "~A Архив не найден: ~A~%" 
                (cl-git-tree/loc:find-emo ws "error") archive-path)
        (return-from extract-and-clone-archive nil))
      
      ;; Проверка, что целевой каталог не существует
      (when (probe-file target-dir)
        (format t "~A Каталог '~A' уже существует.~%" 
                (cl-git-tree/loc:find-emo ws "error") repo-name)
        (return-from extract-and-clone-archive nil))
      
      ;; Распаковка архива, если bare-репозиторий ещё не распакован
      (unless (probe-file bare-repo-path)
        (format t "~A Распаковка архива: ~A~%" 
                (cl-git-tree/loc:find-emo ws "fs archive") archive-name)
        (let ((cmd (format nil "tar -xJf ~A -C ~A" 
                          (namestring archive-path) 
                          (namestring xz-dir))))
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program cmd 
                               :output :string 
                               :error-output :string 
                               :ignore-error-status t)
            (declare (ignore output))
            (unless (zerop exit-code)
              (format t "~A Ошибка при распаковке: ~A~%" 
                      (cl-git-tree/loc:find-emo ws "error") error-output)
              (return-from extract-and-clone-archive nil)))))
      
      ;; Клонирование из bare-репозитория
      (format t "~A Клонирование репозитория: ~A~%" 
              (cl-git-tree/loc:find-emo ws "git clone") repo-name)
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list "git" "clone" (namestring bare-repo-path) repo-name)
                           :output :string
                           :error-output :string
                           :ignore-error-status t)
        (declare (ignore output))
        (unless (zerop exit-code)
          (format t "~A Ошибка клонирования: ~A~%" 
                  (cl-git-tree/loc:find-emo ws "error") error-output)
          (return-from extract-and-clone-archive nil)))
      
      ;; Переходим в клонированный репозиторий и удаляем origin
      (format t "~A Удаление origin~%" (cl-git-tree/loc:find-emo ws "git remote"))
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list "git" "-C" repo-name "remote" "remove" "origin")
                           :output :string
                           :error-output :string
                           :ignore-error-status t)
        (declare (ignore output error-output))
        (unless (zerop exit-code)
          (format t "~A Предупреждение: не удалось удалить origin~%" 
                  (cl-git-tree/loc:find-emo ws "warn"))))
      
      ;; Создаём remote через git-tree
      (format t "~A Создание remote для локации '~A'~%" 
              (cl-git-tree/loc:find-emo ws "git remote") location-key)
      (let ((repo-ws (cl-git-tree/loc:make-workspace target-dir)))
        (cl-git-tree/loc:remote-create repo-ws loc))
      
      (format t "~%~A Репозиторий успешно клонирован: ~A~%" 
              (cl-git-tree/loc:find-emo ws "success") repo-name)
      t)))

(defun transport-clone (args)
  "Клонирует репозиторий из tar.xz архива и создаёт remote для локации.

ARGS — список аргументов после слова clone."
  (let ((archive-name (first args))
        (location-key (second args)))
    
    (cond
      ((or (null args) (member "--help" args :test #'string=))
       (print-transport-clone-help))
      ((or (null archive-name) (null location-key))
       (let ((ws (cl-git-tree/loc:make-workspace ".")))
         (format t "~A Необходимо указать имя архива и локацию.~%" 
                 (cl-git-tree/loc:find-emo ws "error"))
         (format t "Справка: git-tree transport clone --help~%")))
      (t
       (extract-and-clone-archive archive-name location-key)))))
