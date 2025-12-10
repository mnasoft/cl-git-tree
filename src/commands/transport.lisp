;;;; ./src/commands/transport.lisp

(defpackage :cl-git-tree/commands/transport
  (:use :cl)
  (:export cmd-transport))

(in-package :cl-git-tree/commands/transport)

(defun repo-is-clean-p (repo-dir)
  "Проверяет, что репозиторий чист (нет незакоммиченных изменений)."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "status" "--short")
    (declare (ignore err))
    (and (zerop code) (string= out ""))))

(defun repo-last-commit-date (repo-dir)
  "Возвращает дату последнего коммита в формате Unix timestamp."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "log" "-1" "--format=%ct")
    (declare (ignore err))
    (when (zerop code)
      (let ((timestamp-str (string-trim '(#\Space #\Newline #\Return) out)))
        (when (> (length timestamp-str) 0)
          (parse-integer timestamp-str :junk-allowed t))))))

(defun days-since-last-commit (repo-dir)
  "Возвращает количество дней с последнего коммита."
  (let ((last-commit-ts (repo-last-commit-date repo-dir)))
    (when last-commit-ts
      (let* ((now (get-universal-time))
             (unix-epoch 2208988800) ; разница между Unix epoch и Common Lisp epoch
             (now-unix (- now unix-epoch))
             (diff-seconds (- now-unix last-commit-ts))
             (days (floor diff-seconds 86400)))
        days))))

(defun get-repo-providers (repo-dir)
  "Определяет ВСЕ провайдеров репозитория по его remotes."
  (let ((providers nil)
        (remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
    (dolist (remote remotes)
      (multiple-value-bind (out err code)
          (cl-git-tree/git-utils:git-run repo-dir "remote" "get-url" remote)
        (declare (ignore err))
        (when (zerop code)
          (let ((url (string-trim '(#\Space #\Newline #\Return) out)))
            ;; Проверяем, соответствует ли URL какой-либо зарегистрированной локации
            (dolist (loc-key (cl-git-tree/loc:all-location-keys))
              (let ((loc (cl-git-tree/loc:find-location loc-key)))
                (when (and loc (cl-git-tree/loc:<location>-url-git loc))
                  (let ((base-url (cl-git-tree/loc:<location>-url-git loc)))
                    (when (search base-url url)
                      (let ((provider (cl-git-tree/loc:<location>-provider loc)))
                        (unless (member provider providers)
                          (push provider providers))))))))))))
    providers))

(defun create-tar-xz-archive (repo-dir output-path)
  "Создаёт tar.xz архив репозитория в указанном месте.
   Архив содержит только голый git-репозиторий (--bare, без рабочих файлов)."
  (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
         (archive-name (format nil "~A.tar.xz" repo-name))
         ;; Раскрываем output-path в случае, если там есть тильда
         (expanded-output-path (cl-git-tree/loc:expand-home output-path))
         (archive-path (merge-pathnames archive-name expanded-output-path))
         (bare-name (concatenate 'string repo-name ".git"))
         (temp-dir (uiop:ensure-directory-pathname
                     (merge-pathnames (make-pathname :directory (list :relative (format nil "tmp-git-tree-~A" (random 1000000))))
                                      (uiop:temporary-directory)))))
    (ensure-directories-exist expanded-output-path)
    
    ;; Создаём голый клон в временной директории
    (multiple-value-bind (out1 err1 code1)
        (uiop:run-program
         (list "git" "clone" "--bare" (namestring repo-dir) (namestring (merge-pathnames bare-name temp-dir)))
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore out1))
      
      (if (zerop code1)
          (progn
            ;; Архивируем голый репозиторий
            (multiple-value-bind (out err code)
                (uiop:run-program
                 (list "tar" "-C" (namestring temp-dir) 
                       "-cJf" (namestring archive-path)
                       bare-name)
                 :output :string
                 :error-output :string
                 :ignore-error-status t)
              (declare (ignore out))
              
              ;; Очищаем временный каталог
              (uiop:delete-directory-tree temp-dir :validate t)
              
              (if (zerop code)
                  (progn
                    (format t "✔ Архив создан: ~A → ~A~%" archive-name (namestring expanded-output-path))
                    t)
                  (progn
                    (format t "❌ Ошибка при архивировании:~%~A~%" err)
                    nil))))
          (progn
            ;; Очищаем временный каталог при ошибке
            (ignore-errors (uiop:delete-directory-tree temp-dir :validate t))
            (format t "❌ Ошибка при создании голого клона:~%~A~%" err1)
            nil)))))

(defun clean-tar-xz-archives (output-path)
  "Удаляет tar.xz архивы в каталоге output-path."
    (let* ((pattern (merge-pathnames #p"*.tar.xz" output-path))
      (archives (directory pattern))
         (deleted 0))
    (if archives
        (progn
          (dolist (file archives)
            (when (probe-file file)
              (delete-file file)
              (incf deleted)))
          (format t "🧹 Удалено архивов: ~A (путь ~A)~%" deleted output-path))
        (format t "Архивы не найдены в ~A~%" output-path))
    deleted))

(defun apply-tar-xz-archive (archive-path dest-root)
  "Распаковывает bare-архив в целевую директорию dest-root, заменяя существующий mirror."
  (let* ((expanded-archive (cl-git-tree/loc:expand-home archive-path))
         (expanded-dest-root (uiop:ensure-directory-pathname (cl-git-tree/loc:expand-home dest-root)))
         (archive-name (file-namestring expanded-archive))
         (repo-name (if (and archive-name (>= (length archive-name) 7)
                             (string= ".tar.xz" (subseq archive-name (- (length archive-name) 7))))
                        (subseq archive-name 0 (- (length archive-name) 7))
                        archive-name))
         (bare-name (concatenate 'string repo-name ".git"))
         (dest-path (merge-pathnames bare-name expanded-dest-root))
         (temp-dir (uiop:ensure-directory-pathname
                     (merge-pathnames (make-pathname :directory (list :relative (format nil "tmp-git-tree-~A" (random 1000000))))
                                      (uiop:temporary-directory)))))
    (ensure-directories-exist expanded-dest-root)
    (format t "⬇ Распаковка ~A → ~A~%" archive-name (namestring dest-path))
    (multiple-value-bind (out err code)
        (uiop:run-program
         (list "tar" "-C" (namestring temp-dir) "-xJf" (namestring expanded-archive))
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore out))
      (if (zerop code)
          (let ((extracted (merge-pathnames bare-name temp-dir)))
            (if (probe-file extracted)
                (progn
                  (when (probe-file dest-path)
                    (uiop:delete-directory-tree dest-path :validate t))
                  (ensure-directories-exist expanded-dest-root)
                  (rename-file extracted dest-path)
                  (uiop:delete-directory-tree temp-dir :validate t)
                  (format t "✔ Импортировано: ~A~%" (namestring dest-path))
                  t)
                (progn
                  (uiop:delete-directory-tree temp-dir :validate t)
                  (format t "❌ Ошибка: в архиве не найден каталог ~A~%" bare-name)
                  nil)))
          (progn
            (uiop:delete-directory-tree temp-dir :validate t)
            (format t "❌ Ошибка распаковки:~%~A~%" err)
            nil)))))

(defun cmd-transport (&rest args)
  "CLI-команда: архивирует чистые репозитории в tar.xz или очищает каталоги с архивами.
  
  Опции:
    --provider PROVIDER  - фильтр по провайдеру (например, :local, :github)
    --days N             - архивировать только репозитории, обновлённые не позднее N дней назад (по умолчанию 30)
    --output PATH        - путь для архивов/очистки (по умолчанию ~/.git-tree/xz/)
    --help               - показать эту справку"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Архивирует чистые git-репозитории в формате tar.xz, импортирует или очищает архивы.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree transport [--days N]~%")
     (format t "  git-tree transport apply~%")
     (format t "  git-tree transport clean [--output PATH]~%~%")
     (format t "Опции:~%")
     (format t "  --days N             Архивировать только репозитории с коммитами не старее N дней (по умолчанию 30)~%")
     (format t "  --help               Показать эту справку~%~%")
     (format t "Примечание:~%")
     (format t "  Архивы создаются и импортируются для каждого локального провайдера в папки :url-xz и :url-git.~%")
     (format t "  Если :url-xz = NIL, архивирование или импорт для этого провайдера пропускается.~%~%")
     (format t "Примеры:~%")
     (format t "  git-tree transport --days 30~%")
     (format t "  git-tree transport apply~%")
     (format t "  git-tree transport~%")
     (format t "  git-tree transport clean --output /tmp/archives/~%"))
    ((and args (string= (first args) "clean"))
     (let ((output-path (merge-pathnames #p".git-tree/xz/" (user-homedir-pathname))))
       (loop for (arg val) on (rest args) by #'cddr
             do (when (string= arg "--output")
                  (setf output-path (uiop:ensure-directory-pathname val))))
       (format t "🧹 Очистка архива в каталоге ~A~%" output-path)
       (clean-tar-xz-archives output-path)))
    ((and args (string= (first args) "apply"))
     (let ((processed 0)
           (applied 0))
       (format t "⬇ Импорт архивов tar.xz из :url-xz в :url-git для всех локальных локаций~%~%")
       (dolist (loc-key (cl-git-tree/loc:all-location-keys))
         (let* ((loc (cl-git-tree/loc:find-location loc-key))
                (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc)))
                (url-git (and loc (cl-git-tree/loc:<location>-url-git loc)))
                (provider (and loc (cl-git-tree/loc:<location>-provider loc))))
           (when (and loc url-xz url-git)
             (let* ((xz-dir (uiop:ensure-directory-pathname (cl-git-tree/loc:expand-home url-xz)))
                    (archives (directory (merge-pathnames #p"*.tar.xz" xz-dir))))
               (when archives
                 (format t "Локация ~A (провайдер ~A)~%" loc-key provider)
                 (dolist (archive archives)
                   (incf processed)
                   (format t "  • ~A~%" (namestring archive))
                   (if (apply-tar-xz-archive archive url-git)
                       (incf applied)
                       (format t "    ⚠️  Пропущено из-за ошибки~%"))))))))
       (format t "~%=== Итог импорта ===~%")
       (format t "Обработано архивов: ~A~%" processed)
       (format t "Импортировано: ~A~%" applied)))
    (t
         (let ((days-filter 30)
           (processed 0)
           (archived 0))
       
       ;; Парсим аргументы
       (loop for (arg val) on args by #'cddr
             do (when (string= arg "--days")
                  (setf days-filter (parse-integer val :junk-allowed t))))
       
       (format t "🔍 Поиск репозиториев для архивирования...~%")
       (format t "   Фильтр по дате: не старее ~A дней~%" days-filter)
       (format t "   Источники: локальные провайдеры с установленным :url-xz~%~%")
       
       (dolist (repo-dir (cl-git-tree/fs:find-git-repos))
         (incf processed)
         (let ((repo-name (cl-git-tree/fs:repo-name repo-dir))
               (providers (get-repo-providers repo-dir))
               (skip nil))
           (format t "~%Репозиторий: ~A~%" repo-name)
           
           ;; Проверяем чистоту репозитория
           (unless (repo-is-clean-p repo-dir)
             (format t "⚠️  Пропущено: репозиторий имеет незакоммиченные изменения~%")
             (setf skip t))
           
           ;; Проверяем дату последнего коммита
           (when (and (not skip) days-filter)
             (let ((days (days-since-last-commit repo-dir)))
               (if days
                   (if (> days days-filter)
                       (progn
                         (format t "⚠️  Пропущено: последний коммит ~A дней назад (> ~A)~%" 
                                 days days-filter)
                         (setf skip t))
                       (format t "✔ Последний коммит ~A дней назад~%" days))
                   (progn
                     (format t "⚠️  Пропущено: не удалось определить дату последнего коммита~%")
                     (setf skip t)))))
           
           ;; Архивируем для каждого найденного провайдера
           (if (not skip)
               (if providers
                   ;; Обрабатываем каждый провайдер
                   (dolist (provider providers)
                     ;; Ищем ВСЕ локации с этим провайдером и url-xz
                     (let ((matching-locs 
                             (loop for k in (cl-git-tree/loc:all-location-keys)
                                   for l = (cl-git-tree/loc:find-location k)
                                   when (and l 
                                             (eq (cl-git-tree/loc:<location>-provider l) provider)
                                             (cl-git-tree/loc:<location>-url-xz l))
                                   collect l)))
                       (if matching-locs
                           ;; Архивируем в каждую найденную локацию с url-xz
                           (dolist (loc matching-locs)
                             (when (create-tar-xz-archive repo-dir 
                                                           (uiop:ensure-directory-pathname 
                                                            (cl-git-tree/loc:<location>-url-xz loc)))
                               (incf archived)))
                           (format t "⚠️  Пропущено: провайдер ~A не имеет локаций с :url-xz~%" provider))))
                   (format t "⚠️  Пропущено: не определены провайдеры репозитория~%"))
               ;; skip = t, репозиторий уже был пропущен ранее с объяснением
               nil)))
       
       (format t "~%~%=== Итого ===~%")
       (format t "Обработано репозиториев: ~A~%" processed)
       (format t "Создано архивов: ~A~%" archived)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "transport" #'cmd-transport "Архивировать чистые репозитории в tar.xz"))
