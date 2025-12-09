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

(defun get-repo-provider (repo-dir)
  "Определяет провайдера репозитория по его remotes."
  (let ((remotes (cl-git-tree/git-utils:repo-remotes repo-dir)))
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
                      (return-from get-repo-provider 
                        (cl-git-tree/loc:<location>-provider loc)))))))))))
  nil))

(defun create-tar-xz-archive (repo-dir output-path)
  "Создаёт tar.xz архив репозитория в указанном месте."
  (let* ((repo-name (cl-git-tree/fs:repo-name repo-dir))
         (archive-name (format nil "~A.tar.xz" repo-name))
         (archive-path (merge-pathnames archive-name output-path))
         (parent-dir (truename (merge-pathnames "../" repo-dir)))
         (repo-basename (file-namestring (string-right-trim "/" (namestring repo-dir)))))
    (ensure-directories-exist output-path)
    (format t "📦 Создаю архив ~A...~%" archive-path)
    (multiple-value-bind (out err code)
        (uiop:run-program
         (list "tar" "-C" (namestring parent-dir) 
               "-cJf" (namestring archive-path)
               repo-basename)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore out))
      (if (zerop code)
          (progn
            (format t "✔ Архив создан: ~A~%" archive-path)
            t)
          (progn
            (format t "❌ Ошибка при создании архива:~%~A~%" err)
            nil)))))

(defun cmd-transport (&rest args)
  "CLI-команда: архивирует чистые репозитории в tar.xz.
  
  Опции:
    --provider PROVIDER  - фильтр по провайдеру (например, :local, :github)
    --days N            - архивировать только репозитории, обновлённые не позднее N дней назад
    --output PATH       - путь для сохранения архивов (по умолчанию ~/.git-tree/xz/)
    --help              - показать эту справку"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Архивирует чистые git-репозитории в формате tar.xz.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree transport [--provider PROVIDER] [--days N] [--output PATH]~%~%")
     (format t "Опции:~%")
     (format t "  --provider PROVIDER  Фильтр по провайдеру (local, github, gitlab)~%")
     (format t "  --days N            Архивировать только репозитории с коммитами не старее N дней~%")
    ;; Экранируем тильду, чтобы формат не считал директиву ~/ (печатаем буквально ~/)
    (format t "  --output PATH       Путь для сохранения архивов (по умолчанию ~~/.git-tree/xz/)~%")
     (format t "  --help              Показать эту справку~%~%")
     (format t "Примеры:~%")
     (format t "  git-tree transport --provider local --days 30~%")
     (format t "  git-tree transport --output /tmp/archives/~%"))
    (t
     (let ((provider-filter nil)
           (days-filter nil)
           (output-path (merge-pathnames #p".git-tree/xz/" (user-homedir-pathname)))
           (processed 0)
           (archived 0))
       
       ;; Парсим аргументы
       (loop for (arg val) on args by #'cddr
             do (cond
                  ((string= arg "--provider")
                   (setf provider-filter (intern (string-upcase val) :keyword)))
                  ((string= arg "--days")
                   (setf days-filter (parse-integer val :junk-allowed t)))
                  ((string= arg "--output")
                   (setf output-path (uiop:ensure-directory-pathname val)))))
       
       (format t "🔍 Поиск репозиториев для архивирования...~%")
       (when provider-filter
         (format t "   Фильтр по провайдеру: ~A~%" provider-filter))
       (when days-filter
         (format t "   Фильтр по дате: не старее ~A дней~%" days-filter))
       (format t "   Путь для архивов: ~A~%~%" output-path)
       
       (dolist (repo-dir (cl-git-tree/fs:find-git-repos))
         (incf processed)
         (let ((repo-name (cl-git-tree/fs:repo-name repo-dir))
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
           
           ;; Проверяем провайдера
           (when (and (not skip) provider-filter)
             (let ((provider (get-repo-provider repo-dir)))
               (if (and provider (eq provider provider-filter))
                   (format t "✔ Провайдер: ~A~%" provider)
                   (progn
                     (format t "⚠️  Пропущено: провайдер ~A не соответствует фильтру ~A~%" 
                             provider provider-filter)
                     (setf skip t)))))
           
           ;; Архивируем
           (when (and (not skip) (create-tar-xz-archive repo-dir output-path))
             (incf archived))))
       
       (format t "~%~%=== Итого ===~%")
       (format t "Обработано репозиториев: ~A~%" processed)
       (format t "Создано архивов: ~A~%" archived)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "transport" #'cmd-transport "Архивировать чистые репозитории в tar.xz"))
