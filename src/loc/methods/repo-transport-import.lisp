(in-package :cl-git-tree/loc)

;; Вспомогательная функция для получения списка веток из удалённого репозитория
(defun get-remote-branches (root-dir remote-name)
  "Получает список веток (без префикса refs/heads/) из указанного remote.
Возвращает список названий веток."
  (multiple-value-bind (stdout stderr code)
      (cl-git-tree/git-utils:git-run root-dir (list "ls-remote" "--heads" remote-name))
    (declare (ignore stderr))
    (cond
      ((zerop code)
       (let ((branches nil)
             (lines (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space) stdout))))
         (dolist (line lines)
           (when (> (length line) 0)
             (let ((match (cl-ppcre:scan-to-strings "refs/heads/(.+)$" line)))
               (when match
                 (push (aref match 0) branches)))))
         (nreverse branches)))
      (t
       nil))))


;; cleanup-remote-dir заменён на keep-remote-dir (по умолчанию NIL, если T — каталог не удаляется)
(defmethod repo-transport-import ((ws <workspace>) (provider <provider>)
                                  &key verbose keep-remote-dir delete-archive &allow-other-keys)
  "Импортирует изменения из tar.xz архива для WORKSPACE и PROVIDER.
Распаковывает архив из :url-xz провайдера в рабочий каталог репозитория.
Выполняет fetch и pull для всех веток из импортируемого архива.
Если keep-remote-dir=T, временный каталог remote не удаляется."
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (prov-symbol (and provider (<location>-provider provider))))
    (cond
      ((not repo-dir)
       (when verbose
         (format t "~A Не найден путь к репозиторию для ~A~%"
                 (find-emo ws "warning")
                 repo-name))
       (values nil nil))
      ((not url-xz)
       (when verbose
         (format t "~A Локация ~A (провайдер ~A) не имеет :url-xz~%"
                 (find-emo ws "warning")
                 (<location>-id provider) prov-symbol))
       (values nil nil))
      (t
       (if (repo-transport-unpack ws provider :verbose verbose)
           ;; Подключаем временный remote (например, "<loc>-import"), делаем fetch и pull, затем отключаем
           (let ((tmp-remote (format nil "~A-import" (<location>-id provider))))
             (if (remote-import-connect ws provider :remote-name tmp-remote :verbose verbose)
                 (progn
                   ;; Сначала выполняем fetch для получения всех веток
                   (when verbose
                     (format t "  Выполняю fetch для ~A~%" tmp-remote))
                   (cl-git-tree/git-utils:git-run repo-dir (list "fetch" tmp-remote))
                   
                   ;; Получаем список веток из импортируемого хранилища
                   (let ((branches (get-remote-branches repo-dir tmp-remote)))
                     (when verbose
                       (format t "  Найдено веток: ~A~%" (length branches)))
                     
                     ;; Выполняем pull для каждой ветки
                     (let ((pull-success t))
                       (dolist (branch branches)
                         (when verbose
                           (format t "  Выполняю pull ветки ~A из ~A~%" branch tmp-remote))
                         (multiple-value-bind (ignore-ws success)
                             (cl-git-tree/loc:repo-pull ws provider :remote tmp-remote :branch branch)
                           (declare (ignore ignore-ws))
                           (unless success
                             (setf pull-success nil))))
                       
                       ;; Отключаем временный remote
                       (remote-import-disconnect ws provider :remote-name tmp-remote :verbose verbose)
                       
                       ;; Удаляем каталог временного remote только при успехе и если не keep-remote-dir
                       (unless (or keep-remote-dir (not pull-success))
                         (remote-import-cleanup-dir ws provider :verbose verbose))
                       
                       ;; Удаляем архив (опционально)
                       (when delete-archive
                         (remote-import-delete-archive ws provider :verbose verbose))
                       
                       (values pull-success t))))
               ;; Не удалось подключить временный remote
               (values nil nil)))
           ;; Распаковка архива не удалась
           (values nil nil))))))
