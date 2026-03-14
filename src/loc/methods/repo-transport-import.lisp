(in-package :cl-git-tree/loc)

;; Вспомогательная функция для получения списка веток из удалённого репозитория
(defun get-remote-branches (root-dir remote-name)
  "Получает список веток (без префикса refs/heads/) из указанного remote.
Возвращает список названий веток."
  (multiple-value-bind (stdout stderr code)
      (cl-git-tree/git-utils:git-run root-dir "ls-remote" "--heads" remote-name)
    (declare (ignore stderr))
    (cond
      ((zerop code)
       (let ((branches nil)
             (lines (cl-ppcre:split "\\n" (string-trim '(#\Newline #\Space #\Return) stdout))))
         (dolist (line lines)
           (when (> (length line) 0)
             (multiple-value-bind (whole-match groups)
                 (cl-ppcre:scan-to-strings "refs/heads/(.+?)\\s*$" line)
               (declare (ignore whole-match))
               (when (and groups (> (length groups) 0))
                 (let ((branch-name (string-trim '(#\Space #\Return #\Newline #\Tab) (aref groups 0))))
                   (when (> (length branch-name) 0)
                     (push branch-name branches)))))))
         (nreverse branches)))
      (t
       nil))))

(defun local-branch-exists-p (repo-dir branch-name)
  "Проверяет, существует ли локальная ветка BRANCH-NAME в репозитории REPO-DIR."
  (multiple-value-bind (stdout stderr code)
      (cl-git-tree/git-utils:git-run repo-dir "show-ref" "--verify"
                                     (format nil "refs/heads/~A" branch-name))
    (declare (ignore stdout stderr))
    (zerop code)))

(defun switch-to-local-branch (ws repo-dir branch-name &key verbose)
  "Переключает текущий HEAD на локальную ветку BRANCH-NAME."
  (multiple-value-bind (stdout stderr code)
      (cl-git-tree/git-utils:git-run repo-dir "checkout" branch-name)
    (when (and verbose (not (zerop code)))
      (format t "~A Не удалось переключиться на ветку ~A: ~A~%"
              (find-emo ws "error")
              branch-name
              (or stderr stdout)))
    (zerop code)))

(defun create-local-branch-from-remote (ws repo-dir branch-name remote-name &key verbose)
  "Создаёт локальную ветку BRANCH-NAME из REMOTE-NAME/BRANCH-NAME без слияния."
  (let ((remote-ref (format nil "~A/~A" remote-name branch-name)))
    (multiple-value-bind (stdout stderr code)
        (cl-git-tree/git-utils:git-run repo-dir "branch" branch-name remote-ref)
      (if (zerop code)
          (progn
            (when verbose
              (format t "  Создана локальная ветка ~A из ~A~%" branch-name remote-ref))
            t)
          (progn
            (when verbose
              (format t "~A Не удалось создать ветку ~A из ~A: ~A~%"
                      (find-emo ws "error")
                      branch-name
                      remote-ref
                      (or stderr stdout)))
            nil)))))

(defun merge-remote-branch-into-local (ws repo-dir branch-name remote-name &key verbose)
  "Переключается на локальную BRANCH-NAME и пытается влить REMOTE-NAME/BRANCH-NAME."
  (let ((remote-ref (format nil "~A/~A" remote-name branch-name)))
    (if (switch-to-local-branch ws repo-dir branch-name :verbose verbose)
        (progn
          (when verbose
            (format t "  Выполняю merge ~A в локальную ветку ~A~%" remote-ref branch-name))
          (multiple-value-bind (stdout stderr code)
              (cl-git-tree/git-utils:git-run repo-dir "merge" remote-ref)
            (when (and verbose (not (zerop code)))
              (format t "~A Ошибка merge ~A в ~A: ~A~%"
                      (find-emo ws "error")
                      remote-ref
                      branch-name
                      (or stderr stdout)))
            (zerop code)))
        nil)))


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
                   (cl-git-tree/git-utils:git-run repo-dir "fetch" tmp-remote)
                   
                   ;; Получаем список веток из импортируемого хранилища
                   (let ((branches (get-remote-branches repo-dir tmp-remote)))
                     (when verbose
                       (format t "  Найдено веток: ~A~%" (length branches)))
                     
                     ;; Для каждой ветки: если локальной ветки нет, создаём её из tmp-remote;
                     ;; если есть — переключаемся на неё и пробуем merge tmp-remote/<branch>.
                     (let ((pull-success t)
                           (original-branch (cl-git-tree/git-utils:current-branch repo-dir)))
                       (dolist (branch branches)
                         (if (local-branch-exists-p repo-dir branch)
                             (unless (merge-remote-branch-into-local ws repo-dir branch tmp-remote :verbose verbose)
                               (setf pull-success nil))
                             (unless (create-local-branch-from-remote ws repo-dir branch tmp-remote :verbose verbose)
                               (setf pull-success nil))))

                       ;; Возвращаем исходную ветку, если она существовала до импорта.
                       (when (and original-branch
                                  (not (string= original-branch "HEAD"))
                                  (local-branch-exists-p repo-dir original-branch))
                         (switch-to-local-branch ws repo-dir original-branch :verbose verbose))
                       
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
