(in-package :cl-git-tree/loc)

(defmethod remote-import-connect ((ws <workspace>) (provider <provider>) &key (remote-name nil) verbose &allow-other-keys)
  "Подключить только что распакованный каталог архива как временный remote.
        Если remote-name = NIL, используется шаблон <location>-import.
        Проверяет существование каталога распакованного репозитория и добавляет
        remote в git-конфигурацию рабочего каталога.
        Возвращает T при успехе, NIL при ошибке." 
  (let* ((root (git-root ws))
         (repo (repo-name ws))
         (loc-id (<location>-id provider))
         (remote (or remote-name (format nil "~A-import" loc-id)))
         (xz-base (<location>-url-xz provider))
         (candidate (when xz-base
                      (uiop:ensure-directory-pathname
                       (merge-pathnames
                        (format nil "~A.git/" repo)
                        (uiop:ensure-directory-pathname xz-base)))))
         (found nil))
    (when (and (uiop:directory-exists-p candidate) (not found))
      (let ((url (uiop:native-namestring candidate)))
        (multiple-value-bind (out err code)
            (cl-git-tree/git-utils:git-run root "remote" "add" remote url)
          (if (zerop code)
              (progn
                (when verbose
                  (format t "~A Временный remote '~A' добавлен → ~A~%"
                          (find-emo ws "success")
                          remote url))
                (setf found t))
              (progn
                (when verbose
                  (format t "~A Ошибка при добавлении временного remote '~A' (путь ~A): ~A~%"
                          (find-emo ws "error")
                          remote url (or err out)))
                (multiple-value-bind (r-out r-err r-code)
                    (cl-git-tree/git-utils:git-run root "remote")
                  (declare (ignore r-err))
                  (when (and (zerop r-code)
                             (search remote r-out))
                    (multiple-value-bind (u-out u-err u-code)
                        (cl-git-tree/git-utils:git-run root "remote" "get-url" remote)
                      (declare (ignore u-err))
                      (when (and (zerop u-code)
                                 (string= (string-right-trim "\n" u-out) url))
                        (when verbose
                          (format t "~A Временный remote '~A' уже существует и указывает на ~A — пропускаю~%"
                                  (find-emo ws "warning")
                                  remote url))
                        (setf found t))))))))))
    (if found
        t
        (progn (when verbose
                 (format t "~A  Не найден распакованный каталог для репозитория ~A в ~A~%"
                         (find-emo ws "warning")
                         repo
                         xz-base))
               nil))))
