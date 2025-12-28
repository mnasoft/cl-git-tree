(in-package :cl-git-tree/loc)

#+nil
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
         (git-base (<location>-url-git provider))
         (candidates '())
         (found nil))
    ;; Построим список кандидатных путей, которые могут содержать распакованный репозиторий
    (when xz-base
      (push (uiop:ensure-directory-pathname
             (merge-pathnames (format nil "~A/" repo)
                              (uiop:ensure-directory-pathname xz-base)))
            candidates)
      (push (uiop:ensure-directory-pathname (merge-pathnames (format nil "~A.git/" repo) (uiop:ensure-directory-pathname xz-base))) candidates))
    (when git-base
      (push (uiop:ensure-directory-pathname (merge-pathnames (format nil "~A.git/" repo) (uiop:ensure-directory-pathname git-base))) candidates)
      (push (uiop:ensure-directory-pathname (merge-pathnames (format nil "~A/" repo) (uiop:ensure-directory-pathname git-base))) candidates))

    (dolist (p candidates)
      (when (and (uiop:directory-exists-p p) (not found))
        (let ((url (uiop:native-namestring p)))
          (multiple-value-bind (out err code)
              (cl-git-tree/git-utils:git-run root "remote" "add" remote url)
            (if (zerop code)
                (progn
                  (when verbose
                    (format t "✅ Временный remote '~A' добавлен → ~A~%" remote url))
                  (setf found t))
                (when verbose
                  (format t "❌ Ошибка при добавлении временного remote '~A' (путь ~A): ~A~%" remote url (or err out))))))))

    (if found
        t
        (progn (when verbose
                 (format t "⚠️  Не найден распакованный каталог для репозитория ~A в ~A или ~A~%" repo xz-base git-base))
               nil))))

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
                  (format t "✅ Временный remote '~A' добавлен → ~A~%" remote url))
                (setf found t))
              (when verbose
                (format t "❌ Ошибка при добавлении временного remote '~A' (путь ~A): ~A~%" remote url (or err out)))))))
    (if found
        t
        (progn (when verbose
                 (format t "⚠️  Не найден распакованный каталог для репозитория ~A в ~A~%" repo xz-base))
               nil))))
