(in-package :cl-git-tree/loc)

(defmethod repo-transport-unpack ((ws <workspace>) (provider <provider>) &key verbose &allow-other-keys)
  "Распаковать tar.xz архив для WORKSPACE/PROVIDER.
Возвращает T при успешной распаковке, NIL в противном случае." 
  (let* ((repo-dir (or (git-root ws)
                       (<workspace>-path ws)))
         (repo-name (and repo-dir (cl-git-tree/fs:repo-name repo-dir)))
         (url-xz (and provider (<location>-url-xz provider)))
         (tar-xz (and url-xz (concatenate 'string url-xz "/" repo-name ".tar.xz"))))
    (cond
      ((not url-xz)
       (when verbose (format t "~A Локация ~A не имеет :url-xz~%"
                             (find-emo ws "warning")
                             (<location>-id provider)))
       nil)
      ((not (probe-file tar-xz))
       (when verbose
         (format t "~A Архив не найден: ~A~%"
                 (find-emo ws "warning")
                 tar-xz))
       nil)
      (t
       (let ((cmd (format nil "tar -xJf ~A -C ~A" tar-xz url-xz)))
         (when verbose (format t "  ⏳ Импорт: ~A → ~A~%" tar-xz url-xz))
         (multiple-value-bind (output error-output exit-code)
             (uiop:run-program cmd :output :string :error-output :string :ignore-error-status t)
           (declare (ignore output))
           (if (zerop exit-code)
               (progn (when verbose (format t "~A Импорт завершён: ~A~%"
                                            (find-emo ws "success")
                                            repo-name)) t)
               (progn
                 (when verbose
                   (format t "~A Ошибка при распаковке ~A: ~A~%"
                           (find-emo ws "error")
                           tar-xz error-output))
                 nil))))))))
