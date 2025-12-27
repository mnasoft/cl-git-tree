(in-package :cl-git-tree/commands/transport)


;; Новый вариант: распаковка архива в тот же каталог, добавление временного remote, git pull master, удаление remote и каталога
(defun apply-tar-xz-archive (archive-path dest-root)
  "Распаковывает bare-архив в каталог архива, делает pull через временный remote, удаляет временный remote и каталог."
  (let* ((archive-path (if (and (stringp archive-path)
                                (> (length archive-path) 0)
                                (char= (char archive-path (- (length archive-path) 1)) #\/))
                           (subseq archive-path 0 (- (length archive-path) 1))
                           archive-path))
         (expanded-archive (cl-git-tree/fs:expand-home archive-path))
         (archive-dir (uiop:pathname-directory-pathname expanded-archive))
         (archive-name (file-namestring expanded-archive))
         (repo-name (if (and archive-name (>= (length archive-name) 7)
                             (string= ".tar.xz" (subseq archive-name (- (length archive-name) 7))))
                        (subseq archive-name 0 (- (length archive-name) 7))
                        archive-name))
         (bare-name (concatenate 'string repo-name ".git"))
         (bare-path (merge-pathnames bare-name archive-dir))
         (expanded-dest-root (cl-git-tree/fs:expand-home dest-root)))
    (format t "⬇ Распаковка ~A → ~A~%" archive-name (namestring archive-dir))
    (multiple-value-bind (out err code)
        (uiop:run-program
         (list "tar" "-xJf" (namestring expanded-archive) "-C" (namestring archive-dir))
         :output :string :error-output :string :ignore-error-status t)
      (declare (ignore out))
      (if (zerop code)
          (progn
            (if (probe-file bare-path)
                (let* ((default-directory (namestring expanded-dest-root))
                       (remote-name "lc-temp"))
                  (format t "✔ Распаковано: ~A~%" (namestring bare-path))
                  ;; Добавить временный remote
                  (multiple-value-bind (out2 err2 code2)
                      (uiop:run-program
                       (list "git" "-C" default-directory "remote" "add" remote-name (namestring bare-path))
                       :output :string :error-output :string :ignore-error-status t)
                    (declare (ignore out2))
                    (if (zerop code2)
                        (progn
                          ;; git pull lc-temp master
                          (multiple-value-bind (out3 err3 code3)
                              (uiop:run-program
                               (list "git" "-C" default-directory "pull" remote-name "master")
                               :output :string :error-output :string :ignore-error-status t)
                            (declare (ignore out3))
                            (if (zerop code3)
                                (format t "✔ git pull lc-temp master успешно~%")
                                (format t "❌ Ошибка git pull:~%~A~%" err3)))
                          ;; Удалить remote
                          (uiop:run-program
                           (list "git" "-C" default-directory "remote" "remove" remote-name)
                           :output :string :error-output :string :ignore-error-status t)
                          ;; Удалить bare-репозиторий
                          (cl-git-tree/fs:delete-directory-tree bare-path)
                          t)
                        (progn
                          (format t "❌ Ошибка добавления remote:~%~A~%" err2)
                          (cl-git-tree/fs:delete-directory-tree bare-path)
                          nil)))
                  (progn
                    (format t "❌ Ошибка: не найден каталог ~A после распаковки~%" bare-path)
                    nil)))
            (progn
              (format t "❌ Ошибка распаковки:~%~A~%" err)
              nil))))))

(defun transport-import ()
  "Импортирует все найденные *.tar.xz из :url-xz в :url-git для локальных локаций."
  (let ((processed 0)
        (applied 0))
    (format t "⬇ Импорт архивов tar.xz из :url-xz в :url-git для всех локальных локаций~%~%")
    (dolist (loc-key (cl-git-tree/loc:all-location-keys))
      (let* ((loc (cl-git-tree/loc:find-location loc-key))
             (url-xz (and loc (cl-git-tree/loc:<location>-url-xz loc)))
             (url-git (and loc (cl-git-tree/loc:<location>-url-git loc)))
             (provider (and loc (cl-git-tree/loc:<location>-provider loc))))
        (when (and loc url-xz url-git)
          (let* ((xz-dir (uiop:ensure-directory-pathname (cl-git-tree/fs:expand-home url-xz)))
                 (archives (directory (merge-pathnames #p"*.tar.xz" xz-dir))))
            (format t "~%archives: ~S~%" archives)
            (format t "~%url-git: ~S~%"  url-git)
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
