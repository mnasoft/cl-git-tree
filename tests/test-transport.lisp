(in-package :cl-git-tree/tests)

(def-suite transport-tests
  :description "Тесты для команды git-tree transport"
  :in all)

(in-suite transport-tests)

(def-test transport-export-uses-repo-providers ()
  "Проверяем, что transport export использует repo-providers и создаёт архивы
для репозиториев, имеющих соответствующие локации с :url-xz."
  (let ((tmpdir (uiop:ensure-directory-pathname
                 (uiop:merge-pathnames* #P"test-transport/"
                                        (uiop:temporary-directory)))))
    ;; Настраиваем временные директории для репо и архивов
    (let* ((repos-root (uiop:ensure-directory-pathname
                        (uiop:merge-pathnames* #P"repos/" tmpdir)))
           (xz-root   (uiop:ensure-directory-pathname
                        (uiop:merge-pathnames* #P"xz/" tmpdir)))
           (repo-dir  (uiop:ensure-directory-pathname
                        (uiop:merge-pathnames* #P"my-repo/" repos-root)))
           (ws (cl-git-tree/loc:make-workspace repo-dir)))
      (unwind-protect
           (progn
             ;; Инициализируем git-репозиторий и делаем один коммит,
             ;; чтобы days-since-last-commit вернул разумное значение.
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run repo-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run repo-dir "config" "user.name" "Test User")
             (with-open-file (f (uiop:merge-pathnames* #P"README.org" repo-dir)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string "# test" f))
             (cl-git-tree/git-utils:git-run repo-dir "add" "README.org")
             (cl-git-tree/git-utils:git-run repo-dir "commit" "-m" "initial" )

             ;; Регистрируем локацию "origin" с провайдером :local, у которой есть :url-xz
             (cl-git-tree/loc:add-location "origin"
               :description "Local XZ root"
               :url-git (namestring repos-root)
               :url-xz (namestring xz-root)
               :provider :local)

             ;; Добавляем remote origin, который совпадает с id локации
             (cl-git-tree/git-utils:git-run
              repo-dir "remote" "add" "origin"
              (format nil "~A~A.git" (namestring repos-root) "my-repo"))

             ;; Хук discover-репозиториев: временно подменим find-git-repos, чтобы он
             ;; возвращал только наш репозиторий. Для простоты вызываем напрямую
             ;; transport-export, а не cmd-transport.
                  (let* ((days-arg '("export" "--days" "365"))
                    (orig-fn (symbol-function 'cl-git-tree/fs:find-git-repos)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'cl-git-tree/fs:find-git-repos)
                            (lambda () (list repo-dir)))
                       ;; Запускаем экспорт (quiet-режим) через CLI-команду
                       (apply #'cl-git-tree/commands/transport:cmd-transport
                         days-arg)
                      ;; Проверяем, что в каталоге xz-root появился хотя бы один .tar.xz
                      (let* ((pattern (merge-pathnames #p"*.tar.xz" xz-root))
                             (archives (directory pattern)))
                        (is (listp archives))
                        (is (>= (length archives) 1))))
                 (setf (symbol-function 'cl-git-tree/fs:find-git-repos) orig-fn))))
        (when (uiop:directory-exists-p tmpdir)
          (cl-git-tree/tests:force-delete-directory tmpdir))))))
