(in-package :cl-git-tree/tests)

(def-suite remote-import-tests
  :description "Тесты для подключения/отключения временных remote после распаковки архива"
  :in transport-tests)

(in-suite remote-import-tests)

(def-test remote-connect-disconnect-import ()
  (let* ((tmpdir (uiop:ensure-directory-pathname
                  (uiop:merge-pathnames* #P"test-remote-import/" (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (let* ((repos-root (uiop:ensure-directory-pathname
                               (uiop:merge-pathnames* #P"repos/" tmpdir)))
                  (xz-root   (uiop:ensure-directory-pathname
                              (uiop:merge-pathnames* #P"xz/" tmpdir)))
                  (repo-dir  (uiop:ensure-directory-pathname
                              (uiop:merge-pathnames* #P"my-repo/" repos-root)))
                  (ws (cl-git-tree/loc:make-workspace repo-dir)))
             ;; Инициализация git-репозитория
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run repo-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run repo-dir "config" "user.name" "Test User")
             (with-open-file (f (uiop:merge-pathnames* #P"README.org" repo-dir)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string "# test" f))
             (cl-git-tree/git-utils:git-run repo-dir "add" "README.org")
             (cl-git-tree/git-utils:git-run repo-dir "commit" "-m" "initial")

             ;; Создаём директорию, которая будет симулировать распакованный архив
             (let ((unpacked (uiop:ensure-directory-pathname (uiop:merge-pathnames* #P"my-repo/" xz-root))))
               (uiop:copy-directory-tree repo-dir unpacked)
               ;; Регистрируем локацию
               (cl-git-tree/loc:add-location "lc"
                 :description "Local XZ root"
                 :url-git (namestring repos-root)
                 :url-xz (namestring xz-root)
                 :provider :local)
               (let ((provider (cl-git-tree/loc:find-location "lc")))
                 ;; Подключаем временный remote
                 (is-true (cl-git-tree/loc:remote-import-connect ws provider :verbose nil))
                 ;; Проверяем что remote присутствует
                 (multiple-value-bind (out err code)
                     (cl-git-tree/git-utils:git-run repo-dir "remote")
                   (is-true (search "lc-import" out)))
                 ;; Отключаем временный remote
                 (is-true (cl-git-tree/loc:remote-import-disconnect ws provider :verbose nil))
                 ;; Удалённый remote больше не должен присутствовать
                 (multiple-value-bind (out err code)
                     (cl-git-tree/git-utils:git-run repo-dir "remote")
                   (is (not (search "lc-import" out))))))))
      (when (uiop:directory-exists-p tmpdir)
        (cl-git-tree/tests:force-delete-directory tmpdir)))))
