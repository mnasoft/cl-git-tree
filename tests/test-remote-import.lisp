;; Test for temporary remote connect/disconnect after importing an unpacked archive

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
             ;; Initialise git repository
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

             ;; Create directory that simulates unpacked archive and copy contents there
             ;; remote-import-connect expects unpacked dir named <repo>.git/
             (let ((unpacked (uiop:ensure-directory-pathname (uiop:merge-pathnames* #P"my-repo.git/" xz-root))))
               ;; Ensure target exists, then copy repository contents into it
               (cl-git-tree/shell-utils:shell-run-single "." "mkdir" "-p" (namestring unpacked))
               (cl-git-tree/shell-utils:shell-run-single
                "." "cp" "-a"
                (concatenate 'string (namestring repo-dir) ".")
                (namestring unpacked))

               ;; Register location and attempt to connect temporary remote
               (cl-git-tree/loc:add-location "lc"
                 :description "Local XZ root"
                 :url-git (namestring repos-root)
                 :url-xz (namestring xz-root)
                 :provider :local)
               (let ((provider (cl-git-tree/loc:find-location "lc")))
                   (let* ((xz-base (cl-git-tree/loc:<location>-url-xz provider))
                     (candidate (uiop:ensure-directory-pathname
                       (merge-pathnames (format nil "~A.git/" (cl-git-tree/fs:repo-name repo-dir))
                             (uiop:ensure-directory-pathname xz-base)))))
                     (format t "DEBUG: xz-base=~S~%DEBUG: candidate=~A exists=~A~%" xz-base candidate (uiop:directory-exists-p candidate)))
                 (is-true (cl-git-tree/loc:remote-import-connect ws provider :verbose t))
                 (multiple-value-bind (out err code)
                     (cl-git-tree/git-utils:git-run repo-dir "remote")
                   (is-true (search "lc-import" out)))
                 (is-true (cl-git-tree/loc:remote-import-disconnect ws provider :verbose t))
                 (multiple-value-bind (out err code)
                     (cl-git-tree/git-utils:git-run repo-dir "remote")
                   (is (not (search "lc-import" out)))))))
      (when (uiop:directory-exists-p tmpdir)
        (cl-git-tree/tests:force-delete-directory tmpdir))))))

(def-test transport-import-creates-missing-and-merges-existing-branches ()
  (let* ((tmpdir (uiop:ensure-directory-pathname
                  (uiop:merge-pathnames* #P"test-remote-import-branches/"
                                         (uiop:temporary-directory)))))
    (unwind-protect
         (let* ((repos-root (uiop:ensure-directory-pathname
                             (uiop:merge-pathnames* #P"repos/" tmpdir)))
                (xz-root (uiop:ensure-directory-pathname
                          (uiop:merge-pathnames* #P"xz/" tmpdir)))
                (source-dir (uiop:ensure-directory-pathname
                             (uiop:merge-pathnames* #P"source/my-repo/" tmpdir)))
                (target-dir (uiop:ensure-directory-pathname
                             (uiop:merge-pathnames* #P"target/my-repo/" tmpdir)))
                (bare-dir (uiop:ensure-directory-pathname
                           (uiop:merge-pathnames* #P"my-repo.git/" xz-root)))
                (archive-path (uiop:merge-pathnames* #P"my-repo.tar.xz" xz-root))
                (source-ws (cl-git-tree/loc:make-workspace source-dir))
                (target-ws (cl-git-tree/loc:make-workspace target-dir)))

           ;; Подготавливаем source-репозиторий с двумя ветками:
           ;; feature-existing (существует и в target) и feature-new (отсутствует в target).
           (let ((base-branch nil))
             (cl-git-tree/loc:git-init source-ws)
             (cl-git-tree/git-utils:git-run source-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run source-dir "config" "user.name" "Test User")
             (with-open-file (f (uiop:merge-pathnames* #P"README.org" source-dir)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string "# source" f))
             (cl-git-tree/git-utils:git-run source-dir "add" "README.org")
             (cl-git-tree/git-utils:git-run source-dir "commit" "-m" "base")
             (setf base-branch (cl-git-tree/git-utils:current-branch source-dir))

             (cl-git-tree/git-utils:git-run source-dir "checkout" "-b" "feature-existing")
             (with-open-file (f (uiop:merge-pathnames* #P"existing.txt" source-dir)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string "existing from source" f))
             (cl-git-tree/git-utils:git-run source-dir "add" "existing.txt")
             (cl-git-tree/git-utils:git-run source-dir "commit" "-m" "existing-remote-commit")

             (cl-git-tree/git-utils:git-run source-dir "checkout" "-b" "feature-new" "HEAD~1")
             (with-open-file (f (uiop:merge-pathnames* #P"new.txt" source-dir)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
               (write-string "new branch from source" f))
             (cl-git-tree/git-utils:git-run source-dir "add" "new.txt")
             (cl-git-tree/git-utils:git-run source-dir "commit" "-m" "new-branch-commit")

             ;; Возвращаем HEAD на базовую ветку перед клонированием target,
             ;; чтобы в target отсутствовала локальная ветка feature-new до импорта.
             (cl-git-tree/git-utils:git-run source-dir "checkout" base-branch))

           ;; Подготавливаем target-репозиторий: в нём есть только feature-existing
           ;; в точке base, а feature-new отсутствует.
           (cl-git-tree/shell-utils:shell-run-single "." "mkdir" "-p" (namestring target-dir))
           (cl-git-tree/git-utils:git-run "." "clone" (namestring source-dir) (namestring target-dir))
           (cl-git-tree/git-utils:git-run target-dir "config" "user.email" "test@example.com")
           (cl-git-tree/git-utils:git-run target-dir "config" "user.name" "Test User")
           (let ((initial-branch (cl-git-tree/git-utils:current-branch target-dir)))
             (cl-git-tree/git-utils:git-run target-dir "checkout" "feature-existing")
             (cl-git-tree/git-utils:git-run target-dir "reset" "--hard" "HEAD~1")
             (cl-git-tree/git-utils:git-run target-dir "checkout" initial-branch)

             ;; Формируем транспортный архив в формате <repo>.tar.xz с содержимым <repo>.git/.
             (cl-git-tree/git-utils:git-run "." "clone" "--bare" (namestring source-dir) (namestring bare-dir))
             (uiop:run-program
              (format nil "tar -C ~A -c -J -f ~A my-repo.git"
                      (namestring xz-root)
                      (namestring archive-path))
              :output :string
              :error-output :string
              :ignore-error-status t)

             (cl-git-tree/loc:add-location "lc-branch-import"
               :description "Local XZ root"
               :url-git (namestring repos-root)
               :url-xz (namestring xz-root)
               :provider :local)

             (let ((provider (cl-git-tree/loc:find-location "lc-branch-import")))
               (is-true (cl-git-tree/loc:repo-transport-import target-ws provider :verbose t))

               ;; feature-new должна быть создана локально из транспортного архива.
               (multiple-value-bind (out err code)
                   (cl-git-tree/git-utils:git-run target-dir "show-ref" "--verify" "refs/heads/feature-new")
                 (declare (ignore out err))
                 (is (= 0 code)))

               ;; feature-existing должна быть обновлена merge-ом remote-ветки.
               (multiple-value-bind (out err code)
                   (cl-git-tree/git-utils:git-run target-dir "log" "feature-existing" "-1" "--pretty=%s")
                 (declare (ignore err code))
                 (is (search "existing-remote-commit" out)))

               ;; После импорта возвращаемся на исходную ветку.
               (is (string= initial-branch (cl-git-tree/git-utils:current-branch target-dir))))))
      (when (uiop:directory-exists-p tmpdir)
        (cl-git-tree/tests:force-delete-directory tmpdir)))))

