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

