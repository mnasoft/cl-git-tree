;;;; ./tests/test-single-repo-commands.lisp
;;;; Тесты для команд single-repo: switch, checkout, ls-remote

(in-package :cl-git-tree/tests)

(def-suite single-repo-commands
  :description "Тесты для команд на одном репозитории (switch, checkout, ls-remote)"
  :in all)

(in-suite single-repo-commands)

;;; --- Тесты для repo-switch ---

(def-test repo-switch-to-branch ()
  "Тест переключения на существующую ветку."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-repo-switch"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Подготовка репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл и первый коммит
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             
             ;; Создаём новую ветку
             (cl-git-tree/git-utils:git-run test-dir "checkout" "-b" "feature/test")
             
             ;; Переходим обратно на main/master
             (multiple-value-bind (out err code)
                 (cl-git-tree/loc:repo-switch ws "master")
               (declare (ignore out err))
               (is (zerop code)))
             
             ;; Проверяем текущую ветку
             (multiple-value-bind (out err code)
                 (cl-git-tree/git-utils:git-run test-dir "branch" "--show-current")
               (declare (ignore err code))
               (is-true (string= (string-trim '(#\Newline #\Space) out) "master")))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir)))))

(def-test repo-switch-create-new-branch ()
  "Тест создания и переключения на новую ветку."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-repo-switch-create"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Подготовка репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл и первый коммит
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             
             ;; Создаём новую ветку через repo-switch
             (multiple-value-bind (out err code)
                 (cl-git-tree/loc:repo-switch ws "feature/new" :create t)
               (declare (ignore out err))
               (is (zerop code)))
             
             ;; Проверяем текущую ветку
             (multiple-value-bind (out err code)
                 (cl-git-tree/git-utils:git-run test-dir "branch" "--show-current")
               (declare (ignore err code))
               (is-true (string= (string-trim '(#\Newline #\Space) out) "feature/new")))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir)))))

;;; --- Тесты для repo-checkout ---

(def-test repo-checkout-branch ()
  "Тест checkout на существующую ветку."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-repo-checkout"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Подготовка репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл и первый коммит
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             
             ;; Создаём новую ветку
             (cl-git-tree/git-utils:git-run test-dir "checkout" "-b" "develop")
             
             ;; Переходим обратно на master через repo-checkout
             (multiple-value-bind (out err code)
                 (cl-git-tree/loc:repo-checkout ws "master")
               (declare (ignore out err))
               (is (zerop code)))
             
             ;; Проверяем текущую ветку
             (multiple-value-bind (out err code)
                 (cl-git-tree/git-utils:git-run test-dir "branch" "--show-current")
               (declare (ignore err code))
               (is-true (string= (string-trim '(#\Newline #\Space) out) "master")))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir)))))

(def-test repo-checkout-create-branch ()
  "Тест создания ветки через repo-checkout -b."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-repo-checkout-create"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Подготовка репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл и первый коммит
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             
             ;; Создаём новую ветку через repo-checkout
             (multiple-value-bind (out err code)
                 (cl-git-tree/loc:repo-checkout ws "hotfix/urgent" :create t)
               (declare (ignore out err))
               (is (zerop code)))
             
             ;; Проверяем текущую ветку
             (multiple-value-bind (out err code)
                 (cl-git-tree/git-utils:git-run test-dir "branch" "--show-current")
               (declare (ignore err code))
               (is-true (string= (string-trim '(#\Newline #\Space) out) "hotfix/urgent")))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir)))))

;;; --- Тесты для repo-ls-remote ---

(def-test repo-ls-remote-local-repo ()
  "Тест получения списка ссылок из локального удалённого репозитория."
  (let* ((remote-dir (uiop:merge-pathnames*
                      (make-pathname :directory '(:relative "test-remote-repo"))
                      (uiop:temporary-directory)))
         (test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ls-remote"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Создаём bare удалённый репозиторий
           (uiop:ensure-all-directories-exist remote-dir)
           (cl-git-tree/git-utils:git-run remote-dir "init" "--bare" "repo.git")
           
           ;; Подготовка локального репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл и коммит
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             
             ;; Добавляем remote и делаем push
             (let ((remote-url (namestring (uiop:merge-pathnames* #P"repo.git" remote-dir))))
               (cl-git-tree/git-utils:git-run test-dir "remote" "add" "origin" remote-url)
               (cl-git-tree/git-utils:git-run test-dir "push" "-u" "origin" "master")
               
               ;; Получаем список ссылок
               (multiple-value-bind (out err code)
                   (cl-git-tree/loc:repo-ls-remote ws remote-url)
                 (declare (ignore err))
                 (is (zerop code))
                 (is-true (search "master" out))))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir))
      (when (uiop:directory-exists-p remote-dir)
        (cl-git-tree/tests:force-delete-directory remote-dir)))))

(def-test repo-ls-remote-with-filter ()
  "Тест получения списка ссылок с фильтром (только теги)."
  (let* ((remote-dir (uiop:merge-pathnames*
                      (make-pathname :directory '(:relative "test-remote-repo-tags"))
                      (uiop:temporary-directory)))
         (test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ls-remote-tags"))
                    (uiop:temporary-directory)))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Создаём bare удалённый репозиторий
           (uiop:ensure-all-directories-exist remote-dir)
           (cl-git-tree/git-utils:git-run remote-dir "init" "--bare" "repo.git")
           
           ;; Подготовка локального репозитория
           (uiop:ensure-all-directories-exist test-dir)
           (uiop:chdir test-dir)
           (let ((ws (cl-git-tree/loc:make-workspace test-dir)))
             (cl-git-tree/loc:git-init ws)
             (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
             
             ;; Создаём файл, коммит и тег
             (with-open-file (f (merge-pathnames "test.txt" test-dir) :direction :output)
               (write-string "test content" f))
             (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
             (cl-git-tree/git-utils:git-run test-dir "commit" "-m" "Initial commit")
             (cl-git-tree/git-utils:git-run test-dir "tag" "v1.0.0")
             
             ;; Добавляем remote и делаем push
             (let ((remote-url (namestring (uiop:merge-pathnames* #P"repo.git" remote-dir))))
               (cl-git-tree/git-utils:git-run test-dir "remote" "add" "origin" remote-url)
               (cl-git-tree/git-utils:git-run test-dir "push" "-u" "origin" "master")
               (cl-git-tree/git-utils:git-run test-dir "push" "origin" "v1.0.0")
               
               ;; Получаем список только веток
               (multiple-value-bind (out err code)
                   (cl-git-tree/loc:repo-ls-remote ws remote-url :branches t)
                 (declare (ignore err))
                 (is (zerop code))
                 (is-true (search "master" out))))))
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p test-dir)
        (cl-git-tree/tests:force-delete-directory test-dir))
      (when (uiop:directory-exists-p remote-dir)
        (cl-git-tree/tests:force-delete-directory remote-dir)))))
;;; --- Тесты для CLI-команд не требуются, так как они просто выводят в stdout ---

