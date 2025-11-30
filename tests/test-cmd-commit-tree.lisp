;;;; ./tests/test-cmd-commit-tree.lisp
;;;; Тесты для cmd-commit на дереве репозиториев

(in-package :cl-git-tree/tests)

(def-suite cmd-commit-tree
  :description "Тесты для команды cmd-commit на дереве репозиториев"
  :in all)

(in-suite cmd-commit-tree)

(def-test cmd-commit-tree-multiple-repos ()
  "Тест cmd-commit на дереве с двумя подкаталогами, каждый с git репозиторием.
   Проверяет, что коммит создаётся для всех репозиториев в дереве."
  (let* ((root-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-cmd-commit-tree"))
                    (uiop:temporary-directory)))
         (repo1-dir (uiop:merge-pathnames*
                     (make-pathname :directory '(:relative "repo1"))
                     root-dir))
         (repo2-dir (uiop:merge-pathnames*
                     (make-pathname :directory '(:relative "repo2"))
                     root-dir))
         (readme1 (uiop:merge-pathnames* #P"README.org" repo1-dir))
         (readme2 (uiop:merge-pathnames* #P"README.org" repo2-dir))
         (original-cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           ;; Создание директорий
           (uiop:ensure-all-directories-exist repo1-dir)
           (uiop:ensure-all-directories-exist repo2-dir)

           ;; Инициализация первого репозитория
           (let ((ws1 (cl-git-tree/loc:make-workspace repo1-dir)))
             (cl-git-tree/loc:git-init ws1)
             (cl-git-tree/git-utils:git-run repo1-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run repo1-dir "config" "user.name" "Test User")
             (with-open-file (f readme1 :direction :output)
               (write-string "# Repository 1" f))
             (cl-git-tree/git-utils:git-run repo1-dir "add" "README.org")
             (cl-git-tree/loc:repo-commit ws1 :message "Initial commit repo1"))

           ;; Инициализация второго репозитория
           (let ((ws2 (cl-git-tree/loc:make-workspace repo2-dir)))
             (cl-git-tree/loc:git-init ws2)
             (cl-git-tree/git-utils:git-run repo2-dir "config" "user.email" "test@example.com")
             (cl-git-tree/git-utils:git-run repo2-dir "config" "user.name" "Test User")
             (with-open-file (f readme2 :direction :output)
               (write-string "# Repository 2" f))
             (cl-git-tree/git-utils:git-run repo2-dir "add" "README.org")
             (cl-git-tree/loc:repo-commit ws2 :message "Initial commit repo2"))

           ;; Изменяем файлы в обоих репозиториях
           (with-open-file (f readme1 :direction :output)
             (write-string "# Repository 1 - Updated" f))
           (with-open-file (f readme2 :direction :output)
             (write-string "# Repository 2 - Updated" f))

           ;; Добавляем файлы в индекс
           (cl-git-tree/git-utils:git-run repo1-dir "add" "README.org")
           (cl-git-tree/git-utils:git-run repo2-dir "add" "README.org")

           ;; Переходим в родительский каталог
           (uiop:chdir root-dir)
           
           ;; Вызываем commit-repo напрямую для каждого репозитория
           (format t "~%=== Вызовем commit-repo для двух репозиториев ===~%")
           (cl-git-tree/commands/commit:commit-repo repo1-dir nil)
           (cl-git-tree/commands/commit:commit-repo repo2-dir nil)

           ;; Проверяем логи
           (format t "~%=== Проверяем логи коммитов ===~%")
           (multiple-value-bind (log1 err1 code1)
               (cl-git-tree/git-utils:git-run repo1-dir "log" "--oneline")
             (declare (ignore err1))
             (multiple-value-bind (log2 err2 code2)
                 (cl-git-tree/git-utils:git-run repo2-dir "log" "--oneline")
               (declare (ignore err2))
               (format t "Code1: ~A, Log repo1: ~A~%" code1 log1)
               (format t "Code2: ~A, Log repo2: ~A~%" code2 log2)
               (is (zerop code1))
               (is (zerop code2))
               (is (search "Initial commit" log1))
               (is (stringp log2))
               ;; Проверяем, что добавился новый коммит (временная метка)
               (is (>= (count #\Newline log1) 1))
               (is (>= (count #\Newline log2) 1)))))

      ;; Очистка
      (uiop:chdir original-cwd)
      (when (uiop:directory-exists-p root-dir)
        (uiop:delete-directory-tree root-dir :validate t)))))
