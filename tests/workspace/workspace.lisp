(in-package :cl-git-tree/tests)

(def-suite workspace
  :description "Тесты для подсистемы cl-git-tree/workspace"
  :in all)

(in-suite workspace)

;;; Тесты для git-initialized-p

(def-suite workspace-git-initialized-p
  :description "Тесты для метода git-initialized-p"
  :in workspace)

(in-suite workspace-git-initialized-p)

(def-test git-initialized-p-without-git ()
  "Проверка git-initialized-p для workspace без git."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-no-git-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (is (not (cl-git-tree/loc:git-initialized-p ws)))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

 (def-test git-initialized-p-with-git ()
  "Проверка git-initialized-p для workspace с git."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-git-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           (is (cl-git-tree/loc:git-initialized-p ws)))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для git-root

(def-suite workspace-git-root
  :description "Тесты для метода git-root"
  :in workspace)

(in-suite workspace-git-root)

(def-test git-root-without-git ()
  "Проверка git-root для workspace без git."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-no-git-root-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (is (null (cl-git-tree/loc:git-root ws)))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test git-root-with-git ()
  "Проверка git-root для workspace с git."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-git-root-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           (let ((root (cl-git-tree/loc:git-root ws)))
             (is (not (null root)))
             (is (pathnamep root))
             (is (uiop:directory-exists-p root))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для git-init

(def-suite workspace-git-init
  :description "Тесты для метода git-init"
  :in workspace)

(in-suite workspace-git-init)

(def-test git-init-creates-repository ()
  "Проверка создания git репозитория через git-init."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-init-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is (not (cl-git-tree/loc:git-initialized-p ws)))
           (cl-git-tree/loc:git-init ws)
           (is (cl-git-tree/loc:git-initialized-p ws))
           (is (not (null (cl-git-tree/loc:git-root ws)))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test git-init-idempotent ()
  "Проверка, что повторный вызов git-init не вызывает ошибок."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-init-idempotent-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           (is (cl-git-tree/loc:git-initialized-p ws))
           ;; Повторный вызов не должен вызвать ошибку
           (cl-git-tree/loc:git-init ws)
           (is (cl-git-tree/loc:git-initialized-p ws)))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для repo-name

(def-suite workspace-repo-name
  :description "Тесты для метода repo-name"
  :in workspace)

(in-suite workspace-repo-name)

(def-test repo-name-with-git ()
  "Проверка repo-name для workspace с git."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-repo-name"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           (let ((name (cl-git-tree/loc:repo-name ws)))
             (is (or (stringp name) (null name)))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для repo-status

(def-suite workspace-repo-status
  :description "Тесты для метода repo-status"
  :in workspace)

(in-suite workspace-repo-status)

(def-test repo-status-empty-repo ()
  "Проверка repo-status для пустого репозитория."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-status"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           (let ((status (cl-git-tree/loc:repo-status ws (make-instance 'cl-git-tree/loc:<provider>))))
             (is (stringp status))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для repo-branches

(def-suite workspace-repo-branches
  :description "Тесты для метода repo-branches"
  :in workspace)

(in-suite workspace-repo-branches)

(def-test repo-branches-empty-repo ()
  "Проверка repo-branches для пустого репозитория."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-branches-v2"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir))
         (readme-file (uiop:merge-pathnames* #P"README.org" test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           ;; Настройка git config для коммитов
           (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
           (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
           ;; Создание файла README.org
           (with-open-file (f readme-file :direction :output)
             (write-string "# Test Repository" f))
           ;; Добавление файла в индекс
           (cl-git-tree/git-utils:git-run test-dir "add" "README.org")
           ;; Выполнение коммита
           (cl-git-tree/loc:repo-commit ws :message "Initial commit")
           ;; Проверка веток
           (let ((branches (cl-git-tree/loc:repo-branches ws)))
             (is (listp branches))
             ;; В репозитории с коммитом должна быть хотя бы одна ветка (обычно master или main)
             (is (>= (length branches) 1))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для repo-commit

(def-suite workspace-repo-commit
  :description "Тесты для метода repo-commit"
  :in workspace)

(in-suite workspace-repo-commit)

(def-test repo-commit-basic ()
  "Проверка базового функционала repo-commit."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-commit"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir))
         (test-file (uiop:merge-pathnames* #P"test.txt" test-dir)))
    (unwind-protect
         (progn
           (cl-git-tree/loc:git-init ws)
           ;; Настройка git config для коммитов
           (cl-git-tree/git-utils:git-run test-dir "config" "user.email" "test@example.com")
           (cl-git-tree/git-utils:git-run test-dir "config" "user.name" "Test User")
           ;; Создание файла
           (with-open-file (f test-file :direction :output)
             (write-string "test content" f))
           ;; Добавление файла в индекс
           (cl-git-tree/git-utils:git-run test-dir "add" "test.txt")
           ;; Коммит
           (let ((result (cl-git-tree/loc:repo-commit ws :message "Test commit")))
             (is (stringp result))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

;;; Тесты для аксессоров

(def-suite workspace-accessors
  :description "Тесты для аксессоров workspace"
  :in workspace)

(in-suite workspace-accessors)

(def-test workspace-path-accessor ()
  "Проверка аксессора <workspace>-path."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-accessors"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is (pathnamep (cl-git-tree/loc:<workspace>-path ws)))
           (is (uiop:directory-exists-p (cl-git-tree/loc:<workspace>-path ws))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test workspace-description-accessor ()
  "Проверка аксессора <workspace>-description."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-desc"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir :description "Test Description")))
    (unwind-protect
         (is (string= (cl-git-tree/loc:<workspace>-description ws) "Test Description"))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test workspace-description-setf ()
  "Проверка установки description через setf."
  (let* ((test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-setf"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (setf (cl-git-tree/loc:<workspace>-description ws) "New Description")
           (is (string= (cl-git-tree/loc:<workspace>-description ws) "New Description")))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

#+nil
(progn
)
