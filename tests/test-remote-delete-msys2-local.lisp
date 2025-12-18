;;;; tests/test-remote-delete-msys2-local.lisp
;;;; Тест для специализированного метода remote-delete на MSYS2 + local

(in-package :cl-git-tree/tests)

(def-suite remote-delete-msys2-local-tests
  :description "Тесты для remote-delete (<workspace-msys2> <local>)"
  :in all)

(in-suite remote-delete-msys2-local-tests)

(def-test remote-delete-msys2-local-basic ()
  "Проверка, что remote-delete для (<workspace-msys2> <local>) удаляет bare-репозиторий через rm -r."
  (let* ((test-base (uiop:merge-pathnames* 
                     (make-pathname :directory '(:relative "test-remote-delete-msys2"))
                     (uiop:temporary-directory)))
         (ws-dir (uiop:merge-pathnames* 
                  (make-pathname :directory '(:relative "test-repo"))
                  test-base))
         (bare-base (uiop:merge-pathnames* 
                     (make-pathname :directory '(:relative "bare-repos"))
                     test-base))
         (bare-path (uiop:merge-pathnames* 
                     (make-pathname :directory '(:relative "test-repo.git"))
                     bare-base)))
    
    (unwind-protect
         (progn
           ;; 1. Создать workspace и bare-репозиторий
           (ensure-directories-exist ws-dir)
           (ensure-directories-exist bare-base)
           
           ;; Инициализировать workspace
           (uiop:run-program (list "git" "init")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           (uiop:run-program (list "git" "config" "user.name" "Test User")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           (uiop:run-program (list "git" "config" "user.email" "test@example.com")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           
           ;; Создать bare-репозиторий
           (uiop:run-program (list "git" "init" "--bare" (namestring bare-path))
                             :output :string
                             :error-output :string)
           
           (is-true (uiop:directory-exists-p bare-path) 
                    "Bare-репозиторий должен быть создан")
           
           ;; 2. Создать workspace и provider
           (let* ((ws (cl-git-tree/loc:make-workspace ws-dir))
                  (provider (make-instance 'cl-git-tree/loc:<local>
                              :id "test-local"
                              :url-git (namestring bare-base))))
             
             ;; Проверить, что workspace имеет правильный тип
             (is (typep ws 'cl-git-tree/loc:<workspace-msys2>)
                 "Workspace должен быть типа <workspace-msys2> на MSYS2")
             
             ;; 3. Добавить remote
             (let ((repo-name (cl-git-tree/loc:repo-name ws)))
               (uiop:run-program (list "git" "remote" "add" "test-local" 
                                       (format nil "~A~A.git" 
                                               (namestring bare-base) 
                                               repo-name))
                                 :directory ws-dir
                                 :output :string
                                 :error-output :string))
             
             ;; Проверить, что remote добавлен
             (let ((remotes (uiop:run-program (list "git" "remote")
                                              :directory ws-dir
                                              :output :string
                                              :error-output :string)))
               (is-true (search "test-local" remotes)
                        "Remote должен быть добавлен"))
             
             ;; 4. Вызвать remote-delete
             (cl-git-tree/loc:remote-delete ws provider)
             
             ;; 5. Проверить, что bare-репозиторий удалён
             (is-false (uiop:directory-exists-p bare-path)
                       "Bare-репозиторий должен быть удалён после remote-delete")
             
             ;; 6. Проверить, что remote удалён из конфигурации git
             (let ((remotes (uiop:run-program (list "git" "remote")
                                              :directory ws-dir
                                              :output :string
                                              :error-output :string
                                              :ignore-error-status t)))
               (is-false (search "test-local" remotes)
                         "Remote должен быть удалён из git config"))))
      
      ;; Cleanup
      (when (uiop:directory-exists-p test-base)
        (cl-git-tree/tests:force-delete-directory test-base)))))

(def-test remote-delete-msys2-local-nonexistent-bare ()
  "Проверка, что remote-delete не падает, если bare-репозитория уже нет."
  (let* ((test-base (uiop:merge-pathnames* 
                     (make-pathname :directory '(:relative "test-remote-delete-msys2-missing"))
                     (uiop:temporary-directory)))
         (ws-dir (uiop:merge-pathnames* 
                  (make-pathname :directory '(:relative "workspace"))
                  test-base))
         (bare-base (uiop:merge-pathnames* 
                     (make-pathname :directory '(:relative "bare-repos"))
                     test-base)))
    
    (unwind-protect
         (progn
           ;; Создать только workspace, без bare-репозитория
           (ensure-directories-exist ws-dir)
           (ensure-directories-exist bare-base)
           
           (uiop:run-program (list "git" "init")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           (uiop:run-program (list "git" "config" "user.name" "Test User")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           (uiop:run-program (list "git" "config" "user.email" "test@example.com")
                             :directory ws-dir
                             :output :string
                             :error-output :string)
           
           (let* ((ws (cl-git-tree/loc:make-workspace ws-dir))
                  (provider (make-instance 'cl-git-tree/loc:<local>
                              :id "test-local-missing"
                              :url-git (namestring bare-base))))
             
             ;; Добавить remote, но без создания bare-репозитория
             (uiop:run-program (list "git" "remote" "add" "test-local-missing" 
                                     (format nil "~Atest-repo.git" 
                                             (namestring bare-base)))
                               :directory ws-dir
                               :output :string
                               :error-output :string)
             
             ;; Вызвать remote-delete для несуществующего bare-репозитория
             ;; Не должно быть исключений
             (finishes (cl-git-tree/loc:remote-delete ws provider)
                       "remote-delete не должен падать на отсутствующем bare-репозитории")))
      
      ;; Cleanup
      (when (uiop:directory-exists-p test-base)
        (cl-git-tree/tests:force-delete-directory test-base)))))
