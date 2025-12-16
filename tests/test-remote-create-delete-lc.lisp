;;;; tests/test-remote-create-delete-lc.lisp
;;;; Тесты для создания и удаления bare-репозитория в локации lc

(in-package :cl-git-tree/tests)

(def-suite remote-create-delete-lc-tests
  :description "Тесты для создания и удаления репозиториев в локации lc"
  :in all)

(in-suite remote-create-delete-lc-tests)

(deftest remote-create-lc-test
    (:fixture test-fixture)
  "Проверка создания bare-репозитория в локации lc"
  (let ((lc-loc (cl-git-tree/loc:find-location "lc")))
    ;; Убедимся, что локация существует
    (is-true lc-loc "Локация 'lc' должна существовать")
    (is (eq (cl-git-tree/loc:<location>-provider lc-loc) :LOCAL)
        "Провайдер 'lc' должен быть :LOCAL")
    
    ;; Создать репозиторий
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run 
         (cl-git-tree/loc:git-root 
          (cl-git-tree/loc:make-workspace (uiop:getcwd)))
         "remote" "create" "lc")
      (is (zerop code) "Команда 'remote create lc' должна выполниться успешно"))
    
    ;; Проверить, что remote добавлен
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run 
         (cl-git-tree/loc:git-root 
          (cl-git-tree/loc:make-workspace (uiop:getcwd)))
         "remote" "-v")
      (is (search "lc" out) "Remote 'lc' должен быть в списке remotes"))
    
    ;; Проверить корректный URL
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run 
         (cl-git-tree/loc:git-root 
          (cl-git-tree/loc:make-workspace (uiop:getcwd)))
         "config" "--get" "remote.lc.url")
      (is (search "/.git-tree/git/lc/" out) 
          "URL remote 'lc' должен содержать /.git-tree/git/lc/"))
    
    ;; Удалить репозиторий
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run 
         (cl-git-tree/loc:git-root 
          (cl-git-tree/loc:make-workspace (uiop:getcwd)))
         "remote" "remove" "lc")
      (is (zerop code) "Команда 'remote remove lc' должна выполниться успешно"))
    
    ;; Проверить, что remote удалён
    (multiple-value-bind (out err code)
        (cl-git-tree/git-utils:git-run 
         (cl-git-tree/loc:git-root 
          (cl-git-tree/loc:make-workspace (uiop:getcwd)))
         "remote" "-v")
      (is (not (search "lc" out)) "Remote 'lc' должен быть удалён"))))

(deftest remote-create-lc-url-format-test
    (:fixture test-fixture)
  "Проверка правильного формата URL для локации lc"
  (let* ((lc-loc (cl-git-tree/loc:find-location "lc")))
    (is (stringp (cl-git-tree/loc:<location>-url-git lc-loc)) 
        "URL-GIT должен быть строкой")
    (is (search "/.git-tree/git/lc" (cl-git-tree/loc:<location>-url-git lc-loc)) 
        "URL-GIT должен содержать /.git-tree/git/lc")
    (is (uiop:string-suffix-p "/" (cl-git-tree/loc:<location>-url-git lc-loc)) 
        "URL-GIT должен заканчиваться на /")))
