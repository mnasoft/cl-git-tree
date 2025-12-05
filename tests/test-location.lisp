(in-package :cl-git-tree/tests)

(def-suite location
  :description "Тесты для подсистемы cl-git-tree/loc (провайдеры и методы)"
  :in all)

(in-suite location)

(def-test provider-stubs ()
  "Для абстрактного <provider> все методы должны возвращать строку \"неприменим\"."
  (let* ((p (make-instance 'cl-git-tree/loc:<provider>))
         (test-dir (uiop:merge-pathnames* 
                    (make-pathname :directory '(:relative "test-ws-provider"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is-true (search "неприменим" (cl-git-tree/loc:clone p "/tmp")))
           (is-true (search "неприменим" (cl-git-tree/loc:repo-create ws p)))
           (is-true (search "неприменим" (cl-git-tree/loc:repo-delete ws p)))
           #+nil
           (is-true (search "неприменим" (cl-git-tree/loc:repo-push ws p :branch "main")))
           #+nil
           (is-true (search "неприменим" (cl-git-tree/loc:repo-pull ws p :branch "main"))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

#+nil
(progn 
(def-test local-clone ()
  "Проверяем, что clone для <local> создаёт bare‑копию в указанном каталоге."
  (let ((loc (make-instance 'cl-git-tree/loc:<local>
               :id "local-test"
               :url-git "/tmp/source-dir")))
    (is-true (equal (pathname "/tmp/target-dir")
                    (progn
                      (cl-git-tree/loc:clone loc "/tmp/target-dir")
                      (pathname "/tmp/target-dir"))))))

(def-test local-push-pull ()
  "Для <local> операции push/pull не требуются и должны возвращать строку \"не требуется\"."
  (let ((loc (make-instance 'cl-git-tree/loc:<local>)))
    (is-true (search "не требуется" (cl-git-tree/loc:repo-push loc :branch "main")))
    (is-true (search "не требуется" (cl-git-tree/loc:repo-pull loc :branch "main")))))

(def-test github-methods ()
  "Для <github> методы repo-create, repo-push, repo-pull и repo-delete должны возвращать строку команды git."
  (let ((gh (make-instance 'cl-git-tree/loc:<github>
              :id "origin"
              :url-git "git@github.com:user/repo.git")))
    (is-true (stringp (cl-git-tree/loc:repo-create gh nil)))
    (is-true (stringp (cl-git-tree/loc:repo-push gh :branch "main")))
    (is-true (stringp (cl-git-tree/loc:repo-pull gh :branch "main")))
    (is-true (stringp (cl-git-tree/loc:repo-delete gh)))))

(def-test gitlab-methods ()
  "Для <gitlab> методы repo-create, repo-push, repo-pull и repo-delete должны возвращать строку команды git."
  (let ((gl (make-instance 'cl-git-tree/loc:<gitlab>
              :id "origin"
              :url-git "git@gitlab.com:user/repo.git")))
    (is-true (stringp (cl-git-tree/loc:repo-create gl nil)))
    (is-true (stringp (cl-git-tree/loc:repo-push gl :branch "main")))
    (is-true (stringp (cl-git-tree/loc:repo-pull gl :branch "main")))
    (is-true (stringp (cl-git-tree/loc:repo-delete gl)))))

;;; ----------------------------------------------------------------------
;;; Регистрация и поиск локации
;;; ----------------------------------------------------------------------

(def-test register-and-find-location ()
  "Проверяем, что register-location добавляет локацию и find-location её находит."
  (let ((loc (make-instance 'cl-git-tree/loc:<location>
                :id "test"
                :description "Тестовая локация"
                :provider :local
                :url-git #P"/tmp/repos/")))
    (cl-git-tree/loc:register-location loc)
    (let ((found (cl-git-tree/loc:find-location "test")))
      (is-true (typep found 'cl-git-tree/loc:<location>))
      (is-true (string= (cl-git-tree/loc:<location>-id found) "test"))
      (is-true (string= (cl-git-tree/loc:<location>-description found) "Тестовая локация")))))

(def-test duplicate-location-overwrites ()
  "Проверяем, что повторная регистрация с тем же id перезаписывает локацию."
  (let ((loc1 (make-instance 'cl-git-tree/loc:<location>
                 :id "dup"
                 :description "Первая версия"
                 :provider :local
                 :url-git #P"/tmp/one/"))
        (loc2 (make-instance 'cl-git-tree/loc:<location>
                 :id "dup"
                 :description "Вторая версия"
                 :provider :local
                 :url-git #P"/tmp/two/")))
    (cl-git-tree/loc:register-location loc1)
    (cl-git-tree/loc:register-location loc2)
    (let ((found (cl-git-tree/loc:find-location "dup")))
      (is-true (string= (cl-git-tree/loc:<location>-description found) "Вторая версия"))
      (is-true (equal (cl-git-tree/loc:<location>-url-git found) #P"/tmp/two/")))))

;;; ----------------------------------------------------------------------
;;; add-location: нормализация URL, инференция провайдера
;;; ----------------------------------------------------------------------

(def-test add-location-normalizes-trailing-slashes ()
  "Проверяем, что дублирующиеся завершающие слеши убираются."
  (cl-git-tree/loc:add-location "test-slash"
    :description "Test"
    :url-git "/tmp/repos//")
  (let ((loc (cl-git-tree/loc:find-location "test-slash")))
    (is-true (string= (cl-git-tree/loc:<location>-url-git loc) "/tmp/repos/"))))

(def-test add-location-normalizes-ssh-url ()
  "Проверяем, что SSH URL вида user@host:: нормализуется в user@host:/."
  (cl-git-tree/loc:add-location "test-ssh"
    :description "SSH"
    :url-git "git@github.com:user::")
  (let ((loc (cl-git-tree/loc:find-location "test-ssh")))
    (is-true (string= (cl-git-tree/loc:<location>-url-git loc) "git@github.com:user/"))))

(def-test add-location-accepts-name-as-description ()
  "Проверяем, что :name работает как алиас для :description."
  (cl-git-tree/loc:add-location "test-name"
    :name "My Name"
    :url-git "/tmp/test")
  (let ((loc (cl-git-tree/loc:find-location "test-name")))
    (is-true (string= (cl-git-tree/loc:<location>-description loc) "My Name"))))

(def-test add-location-infers-local-provider ()
  "Проверяем, что локальный путь автоматически определяется как :local."
  (cl-git-tree/loc:add-location "test-infer-local"
    :description "Local"
    :url-git "/home/user/repos")
  (let ((loc (cl-git-tree/loc:find-location "test-infer-local")))
    (is-true (eq (cl-git-tree/loc:<location>-provider loc) :local))
    (is-true (typep loc 'cl-git-tree/loc:<local>))))

(def-test add-location-infers-github-provider ()
  "Проверяем, что URL с github.com автоматически определяется как :github."
  (cl-git-tree/loc:add-location "test-infer-gh"
    :description "GitHub"
    :url-git "git@github.com:user/")
  (let ((loc (cl-git-tree/loc:find-location "test-infer-gh")))
    (is-true (eq (cl-git-tree/loc:<location>-provider loc) :github))
    (is-true (typep loc 'cl-git-tree/loc:<github>))))

(def-test add-location-infers-gitlab-provider ()
  "Проверяем, что URL с gitlab.com автоматически определяется как :gitlab."
  (cl-git-tree/loc:add-location "test-infer-gl"
    :description "GitLab"
    :url-git "git@gitlab.com:user/")
  (let ((loc (cl-git-tree/loc:find-location "test-infer-gl")))
    (is-true (eq (cl-git-tree/loc:<location>-provider loc) :gitlab))
    (is-true (typep loc 'cl-git-tree/loc:<gitlab>))))

(def-test add-location-registers-in-global-table ()
  "Проверяем, что add-location добавляет локацию в *locations*."
  (cl-git-tree/loc:add-location "test-register"
    :description "Test Registration"
    :url-git "/tmp/test")
  (is-true (cl-git-tree/loc:location-exists-p "test-register"))
  (is-true (member "test-register" (cl-git-tree/loc:all-location-keys) :test #'string=)))


(def-test all-locations-returns-list ()
  "Проверяем, что all-locations возвращает список зарегистрированных локаций."
  (let ((loc (make-instance 'cl-git-tree/loc:<location>
                :id "list-test"
                :description "Локация для списка"
                :provider :local
                :url-git #P"/tmp/list/")))
    (cl-git-tree/loc:register-location loc)
    (let ((all (cl-git-tree/loc:all-locations)))
      (is-true (listp all))
      (is-true (find "list-test" (mapcar #'cl-git-tree/loc:<location>-id all) :test #'string=)))))

)

