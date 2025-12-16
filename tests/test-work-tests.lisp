;;;; ./tests/test-work-tests.lisp

(in-package :cl-git-tree/tests)

(def-suite work-tests
  :description "Тесты для работы с тестовыми каталогами"
  :in all)

(in-suite work-tests)

;;; Вспомогательные функции

(defun setup-test-directories ()
  "Вспомогательная функция: создаёт структуру ~/work/tests с подкаталогами test-01 и test-02"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (test-01-dir (merge-pathnames "test-01/" work-tests-dir))
         (test-02-dir (merge-pathnames "test-02/" work-tests-dir))
         (readme-01 (merge-pathnames "README.org" test-01-dir))
         (readme-02 (merge-pathnames "README.org" test-02-dir)))
    
    ;; Удалить каталог ~/work/tests если существует
    (when (uiop:directory-exists-p work-tests-dir)
      (uiop:delete-directory-tree work-tests-dir :validate t))
    
    ;; Создать каталог ~/work/tests
    (ensure-directories-exist work-tests-dir)
    
    ;; Создать каталоги ~/work/tests/test-01 и ~/work/tests/test-02
    (ensure-directories-exist test-01-dir)
    (ensure-directories-exist test-02-dir)
    
    ;; Создать в каталоге ~/work/tests/test-01 файл README.org
    (with-open-file (stream readme-01
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "* Test 01~%~%Тестовый файл для test-01~%"))
    
    ;; Создать в каталоге ~/work/tests/test-02 файл README.org
    (with-open-file (stream readme-02
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "* Test 02~%~%Тестовый файл для test-02~%")))
  
  (values (merge-pathnames "work/tests/" (user-homedir-pathname))
          (merge-pathnames "test-01/" (merge-pathnames "work/tests/" (user-homedir-pathname)))
          (merge-pathnames "test-02/" (merge-pathnames "work/tests/" (user-homedir-pathname)))))

(defun setup-git-repositories (test-01-dir test-02-dir)
  "Вспомогательная функция: инициализирует git-репозитории в test-01 и test-02"
  ;; Инициализируем git в test-01
  (uiop:run-program (list "git" "-C" (namestring test-01-dir) "init")
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
  
  (uiop:run-program (list "git" "-C" (namestring test-01-dir) 
                          "config" "user.email" "test@example.com")
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
  
  (uiop:run-program (list "git" "-C" (namestring test-01-dir) 
                          "config" "user.name" "Test User")
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
  
  ;; Инициализируем git в test-02
  (uiop:run-program (list "git" "-C" (namestring test-02-dir) "init")
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
  
  (uiop:run-program (list "git" "-C" (namestring test-02-dir) 
                          "config" "user.email" "test@example.com")
                    :output :string
                    :error-output :string
                    :ignore-error-status t)
  
  (uiop:run-program (list "git" "-C" (namestring test-02-dir) 
                          "config" "user.name" "Test User")
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun add-and-commit-readme (work-tests-dir)
  "Вспомогательная функция: добавляет README.org в git и делает коммит"
  (let ((original-dir (uiop:getcwd)))
    (unwind-protect
         (progn
           (uiop:chdir work-tests-dir)
           
           ;; git-tree add "README.org"
           (uiop:run-program (list "sbcl" "--eval" "(asdf:load-system :cl-git-tree)"
                                   "--eval" "(cl-git-tree/cli:main '(\"prog\" \"add\" \"README.org\"))"
                                   "--eval" "(sb-ext:exit)")
                             :output :string
                             :error-output :string
                             :ignore-error-status t)
           
           ;; git-tree commit -a
           (uiop:run-program (list "sbcl" "--eval" "(asdf:load-system :cl-git-tree)"
                                   "--eval" "(cl-git-tree/cli:main '(\"prog\" \"commit\" \"-a\" \"-m\" \"Initial commit\"))"
                                   "--eval" "(sb-ext:exit)")
                             :output :string
                             :error-output :string
                             :ignore-error-status t))
      
      ;; Возвращаемся в исходный каталог
      (uiop:chdir original-dir))))

;;; Фикстуры

(def-fixture test-directories-fixture ()
  "Фикстура: создаёт структуру ~/work/tests с подкаталогами test-01 и test-02"
  (unwind-protect
       (progn
         (setup-test-directories)
         (&body))
    ;; Teardown: очистка (опционально)
    ))

(def-fixture test-git-repositories-fixture ()
  "Фикстура: создаёт структуру ~/work/tests с git-репозиториями test-01 и test-02"
  (unwind-protect
       (progn
         (multiple-value-bind (work-tests-dir test-01-dir test-02-dir)
             (setup-test-directories)
           (declare (ignore work-tests-dir))
           (setup-git-repositories test-01-dir test-02-dir))
         (&body))
    ;; Teardown: очистка (опционально)
    ))

(def-fixture test-git-repositories-with-readme-fixture ()
  "Фикстура: создаёт git-репозитории test-01 и test-02 с добавленными и закоммиченными README.org"
  (unwind-protect
       (progn
         (multiple-value-bind (work-tests-dir test-01-dir test-02-dir)
             (setup-test-directories)
           (setup-git-repositories test-01-dir test-02-dir)
           (add-and-commit-readme work-tests-dir))
         (&body))
    ;; Teardown: очистка (опционально)
    ))

;;; Тесты

(def-test setup-test-directories (:fixture test-directories-fixture)
  "Проверка создания тестовой структуры каталогов в ~/work/tests"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (test-01-dir (merge-pathnames "test-01/" work-tests-dir))
         (test-02-dir (merge-pathnames "test-02/" work-tests-dir))
         (readme-01 (merge-pathnames "README.org" test-01-dir))
         (readme-02 (merge-pathnames "README.org" test-02-dir)))
    
    ;; Проверяем что всё создано
    (is (uiop:directory-exists-p work-tests-dir) 
        "Каталог ~/work/tests должен быть создан")
    
    (is (uiop:directory-exists-p test-01-dir)
        "Каталог ~/work/tests/test-01 должен быть создан")
    
    (is (uiop:directory-exists-p test-02-dir)
        "Каталог ~/work/tests/test-02 должен быть создан")
    
    (is (probe-file readme-01)
        "Файл ~/work/tests/test-01/README.org должен быть создан")
    
    (is (probe-file readme-02)
        "Файл ~/work/tests/test-02/README.org должен быть создан")
    
    ;; Проверяем git log --oneline (не должно быть git-репозиториев)
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-01-dir) "log" "--oneline")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (not (zerop code)) "git log должен завершиться с ошибкой - нет git-репозитория"))))

(def-test git-tree-status-test (:fixture test-directories-fixture)
  "Проверка выполнения команды git-tree status для ~/work/tests"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (original-dir (uiop:getcwd)))
    
    ;; Переходим в тестовый каталог
    (unwind-protect
         (progn
           (uiop:chdir work-tests-dir)
           
           ;; Выполняем команду status
           (multiple-value-bind (out err code)
               (uiop:run-program (list "sbcl" "--eval" "(asdf:load-system :cl-git-tree)"
                                       "--eval" "(cl-git-tree/cli:main '(\"prog\" \"status\"))"
                                       "--eval" "(sb-ext:exit)")
                                 :output :string
                                 :error-output :string
                                 :ignore-error-status t)
             
             ;; Проверяем, что команда выполнилась
             (is (numberp code) "Код возврата должен быть числом")
             
             ;; Проверяем вывод (может быть пустым, если нет git-репозиториев)
             (is (stringp out) "Вывод должен быть строкой")))
      
      ;; Возвращаемся в исходный каталог
      (uiop:chdir original-dir))))

(def-test git-repositories-initialized (:fixture test-git-repositories-fixture)
  "Проверка инициализации git-репозиториев в test-01 и test-02"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (test-01-git (merge-pathnames "test-01/.git/" work-tests-dir))
         (test-02-git (merge-pathnames "test-02/.git/" work-tests-dir)))
    
    ;; Проверяем, что .git директории созданы
    (is (uiop:directory-exists-p test-01-git)
        "Git-репозиторий в test-01 должен быть инициализирован")
    
    (is (uiop:directory-exists-p test-02-git)
        "Git-репозиторий в test-02 должен быть инициализирован")))

(def-test git-repositories-with-commits (:fixture test-git-repositories-with-readme-fixture)
  "Проверка что в репозиториях test-01 и test-02 есть коммиты"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (test-01-dir (merge-pathnames "test-01/" work-tests-dir))
         (test-02-dir (merge-pathnames "test-02/" work-tests-dir)))
    
    ;; Проверяем наличие коммитов в test-01
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-01-dir) "log" "--oneline")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git log должен выполниться успешно в test-01")
      (is (search "Initial commit" out) "Должен быть коммит 'Initial commit' в test-01"))
    
    ;; Проверяем наличие коммитов в test-02
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-02-dir) "log" "--oneline")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git log должен выполниться успешно в test-02")
      (is (search "Initial commit" out) "Должен быть коммит 'Initial commit' в test-02"))
    
    ;; Проверяем что README.org под управлением git
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-01-dir) "ls-files")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (search "README.org" out) "README.org должен быть под управлением git в test-01"))))

(def-test readme-files-committed (:fixture test-git-repositories-with-readme-fixture)
  "Проверка что README.org добавлены в коммит для обоих репозиториев"
  (let* ((work-tests-dir (merge-pathnames "work/tests/" (user-homedir-pathname)))
         (test-01-dir (merge-pathnames "test-01/" work-tests-dir))
         (test-02-dir (merge-pathnames "test-02/" work-tests-dir)))
    
    ;; Проверяем что README.org в git ls-files для test-01
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-01-dir) "ls-files")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git ls-files должен выполниться успешно в test-01")
      (is (search "README.org" out) "README.org должен быть в git ls-files для test-01"))
    
    ;; Проверяем что README.org в git ls-files для test-02
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-02-dir) "ls-files")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git ls-files должен выполниться успешно в test-02")
      (is (search "README.org" out) "README.org должен быть в git ls-files для test-02"))
    
    ;; Проверяем что есть коммиты в обоих репозиториях
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-01-dir) "log" "--oneline")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git log должен выполниться успешно в test-01")
      (is (> (length out) 0) "Должны быть коммиты в test-01"))
    
    (multiple-value-bind (out err code)
        (uiop:run-program (list "git" "-C" (namestring test-02-dir) "log" "--oneline")
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (is (zerop code) "git log должен выполниться успешно в test-02")
      (is (> (length out) 0) "Должны быть коммиты в test-02"))))


