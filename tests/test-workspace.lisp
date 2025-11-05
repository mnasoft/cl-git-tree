(defpackage :cl-git-tree/tests/workspace
  (:use :cl :fiveam
        :cl-git-tree/workspace
        :cl-git-tree/git-utils))

(in-package :cl-git-tree/tests/workspace)

(defsuite workspace-tests
  :description "Тесты для подсистемы cl-git-tree/workspace")

(in-suite workspace-tests)

(test repo-status-empty
  "Проверяем, что repo-status работает на пустом репозитории."
  (uiop:with-temporary-directory (tmpdir)
    (git-run tmpdir "init")
    (let ((ws (make-instance '<workspace>
                 :path tmpdir
                 :repo-name "tmp")))
      (let ((out (repo-status ws)))
        (is (search "On branch" out))))))

(test repo-commit-and-branches
  "Создаём коммит и проверяем, что он появляется в истории, а ветка master/main существует."
  (uiop:with-temporary-directory (tmpdir)
    (git-run tmpdir "init")
    ;; создаём файл
    (with-open-file (s (merge-pathnames "foo.txt" tmpdir)
                       :direction :output :if-exists :supersede)
      (write-line "hello" s))
    ;; добавляем и коммитим
    (git-run tmpdir "add" "foo.txt")
    (let ((ws (make-instance '<workspace>
                 :path tmpdir
                 :repo-name "tmp")))
      (repo-commit ws "init commit")
      ;; проверяем, что коммит есть в логе
      (let ((log (git-run tmpdir "log" "--oneline")))
        (is (search "init commit" log)))
      ;; проверяем, что есть хотя бы одна ветка
      (let ((branches (repo-branches ws)))
        (is (listp branches))
        (is (some (lambda (b)
                    (or (string= b "master")
                        (string= b "main")))
                  branches))))))

(test repo-branches-empty
  "Проверяем, что repo-branches возвращает список даже без коммитов."
  (uiop:with-temporary-directory (tmpdir)
    (git-run tmpdir "init")
    (let ((ws (make-instance '<workspace>
                 :path tmpdir
                 :repo-name "tmp")))
      (let ((branches (repo-branches ws)))
        ;; В свежем репозитории git branch --list может вернуть пустой список
        (is (listp branches))))))
