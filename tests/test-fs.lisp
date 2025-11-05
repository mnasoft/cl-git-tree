(defpackage :cl-git-tree/tests/fs
  (:use :cl :fiveam
        :cl-git-tree/fs))

(in-package :cl-git-tree/tests/fs)

(defsuite fs-tests
  :description "Тесты для подсистемы cl-git-tree/fs")

(in-suite fs-tests)

(test scan-tree-finds-git-repo
  "Проверяем, что scan-tree находит .git в подкаталоге."
  (uiop:with-temporary-directory (tmpdir)
    ;; создаём подкаталог и инициализируем git
    (let* ((repo-dir (merge-pathnames "repo1/" tmpdir)))
      (ensure-directories-exist repo-dir)
      (cl-git-tree/git-utils:git-run repo-dir "init")
      ;; теперь сканируем tmpdir
      (let ((repos (scan-tree tmpdir)))
        (is (find repo-dir repos :test #'equal))))))
        
(test repo-exists-p-works
  "Проверяем, что repo-exists-p возвращает T для git-репозитория."
  (uiop:with-temporary-directory (tmpdir)
    (cl-git-tree/git-utils:git-run tmpdir "init")
    (is (repo-exists-p tmpdir))
    ;; для пустого каталога должно быть NIL
    (let ((empty-dir (merge-pathnames "empty/" tmpdir)))
      (ensure-directories-exist empty-dir)
      (is (not (repo-exists-p empty-dir))))))

(test list-repos-multiple
  "Проверяем, что list-repos возвращает все репозитории в дереве."
  (uiop:with-temporary-directory (tmpdir)
    (let ((repo-a (merge-pathnames "a/" tmpdir))
          (repo-b (merge-pathnames "b/" tmpdir)))
      (ensure-directories-exist repo-a)
      (ensure-directories-exist repo-b)
      (cl-git-tree/git-utils:git-run repo-a "init")
      (cl-git-tree/git-utils:git-run repo-b "init")
      (let ((repos (list-repos tmpdir)))
        (is (= (length repos) 2))
        (is (every #'repo-exists-p repos))))))
