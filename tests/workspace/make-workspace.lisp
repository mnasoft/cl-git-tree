(in-package :cl-git-tree/tests)

(def-suite make-workspace
  :description "Тесты для подсистемы cl-git-tree/workspace"
  :in workspace)

(in-suite make-workspace)

(def-test create-with-explicit-description ()
  "Создание workspace с явным описанием."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ws-create-explicit"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir :description "Тестовый WS")))
    (unwind-protect
         (progn
           (is (typep ws 'cl-git-tree/loc:<workspace>))
           (is (equal (namestring (cl-git-tree/loc:<workspace>-path ws))
                      (namestring (truename test-dir))))
           (is (string= (cl-git-tree/loc:<workspace>-description ws)
                        "Тестовый WS")))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test create-without-description ()
  "Создание workspace без описания — берётся имя каталога."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ws-create-no-desc"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is (typep ws 'cl-git-tree/loc:<workspace>))
           (is (string= (cl-git-tree/loc:<workspace>-description ws)
                        (car (last (pathname-directory (truename test-dir)))))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test description-type ()
  "Описание всегда строка."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ws-desc-type"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is (stringp (cl-git-tree/loc:<workspace>-description ws))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))

(def-test path-type ()
  "Path всегда pathname."
  (let* ((test-dir (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "test-ws-path-type"))
                    (uiop:temporary-directory)))
         (ws (cl-git-tree/loc:make-workspace test-dir)))
    (unwind-protect
         (progn
           (is (pathnamep (cl-git-tree/loc:<workspace>-path ws))))
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))
