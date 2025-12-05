;;;; ./tests/test-patterns.lisp

(in-package :cl-git-tree/tests)

(def-suite patterns-tests
  :description "Tests for file-patterns persistence and API"
  :in all)

(in-suite patterns-tests)

(def-test save-and-load-patterns ()
  "Проверяем, что паттерны сохраняются и загружаются из файла."
  (let* ((tmp-dir (uiop:ensure-directory-pathname
                   (merge-pathnames
                    (make-pathname :directory (list :relative (format nil "test-pats-~A" (get-universal-time))))
                    (uiop:temporary-directory))))
         (tmp-file (merge-pathnames "test-file-patterns.lisp" tmp-dir))
         (orig-path cl-git-tree/config:*patterns-path*))
    (ensure-directories-exist tmp-dir)
    (unwind-protect
         (progn
           ;; redirect patterns path to temp location
           (setf cl-git-tree/config:*patterns-path* tmp-file)
           ;; ensure defaults
           (cl-git-tree/config:reset-to-defaults)
           (let ((initial (cl-git-tree/config:get-tracked-patterns)))
             (is (not (null initial)) "Initial tracked patterns should exist")
             ;; add a new pattern
             (let ((p "*.rs"))
               (cl-git-tree/config:add-tracked-pattern p)
               (is (member p (cl-git-tree/config:get-tracked-patterns) :test #'string=) "Pattern added")
               ;; reload from disk by reinitializing object
               (setf (cl-git-tree/config:tracked-patterns cl-git-tree/config:*file-patterns*) nil)
               (cl-git-tree/config:load-patterns)
               (is (member p (cl-git-tree/config:get-tracked-patterns) :test #'string=) "Pattern persisted after reload")
               ;; remove and check persisted removal
               (cl-git-tree/config:remove-tracked-pattern p)
               (setf (cl-git-tree/config:tracked-patterns cl-git-tree/config:*file-patterns*) nil)
               (cl-git-tree/config:load-patterns)
               (is (not (member p (cl-git-tree/config:get-tracked-patterns) :test #'string=)) "Pattern removed and change persisted"))))
      ;; cleanup
      (when (probe-file cl-git-tree/config:*patterns-path*)
        (delete-file cl-git-tree/config:*patterns-path*))
      (setf cl-git-tree/config:*patterns-path* orig-path))))

