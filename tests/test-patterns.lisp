;;;; ./tests/test-patterns.lisp

(in-package :cl-git-tree/tests)

(defsuite patterns-tests "Tests for file-patterns persistence and API")

(def-test save-and-load-patterns
  (let* ((tmp-file (merge-pathnames #p"file-patterns.lisp"
                                    (make-pathname :directory (list :relative (format nil "test-pats-~A" (get-universal-time)))
                                                   :name ""
                                                   :type "")))
         (orig-path *file-patterns-path*))
    (unwind-protect
         (progn
           ;; redirect patterns path to temp location
           (setf *patterns-path* tmp-file)
           ;; ensure defaults
           (reset-to-defaults)
           (let ((initial (get-tracked-patterns)))
             (is (not (null initial)) "Initial tracked patterns should exist")
             ;; add a new pattern
             (let ((p "*.rs"))
               (add-tracked-pattern p)
               (is (member p (get-tracked-patterns) :test #'string=) "Pattern added")
               ;; reload from disk by reinitializing object
               (setf (tracked-patterns *file-patterns*) nil)
               (load-patterns)
               (is (member p (get-tracked-patterns) :test #'string=) "Pattern persisted after reload")
               ;; remove and check persisted removal
               (remove-tracked-pattern p)
               (setf (tracked-patterns *file-patterns*) nil)
               (load-patterns)
               (is (not (member p (get-tracked-patterns) :test #'string=)) "Pattern removed and change persisted"))))
      ;; cleanup
      (when (probe-file *patterns-path*)
        (delete-file *patterns-path*))
      (setf *patterns-path* orig-path))))
