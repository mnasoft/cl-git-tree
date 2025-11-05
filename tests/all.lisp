#+nil
(defpackage :cl-git-tree/tests/all
  (:use :cl :fiveam)
;;  (:import-from :cl-git-tree/tests/global    :global-tests)
;;  (:import-from :cl-git-tree/tests/workspace :workspace-tests)
;;  (:import-from :cl-git-tree/tests/location  :location-tests)
;;  (:import-from :cl-git-tree/tests/commands  :commands-tests)
;;  (:import-from :cl-git-tree/tests/fs        :fs-tests)
;;  (:import-from :cl-git-tree/tests/git-utils :git-utils-tests)
  )
#+nil
(in-package :cl-git-tree/tests/all)

(in-package :cl-git-tree/tests)

(def-suite all
  :description "Запускает все тесты cl-git-tree")

(in-suite all)

;; Подключаем все подсuites
;;(add-suite global-tests)
;;(add-suite workspace-tests)
;;(add-suite location-tests)
;;(add-suite commands-tests)
;;(add-suite fs-tests)
;;(add-suite git-utils-tests)

#+nil (defun run-all-tests ()
        "Запускает все тесты проекта."
        (fiveam:run! 'all-tests))
