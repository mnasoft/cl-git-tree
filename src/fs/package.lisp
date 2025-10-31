(defpackage :cl-git-tree/fs
  (:use :cl)
  (:export find-git-repos
           repo-name
           git-repo-p
           with-each-repo
           with-each-repo-simple
           )
  (:import-from cl-git-tree
                *locations*
                location-url-git
                ))

(in-package :cl-git-tree/fs)
