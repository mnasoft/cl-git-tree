;;;; ./src/commands/commit.lisp

(defpackage :cl-git-tree/commands/commit
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git
                )
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo
                with-each-repo-simple
                )
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes
                )
  (:export run
           ))

(in-package :cl-git-tree/commands/commit)

(defun commit-repo (repo-dir message)
  "Делает git commit -am MESSAGE в указанном репо."
  (multiple-value-bind (_out err code)
      (git-run repo-dir "commit" "-am" message)
    (declare (ignore _out))
    (if (zerop code)
        (format t "✔ ~A: commit ~A~%" repo-dir message)
        (format t "❌ ~A: ошибка commit:~%~A" repo-dir err))))

(defun run (&rest args)
  "Если MESSAGE отсутствует, используем текущую дату."
  (let ((message (if args (format nil "~{~A~^ ~}" args)
                     (multiple-value-bind (sec min hour day mon year)
                         (decode-universal-time (get-universal-time))
                       (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                               year mon day hour min sec)))))
    (with-each-repo-simple #'(lambda (repo-dir)
                               (commit-repo repo-dir message)))))

(push (cons "commit" #'run) cl-git-tree:*commands*)
