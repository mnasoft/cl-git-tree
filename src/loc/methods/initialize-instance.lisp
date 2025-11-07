(in-package :cl-git-tree/loc)

(defparameter *github-user* "mnasoft")

(defmethod initialize-instance :after ((loc <github>) &key)
  (unless (<location>-url-git loc)
    (let ((user (or (slot-value loc 'user) *github-user*)))
      (setf (<location>-url-git loc)
            (format nil "git@github.com:~A/" user)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gitlab-user* "mnas")

(defmethod initialize-instance :after ((loc <gitlab>) &key)
  (unless (<location>-url-git loc)
    (let ((user (or (slot-value loc 'user) *gitlab-user*)))
      (setf (<location>-url-git loc)
            (format nil "git@gitlab.com:~A/" user)))))

