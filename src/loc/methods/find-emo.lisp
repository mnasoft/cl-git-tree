(in-package :cl-git-tree/loc)

(defmethod find-emo ((ws <workspace>) key)
  (let ((emo (cl-git-tree/emodji:find-emodji key)))
    (if emo (cl-git-tree/emodji:<emodji>-color emo)
        "#")))

(defmethod find-emo ((ws <workspace-msys2>) key)
  (let ((emo (cl-git-tree/emodji:find-emodji key)))
    (if emo (cl-git-tree/emodji:<emodji>-mono emo)
        "#")))

#+nil
(defparameter *ws*
  (make-instance '<workspace> :path (truename ".")))


