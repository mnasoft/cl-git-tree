(in-package :cl-git-tree/fs)

(defun repo-name (repo-dir)
  "Возвращает имя каталога репозитория без завершающего /."
  (namestring
   (path:basename
    (string-right-trim "/" (namestring (truename repo-dir))))))

(defun git-repo-p (dir)
  "Возвращает T, если в DIR есть подкаталог .git."
  (probe-file (merge-pathnames ".git/" dir)))

(defun find-git-repos (&optional (root (truename ".")))
  "Рекурсивно ищет все каталоги, содержащие подкаталог .git, начиная с ROOT."
  (labels ((scan (dir)
             (let ((results '()))
               (when (git-repo-p dir) (push dir results))
               (dolist (sub (directory (merge-pathnames "*/" dir)))
                 (unless (string= (file-namestring sub) ".git/")
                   (setf results (nconc results (scan sub)))))
               results)))
    (scan root)))

(defun with-each-repo (loc-key fn)
  "Находит все git-репозитории для loc-key и вызывает FN для каждого.
FN вызывается с аргументами (repo-dir loc-key base-url)."
  (let* ((loc (gethash loc-key *locations*))
         (base-url (and loc (location-url-git loc))))
    (if (null base-url)
        (format t "Неизвестная локация: ~A~%" loc-key)
        (dolist (repo-dir (find-git-repos))
          (funcall fn repo-dir loc-key base-url)))))

(defun with-each-repo-simple (fn)
  "Вызывает FN для каждого найденного git-репозитория.
FN вызывается с одним аргументом (repo-dir)."
  (dolist (repo-dir (find-git-repos))
    (funcall fn repo-dir)))
