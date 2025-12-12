;;;; ./src/commands/locations.lisp

(defpackage :cl-git-tree/commands/locations
  (:use :cl
        :cl-git-tree/loc)
  (:export cmd-locations))

(in-package :cl-git-tree/commands/locations)

(defun print-loc (loc-key)
  (let ((loc (find-location loc-key)))
    (if loc
        (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ : ~A~%~%"
                loc-key
                (<location>-description loc)
                (<location>-url-git loc)
                (<location>-tar loc)
                (<location>-url-xz loc))
        (format t "Location ~A not found.~%" loc-key))))

(defun cmd-locations (&rest args)
  "Управление локациями (persistent).

Команды:
  locations list               — показать все локации
  locations show <key>         — показать подробности по ключу
  locations add <key> :url-git <url> [:url-xz <xz>] [:tar <tar>] [:description <desc>] — добавить
  locations edit <key> [same args as add] — редактировать
  locations remove <key>       — удалить локацию
  locations save                — принудительно сохранить locations.lisp
  locations --help             — показать справку
"
  (cond
    ((or (null args) (member "--help" args :test #'string=))
     (format t "~A~%" (documentation #'cmd-locations 'function))
     (format t "Current config file: ~S~%" cl-git-tree:*config-path*))
    ((string= (first args) "list")
     (print-locations))
    ((and (string= (first args) "show") (second args))
     (print-loc (second args)))
    ((string= (first args) "save")
     (cl-git-tree/loc:save-locations-config)
     (format t "Saved locations to ~S~%" cl-git-tree:*config-path*))
    ((string= (first args) "add")
     (let ((key (second args)))
       (when (null key) (error "Specify key to add"))
       (let* ((pairs (cddr args))
              (get-val (lambda (names)
                         (let ((res nil))
                           (loop for (k v . rest) on pairs by #'cddr do
                             (when v
                               (when (or (member k names :test #'string=)
                                         (and (> (length k) 0) (member (concatenate 'string ":" k) names :test #'string=)))
                                 (setf res v)
                                 (return))))
                           res)))
              (url-git (funcall get-val '(":url-git" "url-git")))
              (url-xz  (funcall get-val '(":url-xz" "url-xz")))
              (tar     (funcall get-val '(":tar" "tar")))
              (desc    (funcall get-val '(":description" "description"))))
         (add-location key :description desc :url-git url-git :url-xz url-xz :tar tar)
         (cl-git-tree/loc:save-locations-config)
         (format t "Added and saved location ~A~%" key))))
    ((string= (first args) "edit")
     (let ((key (second args)))
       (when (null key) (error "Specify key to edit"))
       (let* ((pairs (cddr args))
              (get-val (lambda (names)
                         (let ((res nil))
                           (loop for (k v . rest) on pairs by #'cddr do
                             (when v
                               (when (or (member k names :test #'string=)
                                         (and (> (length k) 0) (member (concatenate 'string ":" k) names :test #'string=)))
                                 (setf res v)
                                 (return))))
                           res)))
              (url-git (funcall get-val '(":url-git" "url-git")))
              (url-xz  (funcall get-val '(":url-xz" "url-xz")))
              (tar     (funcall get-val '(":tar" "tar")))
              (desc    (funcall get-val '(":description" "description"))))
         (let ((loc (find-location key)))
           (unless loc (error "Location ~A not found" key))
           (when url-git (setf (<location>-url-git loc) url-git))
           (when url-xz  (setf (<location>-url-xz loc) url-xz))
           (when tar     (setf (<location>-tar loc) tar))
           (when desc    (setf (<location>-description loc) desc))
           (register-location loc)
           (cl-git-tree/loc:save-locations-config)
           (format t "Edited and saved location ~A~%" key)))))
        ((string= (first args) "remove")
         (let ((key (second args)))
      (when (null key) (error "Specify key to remove"))
      (if (cl-git-tree/loc:remove-location key)
          (progn (cl-git-tree/loc:save-locations-config)
            (format t "Removed and saved, key=~A~%" key))
          (format t "Key ~A not found.~%" key))))
    (t
     (format t "Unknown subcommand: ~A~%" (first args)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "locations" #'cmd-locations "Управлять локациями (list/show/add/edit/remove/save)"))
