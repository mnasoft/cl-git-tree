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
  locations remove <key>       — удалить (не разрешено для 'pp' и 'pz')
  locations save                — принудительно сохранить locations.configure
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
       (let ((url-git (getf (loop for x on (cddr args) collect (first x) and (second x)) :url-git))
             (url-xz  (getf (loop for x on (cddr args) collect (first x) and (second x)) :url-xz))
             (tar     (getf (loop for x on (cddr args) collect (first x) and (second x)) :tar))
             (desc    (getf (loop for x on (cddr args) collect (first x) and (second x)) :description)))
         (add-location key :description desc :url-git url-git :url-xz url-xz :tar tar)
         (cl-git-tree/loc:save-locations-config)
         (format t "Added and saved location ~A~%" key))))
    ((string= (first args) "edit")
     (let ((key (second args)))
       (when (null key) (error "Specify key to edit"))
       (let* ((pairs (loop for x on (cddr args) collect (first x) and (second x)))
              (url-git (getf pairs :url-git))
              (url-xz  (getf pairs :url-xz))
              (tar     (getf pairs :tar))
              (desc    (getf pairs :description)))
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
       (when (member key '("pp" "pz") :test #'string=)
         (error "Removing built-in template ~A is not allowed" key))
       (if (cl-git-tree/loc:remove-location key)
           (progn (cl-git-tree/loc:save-locations-config)
                  (format t "Removed and saved, key=~A~%" key))
           (format t "Key ~A not found.~%" key))))
    (t
     (format t "Unknown subcommand: ~A~%" (first args)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "locations" #'cmd-locations "Управлять локациями (list/show/add/edit/remove/save)"))
