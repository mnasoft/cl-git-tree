(in-package :cl-git-tree/loc)

(defmethod print-object ((ws <workspace>) stream)
  "Красивый вывод объекта <workspace> в REPL.
   Показывает path, description, git-root и repo-name."
  (print-unreadable-object (ws stream :type t :identity t)
    (format stream "path: ~S~%git-root: ~S~%repo-name: ~S~%desc: ~S"
            (<workspace>-path ws)
            (git-root ws)
            (repo-name ws)
            (<workspace>-description ws))))

(defmethod print-object ((loc <location>) stream)
  (print-unreadable-object (loc stream :type t :identity t)
    (format stream "id=~S provider=~S url-git=~S url-xz=~S tar=~S desc=~S"
            (if (slot-boundp loc 'id) (<location>-id loc) :unbound)
            (if (slot-boundp loc 'provider) (<location>-provider loc) :unbound)
            (if (slot-boundp loc 'url-git) (<location>-url-git loc) :unbound)
            (if (slot-boundp loc 'url-xz) (<location>-url-xz loc) :unbound)
            (if (slot-boundp loc 'tar) (<location>-tar loc) :unbound)
            (if (slot-boundp loc 'description) (<location>-description loc) :unbound))))

(defmethod print-object ((loc <local>) stream)
  (print-unreadable-object (loc stream :type t :identity t)
    (format stream "id=~S provider=~S url-git=~S desc=~S"
            (if (slot-boundp loc 'id) (<location>-id loc) :unbound)
            (if (slot-boundp loc 'provider) (<location>-provider loc) :unbound)            
            (if (slot-boundp loc 'url-git) (<location>-url-git loc) :unbound)
            (if (slot-boundp loc 'description) (<location>-description loc) :unbound))))

(defmethod print-object ((loc <github>) stream)
  (print-unreadable-object (loc stream :type t :identity t)
    (format stream "id=~S provider=~S url-git=~S desc=~S"
            (if (slot-boundp loc 'id) (<location>-id loc) :unbound)
            (if (slot-boundp loc 'provider) (<location>-provider loc) :unbound)
            (if (slot-boundp loc 'url-git) (<location>-url-git loc) :unbound)
            (if (slot-boundp loc 'description) (<location>-description loc) :unbound))))

(defmethod print-object ((loc <gitlab>) stream)
  (print-unreadable-object (loc stream :type t :identity t)
    (format stream "id=~S provider=~S url-git=~S desc=~S"
            (if (slot-boundp loc 'id) (<location>-id loc) :unbound)
            (if (slot-boundp loc 'provider) (<location>-provider loc) :unbound)
            (if (slot-boundp loc 'url-git) (<location>-url-git loc) :unbound)
            (if (slot-boundp loc 'description) (<location>-description loc) :unbound))))


(make-instance '<local> :id "pp")
