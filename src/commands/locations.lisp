;;;; ./src/commands/locations.lisp

(defpackage :cl-git-tree/commands/locations
  (:use :cl
        :cl-git-tree/loc)
  (:export cmd-locations))

(in-package :cl-git-tree/commands/locations)

(defun print-loc (loc-key)
  (let ((loc (find-location loc-key)))
    (if loc
  (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ : ~A~%   Provider: ~A~%   Args: ~A~%~%"
    loc-key
    (<location>-description loc)
    (<location>-url-git loc)
    (<location>-tar loc)
    (<location>-url-xz loc)
    (<location>-provider loc)
    (location->arg-string loc))
        (format t "Location ~A not found.~%" loc-key))))

(defun parse-location-args (pairs)
  "Парсит пары аргументов вида :url-git <val> :url-xz <val> и т.д.
Возвращает plist с ключами :url-git :url-xz :tar :description :provider."
  (let ((get-val (lambda (names)
                   (let ((res nil))
                     (loop for (k v . rest) on pairs by #'cddr do
                       (when v
                         (when (or (member k names :test #'string=)
                                   (and (> (length k) 0) 
                                        (member (concatenate 'string ":" k) names :test #'string=)))
                           (setf res v)
                           (return))))
                     res))))
    (list :url-git (funcall get-val '(":url-git" "url-git"))
          :url-xz  (funcall get-val '(":url-xz" "url-xz"))
          :tar     (funcall get-val '(":tar" "tar"))
          :description (funcall get-val '(":description" "description"))
          :provider (funcall get-val '(":provider" "provider")))))

(defun cmd-locations-help ()
  "Показывает справку по команде locations."
  (format t "Управление локациями (persistent).~%~%")
  (format t "Команды:~%")
  (format t "  locations list               — показать все локации~%")
  (format t "  locations show <key>         — показать подробности по ключу~%")
  (format t "  locations args <key>         — вывести CLI-аргументы для add/edit~%")
  (format t "  locations add <key> :url-git <url> [:url-xz <xz>] [:tar <tar>] [:description <desc>] [:provider <prov>] — добавить~%")
  (format t "  locations edit <key> [same args as add] — редактировать~%")
  (format t "  locations remove <key>       — удалить локацию~%")
  (format t "  locations save               — принудительно сохранить locations.lisp~%")
  (format t "  locations reset              — сбросить конфигурацию к дефолтным значениям (gh, lc)~%")
  (format t "  locations --help             — показать справку~%~%")
  (format t "Поддерживаемые провайдеры: local, github, gitlab~%~%")
  (format t "Current config file: ~S~%" cl-git-tree:*config-path*))

(defun cmd-locations-list ()
  "Показывает список всех локаций."
  (print-locations))

(defun cmd-locations-show (key)
  "Показывает подробности по указанному ключу локации."
  (print-loc key))

(defun cmd-locations-args (key)
  "Печатает CLI-аргументы для локации KEY (для копирования в add/edit)."
  (let ((loc (find-location key)))
    (if loc
        (format t "locations add ~A ~A~%" key (location->arg-string loc))
        (format t "Location ~A not found.~%" key))))

(defun cmd-locations-save ()
  "Сохраняет конфигурацию локаций."
  (cl-git-tree/loc:save-locations-config)
  (format t "Saved locations to ~S~%" cl-git-tree:*config-path*))

(defun cmd-locations-reset ()
  "Сбрасывает конфигурацию к дефолтным значениям."
  (cl-git-tree:reset-config)
  (clrhash cl-git-tree/loc:*locations*)
  (cl-git-tree:load-config)
  (format t "Конфигурация перезагружена с дефолтными значениями.~%"))

(defun cmd-locations-add (key args)
  "Добавляет новую локацию с ключом KEY и параметрами из ARGS."
  (when (null key)
    (format t "Usage: locations add <key> :url-git <url> [:url-xz <xz>] [:tar <tar>] [:description <desc>] [:provider <prov>]~%" )
    (format t "See: locations --help~%")
    (return-from cmd-locations-add))
  (let* ((parsed (parse-location-args args))
         (url-git (getf parsed :url-git))
         (url-xz  (getf parsed :url-xz))
         (tar     (getf parsed :tar))
         (desc    (getf parsed :description))
         (prov    (getf parsed :provider)))
    (add-location key :description desc :url-git url-git :url-xz url-xz :tar tar :provider prov)
    (cl-git-tree/loc:save-locations-config)
    (format t "Added and saved location ~A~%" key)))

(defun cmd-locations-edit (key args)
  "Редактирует существующую локацию с ключом KEY, обновляя параметры из ARGS."
  (when (null key)
    (format t "Usage: locations edit <key> :url-git <url> [:url-xz <xz>] [:tar <tar>] [:description <desc>] [:provider <prov>]~%")
    (format t "See: locations --help~%")
    (return-from cmd-locations-edit))
  (let* ((parsed (parse-location-args args))
         (url-git (getf parsed :url-git))
         (url-xz  (getf parsed :url-xz))
         (tar     (getf parsed :tar))
         (desc    (getf parsed :description))
         (prov    (getf parsed :provider)))
    (let ((loc (find-location key)))
      (unless loc (error "Location ~A not found" key))
      (when url-git (setf (<location>-url-git loc) url-git))
      (when url-xz  (setf (<location>-url-xz loc) url-xz))
      (when tar     (setf (<location>-tar loc) tar))
      (when desc    (setf (<location>-description loc) desc))
      (when prov    (setf (<location>-provider loc) prov))
      (register-location loc)
      (cl-git-tree/loc:save-locations-config)
      (format t "Edited and saved location ~A~%" key))))

(defun cmd-locations-remove (key)
  "Удаляет локацию с ключом KEY."
  (when (null key) (error "Specify key to remove"))
  (if (cl-git-tree/loc:remove-location key)
      (progn (cl-git-tree/loc:save-locations-config)
             (format t "Removed and saved, key=~A~%" key))
      (format t "Key ~A not found.~%" key)))

(defun cmd-locations (&rest args)
  "Управление локациями (persistent).

Команды:
  locations list               — показать все локации
  locations show <key>         — показать подробности по ключу
  locations args <key>         — вывести CLI-аргументы для add/edit
  locations add <key> :url-git <url> [:url-xz <xz>] [:tar <tar>] [:description <desc>] — добавить
  locations edit <key> [same args as add] — редактировать
  locations remove <key>       — удалить локацию
  locations save               — принудительно сохранить locations.lisp
  locations reset              — сбросить конфигурацию к дефолтным значениям (gh, lc)
  locations --help             — показать справку
"
  (cond
    ((or (null args) (member "--help" args :test #'string=))
     (cmd-locations-help))
    
    ((string= (first args) "list")
     (cmd-locations-list))
    
    ((and (string= (first args) "show") (second args))
     (cmd-locations-show (second args)))

    ((and (string= (first args) "args") (second args))
     (cmd-locations-args (second args)))
    
    ((string= (first args) "save")
     (cmd-locations-save))
    
    ((string= (first args) "reset")
     (cmd-locations-reset))
    
    ((string= (first args) "add")
     (cmd-locations-add (second args) (cddr args)))
    
    ((string= (first args) "edit")
     (cmd-locations-edit (second args) (cddr args)))
    
    ((string= (first args) "remove")
     (cmd-locations-remove (second args)))
    
    (t
     (format t "Unknown subcommand: ~A~%" (first args)))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "locations" #'cmd-locations "Управлять локациями (list/show/add/edit/remove/save/reset)"))
