(in-package :cl-git-tree/loc)

(defclass <local> (<provider> <location>) ()
  (:default-initargs
    :provider :local
    :id "lc")
  (:documentation
   "Локация для локальных репозиториев.
    По умолчанию имеет идентификатор \"lc\" и провайдера :local."))

(defmethod initialize-instance :after ((loc <local>) &key)
  "Автоматически задаёт url-git для <local> на основе id.
   Формат: ~/.git-tree/git/<id>/"
  (unless (<location>-url-git loc)
    (setf (<location>-url-git loc)
          (merge-pathnames
           (make-pathname :directory `(:relative ".git-tree" "git" ,(<location>-id loc)))
           (user-homedir-pathname)))))

(defclass <github> (<provider> <location>)
  ((user
    :initarg :user
    :initform nil
    :accessor <github>-user
    :type (or null string)
    :documentation "Имя пользователя/организации на GitHub."))
  (:default-initargs
    :provider :github
    :id "gh")
  (:documentation
   "Локация для репозиториев на GitHub.
    По умолчанию имеет идентификатор \"gh\" и провайдера :github.
    Слот :user задаёт имя пользователя/организации."))

(defclass <gitlab> (<provider> <location>)
  ((user
    :initarg :user
    :initform nil
    :accessor <gitlab>-user
    :type (or null string)
    :documentation "Имя пользователя/организации на GitLab."))
  (:default-initargs
    :provider :gitlab
    :id "gl")
  (:documentation
   "Локация для репозиториев на GitLab.
    По умолчанию имеет идентификатор \"gl\" и провайдера :gitlab.
    Слот :user задаёт имя пользователя/организации."))
