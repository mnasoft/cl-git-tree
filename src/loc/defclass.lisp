(in-package :cl-git-tree/loc)

(defclass <location> ()
  ((id
    :initarg :id
    :initform nil
    :accessor <location>-id
    :type (or null string)
    :documentation
    "@b(Идентификатор:) Строковый ключ локации; используется как ключ в
*locations* и совпадает с именами remotes.")
   (description 
    :initarg :description
    :initform ""
    :accessor <location>-description
    :type (or null string)
    :documentation
    "@b(Описание:) Человекочитаемая метка локации для CLI и справочной
информации.")
   (url-git
    :initarg :url-git
    :initform nil
    :accessor <location>-url-git
    :type (or null string pathname)
    :documentation
    "@b(URL:) Базовый git-URL или локальный @c(pathname) для группы
репозиториев.
@begin(list)
    @item(GitHub/GitLab: строки вида \"git@github.com:user/\")
    @item(Local: @c(pathname), например @c(#P\"/tmp/repos/\"))
@end(list)

 Может быть @c(NIL), если локация ещё не инициализирована.")
   (url-xz
    :initarg :url-xz
    :initform nil
    :accessor <location>-url-xz
    :type (or null pathname)
    :documentation
    "@b(Архив:) Путь к @c(.tar.xz), если используется для
распространения репозиториев в виде архивов.")
   (tar
    :initarg :tar
    :initform nil
    :accessor <location>-tar
    :type (or null string)
    :documentation
    "@b(TAR:) Имя tar-архива (строка) или @c(NIL); при необходимости может быть заменено на @c(pathname) в будущем.")
   (provider
    :initarg :provider
    :initform nil
    :accessor <location>-provider
    :type (or null symbol)
    :documentation "@b(Провайдер:) Символ, определяющий тип провайдера: @c(:local),
@c(:github) или @c(:gitlab)."))
  (:documentation
   "@b(Назначение:) Описывает общий шаблон локации: идентификатор, базовые
URL и провайдера.

 @b(Использование:) Экземпляры применяются при сборке путей к
репозиториям, поиске в *locations* и сопоставлении с git remotes."))

(defclass <provider> (<location>) ()
  (:documentation "@b(Назначение:) Абстрактный базовый класс провайдеров размещения.

 Слотов не добавляет и служит точкой диспетчеризации методов."))

(defun detect-os ()
  "@b(Назначение:) Определяет операционную систему среды выполнения.

 @b(Возвращает:) Одно из @c(:linux), @c(:windows) или @c(:msys2).

 @b(Особенности:) MSYS2 определяется по переменной окружения @c(MSYSTEM)."
  (let ((os (uiop:operating-system)))
    (cond
      ;; MSYS2 определяется по наличию переменной окружения MSYSTEM
      ((uiop:getenv "MSYSTEM") :msys2)
      ;; Windows (без MSYS2)
      ((member os '(:win :win32 :windows :mswindows)) :windows)
      ;; Linux и остальные Unix-подобные
      (t :linux))))

(defclass <workspace> ()
  ((path
    :initarg :path
    :accessor <workspace>-path
    :initform nil
    :type (or null pathname)
    :documentation "@b(Путь:) @c(pathname) рабочего каталога или @c(NIL), если
workspace не привязан.")
   (description
    :initarg :description
    :accessor <workspace>-description
    :initform ""
    :type string
    :documentation "@b(Описание:) Человекочитаемое описание workspace для тестов и CLI-help.")
   (os-type
    :initarg :os-type
    :accessor <workspace>-os-type
    :initform (detect-os)
    :type symbol
    :documentation "@b(ОС:) Тип операционной системы: @c(:linux), @c(:windows) или @c(:msys2)."))
  (:documentation
   "@b(Назначение:) Базовый workspace с путём, описанием и типом ОС.

 @b(Поля:)
@begin(list)
 @item(@c(path) - путь к каталогу workspace в виде @c(pathname);)
 @item(@c(description) - человекочитаемое описание для CLI и тестов;)
 @item(@c(os-type) - тип ОС: @c(:linux), @c(:windows) или @c(:msys2);)
@end(list)

 @b(Использование:) Базовый класс для OS-специфичных workspace; методы
 @c(repo-*) используют его для нормализации путей."))

(defclass <workspace-linux> (<workspace>)
  ()
  (:documentation "@b(Назначение:) Workspace для Linux; использует прямые слеши в путях."))

(defclass <workspace-windows> (<workspace>)
  ()
  (:documentation "@b(Назначение:) Workspace для Windows (без MSYS2); использует обратные
слеши."))

(defclass <workspace-msys2> (<workspace>)
  ()
  (:documentation "@b(Назначение:) Workspace для MSYS2; использует Unix-стиль слешей, но работает поверх Windows, поэтому требует специальной обработки путей."))

(defun make-workspace (path &key description)
  "@b(Назначение:) Создаёт workspace под текущую ОС и гарантирует наличие каталога.

 @b(Аргументы:)
@begin(list)
 @item(@c(path) - путь к каталогу workspace; при отсутствии каталог создаётся;)
 @item(@c(description) - опциональное описание; по умолчанию используется имя каталога.)
@end(list)

 @b(Логика:) Определяет ОС через @c(detect-os), выбирает
 класс (@c(<workspace-linux>), @c(<workspace-windows>),
 @c(<workspace-msys2>)), разворачивает тильду, создаёт каталог и
 возвращает экземпляр с @c(truename).

 @b(Пример:)
@begin[lang=lisp](code)
 (make-workspace \"~/proj/demo\")
@end(code)"
  (let* ((pathname (uiop:ensure-directory-pathname
                    (cl-git-tree/fs:expand-home path)))
         (os-type (detect-os))
         (class (case os-type
                  (:linux '<workspace-linux>)
                  (:windows '<workspace-windows>)
                  (:msys2 '<workspace-msys2>)
                  (t '<workspace>))))
    (handler-case
        (progn
          ;; Создаём каталог, если не существует
          (ensure-directories-exist pathname)
          ;; Получаем truename
          (let* ((truename (truename pathname)))
            (unless (uiop:directory-exists-p truename)
              (error "Путь ~A существует, но не является каталогом." truename))
            (let ((desc (or description
                            (car (last (pathname-directory truename))))))
              (make-instance class
                             :path truename
                             :description desc
                             :os-type os-type))))
      (file-error (e)
        (error "Не удалось создать или получить доступ к каталогу ~A: ~A"
               path e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(progn (make-instance '<local>  :id "lc")
       (make-instance '<github> :id "gh")
       (make-instance '<gitlab> :id "gl"))

(cl-git-tree/fs:expand-home "~")
