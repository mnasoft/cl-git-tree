(in-package :cl-git-tree/loc)

(defclass <location> ()
  ((id
    :initarg :id
    :initform nil
    :accessor <location>-id
    :type (or null string)
    :documentation
    "Строковый идентификатор локации. Соответствует ключу в глобальной
таблице *locations* и используется для поиска и сопоставления с именами remotes.")
   (description 
    :initarg :description
    :initform ""
    :accessor <location>-description
    :type (or null string)
    :documentation
    "Человекочитаемое описание или метка локации. Может использоваться
для вывода пользователю и в справочной информации.")
   (url-git
    :initarg :url-git
    :initform nil
    :accessor <location>-url-git
    :type (or null string pathname)
    :documentation
    "Базовый git-URL или локальный путь (pathname) для группы репозиториев.
     - Для <github>/<gitlab> это строка вида \"git@github.com:user/repo.git\".
     - Для <local> это pathname, например #P\"/tmp/repos/\".
     Может быть NIL, если локация ещё не инициализирована.")
   (url-xz
    :initarg :url-xz
    :initform nil
    :accessor <location>-url-xz
    :type (or null pathname)
    :documentation
    "Путь к архиву .tar.xz, если используется для распространения
     репозиториев в виде архивов.")
   (tar
    :initarg :tar
    :initform nil
    :accessor <location>-tar
    :type (or null string)
    :documentation
    "Имя файла tar-архива (строка) или NIL, если не используется.
     В дальнейшем можно заменить на pathname или специализированный тип,
     когда появится ясность по формату и применению.")
   (provider
    :initarg :provider
    :initform nil
    :accessor <location>-provider
    :type (or null symbol)
    :documentation "Тип провайдера (например, :local, :github, :gitlab)."))
  (:documentation
   "Класс <location> описывает шаблон для группы репозиториев.

Основные элементы:
- `id` — строковый ключ, под которым локация хранится в глобальной таблице *locations*.
- `description` — человекочитаемое описание/метка.
- `url-git` и дополнительные поля (`url-xz`, `tar`) — базовые пути для доступа к репозиториям.
- `provider` — символ, определяющий тип провайдера (:local, :github, :gitlab).

Экземпляры этого класса применяются для построения полных путей к конкретным
репозиториям, фильтрации по типу локации и проверки соответствия с git remotes."))

(defclass <provider> (<location>) ()
  (:documentation "Абстрактный базовый класс для всех провайдеров размещения.
Не содержит слотов, служит для диспетчеризации методов."))

(defun detect-os ()
  "Определяет операционную систему. Возвращает :linux, :windows или :msys2."
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
    :documentation "Путь к рабочему каталогу (pathname) или NIL, если workspace пока не привязан.")
   (description
    :initarg :description
    :accessor <workspace>-description
    :initform ""
    :type string
    :documentation "Человеко‑читаемое описание workspace, полезное для тестов и CLI help.")
   (os-type
    :initarg :os-type
    :accessor <workspace>-os-type
    :initform (detect-os)
    :type symbol
    :documentation "Тип операционной системы: :linux, :windows или :msys2."))
  (:documentation
   "Базовый класс <workspace> описывает локальный рабочий каталог.
    Содержит путь к каталогу, описание и тип ОС для правильной обработки путей.
    От него наследуются специализированные классы для разных ОС."))

(defclass <workspace-linux> (<workspace>)
  ()
  (:documentation "Workspace для Linux. Использует прямые слеши в путях."))

(defclass <workspace-windows> (<workspace>)
  ()
  (:documentation "Workspace для Windows (без MSYS2). Использует обратные слеши."))

(defclass <workspace-msys2> (<workspace>)
  ()
  (:documentation "Workspace для MSYS2. Требует специальной обработки путей (прямые слеши в Unix-стиле, но работает в Windows)."))

(defun make-workspace (path &key description)
  "Создаёт экземпляр workspace для текущей ОС.
   Автоматически выбирает подходящий класс в зависимости от detect-os.
   
   PATH — путь к каталогу workspace (будет создан, если не существует).
   DESCRIPTION — человекочитаемое описание (по умолчанию — имя каталога)."
  (let* ((pathname (uiop:ensure-directory-pathname path))
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
