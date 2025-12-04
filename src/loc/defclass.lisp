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
    :documentation "Человеко‑читаемое описание workspace, полезное для тестов и CLI help."))
  (:documentation
   "Класс <workspace> описывает локальный рабочий каталог, связанный с операциями над репозиториями.
    Экземпляр хранит путь к каталогу и дополнительное описание. 
    Workspace выступает как абстракция над файловой системой, позволяя методам (например, repo-create, 
    repo-push, repo-pull) работать не с голым pathname, а с объектом с явными слотами и аксессорами."))




