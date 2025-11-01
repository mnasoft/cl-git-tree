(in-package :cl-git-tree/loc)

(defclass <location> ()
  ((name
    :initarg :name
    :accessor <location>-name
    :documentation "Человекочитаемое имя/идентификатор шаблона.")
   (url-git
    :initarg :url-git
    :accessor <location>-url-git
    :documentation "Базовый git-URL для группы репозиториев. Обычно оканчивается на '/'.")
   (url-xz
    :initarg :url-xz
    :initform nil
    :accessor <location>-url-xz
    :documentation "Путь к архиву .tar.xz, если используется для распространения.")
   (tar
    :initarg :tar
    :initform nil
    :accessor <location>-tar
    :documentation "Путь к tar-архиву, если используется.")
   (local
    :initarg :local
    :initform nil
    :accessor <location>-local
    :documentation
    "Флаг, указывающий, можно ли использовать эту локацию для локального клонирования.
NIL — определяется автоматически по url-git (локальный путь или UNC).
T — явно разрешено локальное клонирование, даже если URL выглядит сетевым.
:FORCE-NO — явно запрещено локальное клонирование."))
  (:documentation
   "Класс <location> описывает шаблон для группы репозиториев.
Содержит имя, базовые URL для git/архивов и флаг локального использования.
Используется как элемент глобальной таблицы *locations* для построения
полных путей к конкретным репозиториям и для фильтрации по типу локации."))

(defparameter *locations* (make-hash-table :test 'equal)
  "Глобальная таблица всех зарегистрированных локаций.

Ключ: строка (имя/идентификатор локации).
Значение: объект класса <location>, содержащий параметры шаблона
(имя, базовый git‑URL, пути к архивам, флаг локального использования).

Используется всеми командами (clone, unclone, remote-*) для поиска и
построения URL репозиториев.")

(defun add-location (key &key name url-git url-xz tar)
  "Добавляет или обновляет запись о локации в глобальной таблице *locations*.

KEY — строковый идентификатор локации (ключ в хэш-таблице).
:name     — человекочитаемое имя/описание локации.
:url-git  — базовый git-URL для группы репозиториев (обычно оканчивается на '/').
:url-xz   — путь к архиву .tar.xz, если используется (по умолчанию NIL).
:tar      — путь к tar-архиву, если используется (по умолчанию NIL).

Если в *locations* уже существует запись с данным KEY, она будет
перезаписана, и будет выдано предупреждение через WARN.

Возвращает созданный объект класса <location>."
  (when (gethash key *locations*)
    (warn "Перезаписываю существующую локацию с ключом ~A" key))
  (setf (gethash key *locations*)
        (make-instance '<location>
                       :name name
                       :url-git url-git
                       :url-xz url-xz
                       :tar tar)))

(defun find-location (key)
  "Возвращает объект <location> по ключу или NIL.

KEY — строковый идентификатор локации (ключ в глобальной хэш-таблице *locations*).
Например: \"pp\", \"gh\", \"mirror\".

Если в *locations* нет записи с таким ключом, возвращает NIL."
  (gethash key *locations*))


(defun location-exists-p (key)
  "Проверяет, существует ли локация с данным ключом в глобальной таблице *locations*.

KEY — строковый идентификатор локации (ключ в хэш-таблице *locations*),
например: \"pp\", \"gh\", \"mirror\".

Возвращает T, если в *locations* есть запись с таким ключом,
и NIL — если локация не найдена."
  (not (null (find-location key))))

(defun all-location-keys ()
  "Возвращает список всех ключей (строковых идентификаторов) локаций
из глобальной хэш-таблицы *locations*.

Каждый элемент списка — это ключ (строка), под которым хранится объект
класса <location>. Эти ключи можно использовать для вызова функций
FIND-LOCATION, LOCATION-EXISTS-P или для построения URL через REPO-URL.

Пример:
  (all-location-keys) ;; => (\"pp\" \"gh\" \"mirror\")"
  (loop for k being the hash-keys of *locations* collect k))

(defun all-locations ()
  "Возвращает список всех объектов класса <location> из глобальной таблицы *locations*.

Каждый элемент списка — это экземпляр <location>, содержащий имя,
базовый git‑URL, пути к архивам и флаг локального использования.

Используется для обхода всех зарегистрированных локаций, например,
в командах clone/unclone/remote-*.

Пример:
  (all-locations)
  ;; => (#<LOCATION {1004A3C1B3}> #<LOCATION {1004A3D2C7}> …)"
  (loop for v being the hash-values of *locations* collect v))

(defun repo-url (key repo-name)
  "Строит полный git-URL для конкретного репозитория внутри локации.

KEY — строковый идентификатор локации (ключ в глобальной таблице *locations*).
REPO-NAME — имя репозитория (строка без расширения \".git\").

Функция ищет объект <location> по ключу KEY и, если он найден,
возвращает строку с полным git-URL для указанного репозитория.
Если локация не найдена, возвращает NIL.

Пример:
  (add-location \"gh\" :name \"GitHub\" :url-git \"https://github.com/\")
  (repo-url \"gh\" \"cl-git-tree\")
  ;; => \"https://github.com/cl-git-tree.git\""
  (let ((loc (find-location key)))
    (when loc
      (concatenate 'string (<location>-url-git loc) repo-name ".git"))))

(defun print-locations ()
  "Выводит список всех зарегистрированных локаций из глобальной таблицы *locations* 
в читаемом виде.

Для каждой локации печатается:
  - ключ (строковый идентификатор в *locations*);
  - человекочитаемое имя (<location>-name);
  - базовый git-URL (<location>-url-git);
  - путь к tar-архиву (<location>-tar), если задан;
  - путь к .xz-архиву (<location>-url-xz), если задан.

Пример вывода:
  === Список локаций ===
  gh: GitHub
     Git: https://github.com/
     TAR: NIL
     XZ : NIL"
  (format t "~%=== Список локаций ===~%")
  (maphash (lambda (key loc)
             (format t "~A: ~A~%   Git: ~A~%   TAR: ~A~%   XZ : ~A~%~%"
                     key
                     (<location>-name loc)
                     (<location>-url-git loc)
                     (<location>-tar loc)
                     (<location>-url-xz loc)))
           *locations*))

(defun infer-local-p (url)
  "Определяет, можно ли считать URL локальным.

URL считается локальным, если:
  - начинается с '/' (абсолютный путь в файловой системе);
  - начинается с '//' (UNC-путь или адрес во внутренней сети).

Возвращает T, если URL локальный, иначе NIL."
  (or (uiop:string-prefix-p "/" url)
      (uiop:string-prefix-p "//" url)))

(defun location-local-p (loc)
  "Определяет, можно ли использовать объект <location> LOC для локального клонирования.

LOC — экземпляр класса <location>.

Правила:
  - Если в слоте <location>-local явно указано значение:
      * T         — локальное клонирование разрешено;
      * :FORCE-NO — локальное клонирование запрещено;
      * NIL       — решение принимается автоматически.
  - Если слот <location>-local равен NIL, то проверяется базовый git-URL:
      * если он начинается с '/' (абсолютный путь) или с '//' (UNC/интрасеть),
        то URL считается локальным.

Возвращает T, если локальное клонирование разрешено, иначе NIL."
  (let ((flag (<location>-local loc)))
    (cond
      ((eq flag :force-no) nil)
      (flag t)
      (t (infer-local-p (<location>-url-git loc))))))

(defun match-location-keys (pattern)
  "Возвращает список ключей (строковых идентификаторов) локаций,
совпадающих с указанным glob-шаблоном.

PATTERN — строка с шаблоном, в котором:
  *  соответствует любой последовательности символов (включая пустую),
  ?  соответствует ровно одному символу.

Функция преобразует glob-шаблон в регулярное выражение и проверяет
все ключи из (ALL-LOCATION-KEYS). Возвращает список строк-ключей,
совпавших с шаблоном. Если совпадений нет — возвращает NIL.

Пример:
  (all-location-keys) ;; => (\"gh\" \"pp\" \"mirror\")
  (match-location-keys \"g*\") ;; => (\"gh\")
  (match-location-keys \"??\") ;; => (\"pp\")"
  (let* ((regex (cl-ppcre:quote-meta-chars pattern)))
    ;; заменяем glob-символы на эквиваленты в regex
    (setf regex (cl-ppcre:regex-replace-all "\\\\*" regex ".*"))
    (setf regex (cl-ppcre:regex-replace-all "\\\\?" regex "."))
    (remove-if-not
     (lambda (key) (cl-ppcre:scan regex key))
     (all-location-keys))))

