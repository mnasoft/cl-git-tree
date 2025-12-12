(in-package :cl-git-tree/loc)

(defparameter *locations* (make-hash-table :test 'equal)
  "Глобальная таблица всех зарегистрированных локаций.

Ключ: строка (имя/идентификатор локации).
Значение: объект класса <location>, содержащий параметры шаблона
(имя, базовый git‑URL, пути к архивам, флаг локального использования).

Используется всеми командами (clone, unclone, remote-*) для поиска и
построения URL репозиториев.")

(defun expand-home (path)
  "Заменяет ведущий '~' на домашний каталог (Linux/MSYS2)."
  (cond
    ((pathnamep path)
     (uiop:native-namestring (uiop:ensure-pathname path)))
    ((and (stringp path) (> (length path) 0)
          (char= (char path 0) #\~))
     (handler-case
         (uiop:native-namestring (uiop:ensure-pathname path :want-relative nil))
       (error () path)))
    (t path)))

(defun register-location (loc)
  "Зарегистрировать объект LOC в глобальной таблице локаций.
Если локация с таким id уже существует, она будет перезаписана."
  (when (gethash (<location>-id loc) *locations*)
    (warn "Перезаписываю существующую локацию с ключом ~A" (<location>-id loc)))
  (setf (gethash (<location>-id loc) *locations*) loc)
  loc)

(defun add-location (id &key description name url-git url-xz tar provider)
  "Добавляет или обновляет запись о локации в глобальной хэш-таблице *locations*.

ID — строковый идентификатор локации (ключ в хэш-таблице). Убедимся, что это строка.
:description — человекочитаемое имя/описание локации.
:url-git  — базовый git-URL для группы репозиториев (обычно оканчивается на '/' или ':' для SSH).
:url-xz   — путь к архиву .tar.xz, если используется (по умолчанию NIL).
:tar      — путь к tar-архиву, если используется (по умолчанию NIL).
:provider — (опционально) явный символ провайдера, например :local, :github, :gitlab.

Функция выполняет простую валидацию/нормализацию:
- гарантирует, что ID — строка;
- нормализует `url-git`: добавляет завершающий '/' для HTTP/файловых путей и ':' для SSH-форматов;
- если :provider не указан, пытается вывести его по `url-git` (локальный путь, github.com, gitlab.com).

Если в *locations* уже существует запись с данным ID, она будет
перезаписана, и будет выдано предупреждение через WARN.

Возвращает созданный объект класса <location>."
  ;; ID как строка
    (let* ((id-str (if (stringp id) id (prin1-to-string id)))
      (desc (or description name))
      (url url-git)
      (url-xz* url-xz)
      (tar* tar)
      (prov provider))
    ;; Нормализация url-git
    (when url
      ;; локальные пути: завершаем '/' если нужно
      (when (infer-local-p url)
        (unless (uiop:string-suffix-p "/" url)
          (setf url (concatenate 'string url "/"))))
      ;; HTTP(S) — завершаем '/'
      (when (uiop:string-prefix-p "http" url)
        (unless (uiop:string-suffix-p "/" url)
          (setf url (concatenate 'string url "/"))))
      ;; SSH scp-подобный синтаксис (есть '@') — приводим конец к одиночному '/'
      (when (search "@" url)
        ;; заменить любую последовательность ':' или '/' в конце на одиночный '/'
        (setf url (cl-ppcre:regex-replace-all "[:/]+$" url "/")))
      ;; Убедимся, что в конце нет нескольких '/' — заменим на одну
      (setf url (cl-ppcre:regex-replace-all "/+$" url "/")))

    ;; Если провайдер не указан — сделаем простую эвристику по URL
    (unless prov
      (cond
        ((and url (infer-local-p url)) (setf prov :local))
        ((and url (search "github.com" url)) (setf prov :github))
        ((and url (search "gitlab.com" url)) (setf prov :gitlab))
        (t (setf prov nil))))

    (when (gethash id-str *locations*)
      (warn "Перезаписываю существующую локацию с ключом ~A" id-str))
    ;; choose class according to provider
    (let ((class
           (cond
             ((eq prov :local) '<local>)
             ((eq prov :github) '<github>)
             ((eq prov :gitlab) '<gitlab>)
             (t '<location))))
      (let ((loc (make-instance class
                                :id id-str
                                :description desc
                                :url-git url
                                :url-xz url-xz*
                                :tar tar*
                                :provider prov)))
        (register-location loc)
        (find-location id-str)))))

(defun find-location (id)
  "Возвращает объект <location> по ключу или NIL.

KEY — строковый идентификатор локации (ключ в глобальной хэш-таблице *locations*).
Например: \"pp\", \"gh\", \"mirror\".

Если в *locations* нет записи с таким ключом, возвращает NIL."
  (gethash id *locations*))

(defun location-exists-p (id)
  "Проверяет, существует ли локация с данным ключом в глобальной таблице *locations*.

ID — строковый идентификатор локации (ключ в хэш-таблице *locations*),
например: \"pp\", \"gh\", \"mirror\".

Возвращает T, если в *locations* есть запись с таким ключом,
и NIL — если локация не найдена."
  (not (null (find-location id))))

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
                     (<location>-id loc)
                     (<location>-url-git loc)
                     (<location>-tar loc)
                     (<location>-url-xz loc)))
           *locations*))

(defun remove-location (id)
  "Удаляет локацию с ключом ID из глобальной таблицы *locations*.
Возвращает T если удаление произошло, NIL если не найдено." 
  (when (gethash id *locations*)
    (remhash id *locations*)
    t))

(defun save-locations-config (&optional path)
  "Сохраняет текущие локации в файл конфигурации PATH.
Если PATH не указан, используется ~/.git-tree/locations.lisp.
Записывает серию форм (add-location ...) для всех локаций, кроме ключей 'pp' и 'pz'.
Создаёт резервную копию старого файла с расширением .bak.
Возвращает PATH." 
  (let* ((default-path (merge-pathnames #p".git-tree/locations.lisp" (user-homedir-pathname)))
         (path (or path default-path)))
    (when (probe-file path)
      (let* ((dir (pathname-directory path))
             (name (pathname-name path))
             (bak-name (concatenate 'string name ".bak"))
             (bak (merge-pathnames (make-pathname :name bak-name :type nil :directory dir) (make-pathname :directory dir))))
        (when (probe-file path)
          (rename-file path bak))))
    (ensure-directories-exist (make-pathname :directory (pathname-directory path)))
    (with-open-file (s path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format s "(in-package :cl-git-tree/loc)~%~%")
      ;; iterate keys sorted for stable output
      (dolist (k (sort (all-location-keys) #'string< :key #'identity))
        (let ((loc (find-location k)))
                (format s "(add-location ~S~%~VT:url-git ~S~%~VT:url-xz ~S~%~VT:tar ~S~%~VT:description ~S~%~VT:provider ~S)~%~%"
                  k
                  4 (<location>-url-git loc)
                  4 (<location>-url-xz loc)
                  4 (<location>-tar loc)
                  4 (<location>-description loc)
                  4 (<location>-provider loc))))
      path)))

(defun infer-local-p (url)
  "Определяет, можно ли считать URL локальным.

URL считается локальным, если:
  - начинается с '/' (абсолютный путь в файловой системе);
  - начинается с '//' (UNC-путь или адрес во внутренней сети).

Возвращает T, если URL локальный, иначе NIL."
  (or (uiop:string-prefix-p "/" url)
      (uiop:string-prefix-p "//" url)))

(defun location-local-p (loc)
  "Вернуть T, если локация LOC имеет провайдера :local."
  (and (typep loc '<location>)
       (eq (<location>-provider loc) :local)))

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

