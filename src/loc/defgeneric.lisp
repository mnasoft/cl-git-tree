(in-package :cl-git-tree/loc)

;;; ----------------------------------------------------------------------
;;; Обобщённые функции для провайдеров
;;; ----------------------------------------------------------------------

(defgeneric clone (provider target-path)
  (:documentation
   "Получить копию репозитория из источника PROVIDER в каталог TARGET-PATH.
    Для <local> это копирование каталога, для удалённых провайдеров не применяется."))

(defgeneric repo-create (workspace provider &key &allow-other-keys)
  (:documentation
   "Создать новый репозиторий на удалённом провайдере.
    Дополнительные ключи можно передавать свободно благодаря &allow-other-keys."))


(defgeneric repo-push (workspace provider &key &allow-other-keys)
  (:documentation
   "Выполнить git push для указанного WORKSPACE.
    Ключи:
      :remote        → имя удалённого (по умолчанию \"origin\")
      :branch        → имя ветки (по умолчанию текущая)
      :force         → T → использовать --force
      :tags          → T → отправить все теги
      :set-upstream  → T → добавить --set-upstream"))

(defgeneric repo-pull (workspace provider &key &allow-other-keys)
  (:documentation
   "Выполнить git pull для указанного WORKSPACE.
    PROVIDER используется для определения имени remote.
    Ключи:
      :remote  → имя удалённого (по умолчанию \"origin\")
      :branch  → имя ветки (по умолчанию текущая)
      :rebase  → T → использовать --rebase
      :ff-only → T → использовать --ff-only"))

(defgeneric repo-delete (workspace provider &key &allow-other-keys)
  (:documentation
   "Удалить репозиторий на удалённом провайдере.
    PROVIDER — объект провайдера (например, <github>).
    WORKSPACE — локальный workspace, связанный с репозиторием.
    Дополнительные ключи:
      :yes         → не задавать вопросов (по умолчанию T)
      :remote-only → удалить только удалённый репозиторий, не трогая локальный."))

(defgeneric repo-status (workspace provider &key &allow-other-keys)
  (:documentation
   "Вернуть статус git‑репозитория в рабочем пространстве."))

(defgeneric repo-commit (workspace &key &allow-other-keys)
  (:documentation
   "Выполнить коммит в рабочем пространстве с сообщением MESSAGE."))

(defgeneric repo-branches (workspace)
  (:documentation
   "Вернуть список веток git‑репозитория в рабочем пространстве."))

(defgeneric git-init (workspace &key &allow-other-keys)
  (:documentation
   "Инициализировать git-репозиторий в рабочем пространстве.
    Ключи:
      :bare             → создать bare-репозиторий
      :initial-branch   → имя начальной ветки
      :separate-git-dir → путь к внешнему .git
      :quiet            → подавить вывод
      :template         → шаблонный каталог
      :shared           → режим совместного доступа"))

(defgeneric git-initialized-p (workspace)
  (:documentation
   "Проверить, инициализирован ли git для указанного объекта (например, <workspace>)."))

(defgeneric git-root (workspace)
  (:documentation
   "Найти корень git-репозитория для указанного объекта."))

(defgeneric repo-name (workspace)
  (:documentation
   "Вернуть базовое имя git-репозитория для указанного объекта.
    Если git не инициализирован, возвращает NIL."))

(defgeneric repo-add (workspace &key &allow-other-keys)
  (:documentation
   "Добавить файлы в git индекс для указанного WORKSPACE и PROVIDER (значение игнорироуется).
    Ключи:
      :files        → список файлов/паттернов для добавления (строка или список строк)
      :all          → T → добавить все изменения (git add --all)
      :update       → T → обновить только отслеживаемые файлы (git add --update)
      :force        → T → принудительно добавить (git add --force)
      :dry-run      → T → только показать, что будет добавлено (git add --dry-run)"))
