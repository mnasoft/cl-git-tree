(in-package :cl-git-tree/loc)

;;; ----------------------------------------------------------------------
;;; Обобщённые функции для провайдеров
;;; ----------------------------------------------------------------------

(defgeneric remote-create (workspace provider &key &allow-other-keys)
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

(defgeneric remote-delete (workspace provider &key &allow-other-keys)
  (:documentation
   "Удалить репозиторий на удалённом провайдере.
    PROVIDER — объект провайдера (например, <github>).
    WORKSPACE — локальный workspace, связанный с репозиторием.
    Дополнительные ключи:
      :yes         → не задавать вопросов (по умолчанию T)
      :remote-only → удалить только удалённый репозиторий, не трогая локальный."))

(defgeneric repo-status (workspace &key &allow-other-keys)
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

(defgeneric repo-provider-keys (workspace)
  (:documentation
   "Вернуть список ключей (имен remotes), соответствующих зарегистрированным локациям
для указанного WORKSPACE. Обычно это строковые идентификаторы локаций
в глобальной таблице *locations* (например, \"lc\", \"gh\")."))

(defgeneric repo-providers (workspace)
  (:documentation
   "Определить список локаций-провайдеров git‑репозитория для WORKSPACE по его remotes.
Возвращает список объектов <location>, соответствующих найденным remotes."))


(defgeneric repo-transport-export (workspace provider &key &allow-other-keys)
  (:documentation
   "Создать tar.xz‑архив(ы) для git‑репозитория WORKSPACE на провайдере PROVIDER.
Используется командой git-tree transport export. Возвращает количество
созданных архивов (целое число)."))

(defgeneric repo-transport-import (workspace provider &key verbose &allow-other-keys)
  (:documentation
   "Импортировать изменения из tar.xz архива для WORKSPACE и PROVIDER.\n\nДолжен распаковать архив из :url-xz провайдера в рабочий каталог репозитория.\nВозвращает T при успешном импорте, NIL иначе."))

(defgeneric repo-add (workspace &key &allow-other-keys)
  (:documentation
   "Добавить файлы в git индекс для указанного WORKSPACE и PROVIDER (значение игнорироуется).
    Ключи:
      :files        → список файлов/паттернов для добавления (строка или список строк)
      :all          → T → добавить все изменения (git add --all)
      :update       → T → обновить только отслеживаемые файлы (git add --update)
      :force        → T → принудительно добавить (git add --force)
      :dry-run      → T → только показать, что будет добавлено (git add --dry-run)"))

(defgeneric remote-add (workspace provider &key &allow-other-keys)
    (:documentation
   "Добавить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."))

(defgeneric remote-remove (workspace provider &key &allow-other-keys)
  (:documentation
   "Удалить отдаленный репозиторий для рабочего пространства WORKSPACE,
связанный с провайдером PROVIDER."))

(defgeneric remote-readd (workspace provider &key &allow-other-keys)
  (:documentation
   "Удалить и снова добавить отдаленный репозиторий для рабочего
пространства WORKSPACE, связанный с провайдером PROVIDER."))

(defgeneric remote-url (workspace provider &key &allow-other-keys)
  (:documentation
   "Построить URL удалённого репозитория для WORKSPACE на провайдере PROVIDER.
    Возвращает строку с полным git-URL репозитория."))

;;; ----------------------------------------------------------------------
;;; Дополнительные операции над git-репозиторием рабочего пространства
;;; ----------------------------------------------------------------------

(defgeneric repo-is-clean-p (workspace)
  (:documentation
   "Проверить, что git‑репозиторий в WORKSPACE чист (нет незакоммиченных изменений).
Возвращает T, если git status --short пустой и команда завершилась успешно."))

(defgeneric repo-last-commit-date (workspace)
  (:documentation
   "Вернуть дату последнего коммита в WORKSPACE в формате Unix‑timestamp (целое число)
или NIL, если дату получить не удалось."))

(defgeneric days-since-last-commit (workspace)
  (:documentation
   "Вернуть количество дней, прошедших с момента последнего коммита в WORKSPACE,
или NIL, если дату последнего коммита получить не удалось."))


(defgeneric expand-path (workspace path)
  (:documentation
   "Раскрыть '~' в пути PATH в контексте WORKSPACE.
    Заменяет ведущий '~' на домашний каталог. Нормализует ./ и ../ компоненты пути."))

(defgeneric remote-import-cleanup-dir (ws provider &key &allow-other-keys) 
  (:documentation
   "Удаляет каталог временного remote после отключения.
Аргументы: ws, provider."))

(defgeneric remote-import-delete-archive (ws provider &key &allow-other-keys) 
  (:documentation
   "Удаляет tar.xz архив для WORKSPACE/PROVIDER. Аргументы: ws, provider."))


(defgeneric find-emo (workspace key)
  (:documentation
   "Возвращает emodji по ключу key в рабочем пространстве workspace."))
