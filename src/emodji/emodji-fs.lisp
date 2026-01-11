;;;; ./src/emodji/emodji-fs.lisp

(in-package :cl-git-tree/emodji)

;; File operations
(define-emodji "fs file create"   :fs-file "Создать файл" "+" "📄✨")
(define-emodji "fs file open"     :fs-file "Открыть файл" "○" "📂")
(define-emodji "fs file edit"     :fs-file "Редактировать" "✎" "✏️")
(define-emodji "fs file save"     :fs-file "Сохранить" "⇓" "💾")
(define-emodji "fs file copy"     :fs-file "Копировать" "⎘" "📋")
(define-emodji "fs file move"     :fs-file "Переместить" "⇄" "🔀")
(define-emodji "fs file rename"   :fs-file "Переименовать" "✎" "✏️📛")
(define-emodji "fs file delete"   :fs-file "Удалить файл" "✖" "🗑️")

;; Directory operations
(define-emodji "fs dir create"    :fs-dir "Создать каталог" "+" "📁✨")
(define-emodji "fs dir open"      :fs-dir "Открыть каталог" "○" "📂")
(define-emodji "fs dir list"      :fs-dir "Список файлов" "☰" "📋")
(define-emodji "fs dir delete"    :fs-dir "Удалить каталог" "✖" "🗑️📁")
(define-emodji "fs dir empty"     :fs-dir "Пустой каталог" "□" "📁")

;; General filesystem
(define-emodji "fs delete"        :fs-general "Удаление" "✖" "🗑️")
(define-emodji "fs trash"         :fs-general "В корзину" "⌧" "🗑️")
(define-emodji "fs clean"         :fs-general "Очистка" "✧" "🧹")
(define-emodji "fs archive"       :fs-general "Архив" "▣" "📦")
(define-emodji "fs compress"      :fs-general "Сжатие" "⊡" "🗜️")
(define-emodji "fs extract"       :fs-general "Распаковка" "⊞" "📂")
(define-emodji "fs search"        :fs-general "Поиск" "⌕" "🔍")
(define-emodji "fs folder"        :fs-general "Папка" "▢" "📁")
(define-emodji "fs file"          :fs-general "Файл" "○" "📄")

;; Permissions
(define-emodji "fs lock"          :fs-perm "Заблокировать" "⊠" "🔒")
(define-emodji "fs unlock"        :fs-perm "Разблокировать" "⊡" "🔓")
(define-emodji "fs chmod"         :fs-perm "Изменить права" "⚙" "🔐")
(define-emodji "fs readonly"      :fs-perm "Только чтение" "⊟" "👁️")
