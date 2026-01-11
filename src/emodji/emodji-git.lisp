;;;; ./src/emodji/emodji-git.lisp

(in-package :cl-git-tree/emodji)

;; Repo
(define-emodji "git init"           :git-repo "Новый репозиторий" "◻" "🧱")
(define-emodji "git clone"          :git-repo "Клонирование" "⬇" "📥")
(define-emodji "git clone --bare"   :git-repo "Голый репозиторий" "◻◻" "📦")
(define-emodji "git clone --mirror" :git-repo "Зеркало" "⬍" "🪞")

;; Remote
(define-emodji "git remote add"     :git-remote "Добавить remote" "➕" "➕🔗")
(define-emodji "git remote remove"  :git-remote "Удалить remote" "✖" "❌🔌")
(define-emodji "git remote rename"  :git-remote "Переименовать" "✎" "✏️")
(define-emodji "git remote set-url" :git-remote "Изменить URL" "⚙" "🌐")
(define-emodji "git remote -v"      :git-remote "Список remotes" "☰" "📋")

;; Branch
(define-emodji "git branch"         :git-branch "Список веток" "⎇" "🌿")
(define-emodji "git branch <name>"  :git-branch "Создать ветку" "➕⎇" "➕🌿")
(define-emodji "git branch -d"      :git-branch "Удалить ветку" "✖⎇" "🗑️🌿")
(define-emodji "git switch"         :git-branch "Переключиться" "⇄" "🔀")

;; Commit
(define-emodji "git add"            :git-commit "Добавить в индекс" "➕" "➕📄")
(define-emodji "git commit"         :git-commit "Коммит" "●" "📝")
(define-emodji "git commit --amend" :git-commit "Изменить коммит" "✎●" "✏️📝")
(define-emodji "git log"            :git-commit "История" "☰" "📜")

;; History
(define-emodji "git merge"          :git-history "Слияние" "⇉" "🔀")
(define-emodji "git rebase"         :git-history "Переписать историю" "⇅" "🧬")
(define-emodji "git cherry-pick"    :git-history "Выборочный коммит" "◎" "🍒")
(define-emodji "git revert"         :git-history "Откат" "↶" "↩️")

;; Stash
(define-emodji "git stash"          :git-stash "Спрятать изменения" "⌁" "🎒")
(define-emodji "git stash pop"      :git-stash "Вернуть" "⌁↑" "🎒⬆️")
(define-emodji "git stash list"     :git-stash "Список stash" "☰⌁" "📋🎒")

;; Clean
(define-emodji "git clean"    :git-clean "Очистка" "✧" "🧹")
(define-emodji "git gc"       :git-clean "Сборка мусора" "♻" "♻️")

;; Network
(define-emodji "git fetch" :git-network "Получить" "↓" "📡⬇️")
(define-emodji "git pull"  :git-network "Получить + слить" "⇅" "📡🔀")
(define-emodji "git push"  :git-network "Отправить" "↑" "📡⬆️")

;; Diagnostics
(define-emodji "git status" :git-diag "Состояние" "○" "🧭")
(define-emodji "git diff"   :git-diag "Разница" "≠" "⚖️")
(define-emodji "git blame"  :git-diag "Кто изменил строку" "☍" "🕵️")

