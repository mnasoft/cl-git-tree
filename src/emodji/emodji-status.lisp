;;;; ./src/emodji/emodji-status.lisp

(in-package :cl-git-tree/emodji)

(define-emodji "success"     :status "Успешно"          "✔"  "✅")
(define-emodji "warning"     :status "Предупреждение"   "⚠"  "⚠️")
(define-emodji "error"       :status "Ошибка"           "✖"  "❌")
(define-emodji "info"        :status "Информация"       "ℹ"  "ℹ️")
(define-emodji "pending"     :status "Ожидание"         "…"  "⏳")
(define-emodji "in-progress" :status "Выполняется"      "➤"  "🔄")
(define-emodji "skipped"     :status "Пропущено"        "↷"  "⏭️")
(define-emodji "blocked"     :status "Заблокировано"    "⛔" "⛔")
