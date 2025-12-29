;; Инструкция для сборки бинарника:
;;
;;   sh ./build-bin.sh
;;
;; (Если нет прав на запуск: chmod +x build-bin.sh)
;;
;; После сборки для установки используйте: sudo sh ./install-git-tree-bin.sh 

;;; git-tree-bin.lisp — точка входа для standalone бинарника cl-git-tree

;; Загрузка quicklisp
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Тихая загрузка системы
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-git-tree :silent t))

;; Вызов основной функции с аргументами командной строки
(cl-git-tree/cli:main sb-ext:*posix-argv*)
