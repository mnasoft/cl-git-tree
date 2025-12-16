;;;; tests/package.lisp

(defpackage :cl-git-tree/tests
  (:use #:cl #:fiveam)
  (:export run-tests force-delete-directory)
  (:documentation " :cl-git-tree/tests "))

(in-package :cl-git-tree/tests)

(defun force-delete-directory (dir)
  "Удалить DIR, предварительно снимая атрибут read-only под Windows/MSYS2.
   Используется в тестах для очистки git-репозиториев, где объекты могут быть read-only."
  (handler-case
      (uiop:delete-directory-tree dir :validate t)
    (error (e)
      ;; На Windows pack-файлы в .git/objects иногда помечаются read-only.
      ;; Снимаем атрибуты через cmd.exe и пробуем ещё раз.
      (let ((os-type (let ((os (uiop:operating-system)))
                       (cond
                         ((uiop:getenv "MSYSTEM") :msys2)
                         ((member os '(:win :win32 :windows :mswindows)) :windows)
                         (t :linux)))))
        (when (member os-type '(:windows :msys2))
          (ignore-errors
            (uiop:run-program (list "cmd.exe" "/c" "attrib" "-R" "/S" "/D"
                                    (uiop:native-namestring dir))
                              :ignore-error-status t))
          ;; После снятия атрибутов пробуем удалить ещё раз (игнорируя любые ошибки)
          (ignore-errors (uiop:delete-directory-tree dir :validate nil))
          (return-from force-delete-directory t))
        ;; На Linux/других ОС — просто пробрасываем ошибку
        (error e)))))

(defun run-tests () (run! 'all))
