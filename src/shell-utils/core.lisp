(in-package :cl-git-tree/shell-utils)

(defun shell-run (cwd &rest args)
  "Запустить произвольную команду ARGS в каталоге CWD.
Возвращает три значения: stdout, stderr, код возврата."
  (let ((dir (truename cwd)))
    (unless (uiop:directory-exists-p dir)
      (error "Каталог ~A не существует или недоступен." dir))
    (uiop:run-program args
                      :directory dir
                      :output :string
                      :error-output :string
                      :ignore-error-status t)))

(defun shell-run (cwd &rest args)
  "Запустить произвольную команду ARGS в каталоге CWD.
Возвращает три значения: stdout, stderr, код возврата."
  (let ((dir (truename cwd)))
    (unless (uiop:directory-exists-p dir)
      (error "Каталог ~A не существует или недоступен." dir))
    (multiple-value-bind (out err code)
        (uiop:run-program args
                          :directory dir
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (when (zerop code)
        (let ((rez
                (remove-if
                 #'uiop:emptyp
                 (uiop:split-string out :separator '(#\Newline)))))
          (cond
            ((= 1 (length rez)) (car rez))
            (t rez)))))))

