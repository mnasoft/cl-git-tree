(in-package :cl-git-tree/shell-utils)

(defun split-args-by-keys (args)
  "Разделяет сптск аргументов на alist вида ((:key (values ...)) ...).
  Ключи должны начинаться с \"--\"."
  (let ((result '())
        (current-key "--preamble")
        (current-values '()))
    (labels ((flush ()
               (when current-key
                 (push (cons
                        (intern
                         (string-upcase (subseq current-key 2)) :keyword)
                             (nreverse current-values))
                       result))
               (setf current-values '())))
      (dolist (arg args)
        (if (and (>= (length arg) 2)
                 (string= (subseq arg 0 2) "--"))
            (progn
              (flush)
              (setf current-key arg))
            (push arg current-values)))
      (flush))
    (nreverse result)))

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
        (let ((rez (remove-if #'uiop:emptyp
                              (uiop:split-string out :separator '(#\Newline)))))
          (cond
            ((= 1 (length rez)) (car rez))
            (t rez)))))))


