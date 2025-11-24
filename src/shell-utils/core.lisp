;;;; ./src/shell-utils/core.lisp

(in-package :cl-git-tree/shell-utils)

(defun split-args-by-keys (args)
  "Разделяет список аргументов на alist вида ((:KEY (values ...)) ...).
   Ключи начинаются с \"--\". Специальный маркер \"--\" отделяет ключи от позиционных аргументов."
  (let ((result '())
        (current-key "--preamble")
        (current-values '())
        (positional '()))
    (labels ((flush ()
               (when current-key
                 (push (cons
                        (intern (string-upcase (subseq current-key 2)) :keyword)
                        (nreverse current-values))
                       result))
               (setf current-values '())))
      (dolist (arg args)
        (cond
          ;; маркер "--" → всё дальше идёт в positional
          ((string= arg "--")
           (flush)
           (setf current-key nil))
          ;; ключ "--xxx"
          ((and (>= (length arg) 2)
                (string= (subseq arg 0 2) "--"))
           (flush)
           (setf current-key arg))
          ;; позиционные аргументы
          ((null current-key)
           (push arg positional))
          ;; значения текущего ключа
          (t
           (push arg current-values))))
      (flush))
    ;; возвращаем alist + отдельный ключ :ARGS для позиционных
    (nreverse (if positional
                  (cons (cons :ARGS (nreverse positional)) result)
                  result))))

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


