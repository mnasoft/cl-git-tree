(in-package :cl-git-tree/loc)

(defmethod repo-push-all ((ws <workspace>) &key &allow-other-keys)
  "Выполнить git push для всех веток на все зарегистрированные провайдеры.
Возвращает WS."
  (let* ((root (git-root ws))
         (providers (repo-providers ws)))
    (dolist (provider providers)
      (let ((remote-name (<location>-id provider)))
        (handler-case
            (progn
              ;; 1. Получаем список всех локальных веток
              (multiple-value-bind (stdout stderr code)
                  (cl-git-tree/git-utils:git-run root "branch")
                (when (zerop code)
                  (let ((local-branches
                          (remove-if #'uiop:emptyp
                                     (uiop:split-string stdout :separator '(#\Newline)))))
                    ;; 2. Для каждой локальной ветки выполняем push
                    (dolist (local-branch local-branches)
                      (let* ((trimmed (string-trim '(#\Space #\*) local-branch))
                             (branch-name (string-trim '(#\Space) 
                                                       (if (string= (subseq trimmed 0 (min 1 (length trimmed))) "*")
                                                           (subseq trimmed 1)
                                                           trimmed))))
                        (when (not (uiop:emptyp branch-name))
                          (multiple-value-bind (push-out push-err push-code)
                              (cl-git-tree/git-utils:git-run root "push" remote-name branch-name)
                            (if (zerop push-code)
                                (format t "~A [~A] Push ~A/~A успешно~%"
                                        (find-emo ws "success")
                                        remote-name (repo-name ws) branch-name)
                                (format t "~A [~A] Ошибка push ~A/~A: ~A~%"
                                        (find-emo ws "error")
                                        remote-name (repo-name ws) branch-name
                                        (or push-err push-out "неизвестная ошибка"))))))))))
          (error (e)
            (format t "~A [~A] Ошибка: ~A~%"
                    (find-emo ws "error")
                    remote-name
                    e))))
    ws))
