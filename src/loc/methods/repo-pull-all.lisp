(in-package :cl-git-tree/loc)

(defmethod repo-pull-all ((ws <workspace>) &key &allow-other-keys)
  "Выполнить git fetch и создать локальные ветки для всех отслеживаемых веток.
Работает для всех зарегистрированных провайдеров рабочего пространства.
Возвращает WS."
  (let* ((root (git-root ws))
         (original-branch (and root (cl-git-tree/git-utils:current-branch root)))
         (providers (repo-providers ws)))
    (dolist (provider providers)
      (let ((remote-name (<location>-id provider)))
        (handler-case
            (progn
              ;; 1. Выполняем fetch всех веток
              (multiple-value-bind (stdout stderr code)
                  (cl-git-tree/git-utils:git-run root "fetch" remote-name)
                (if (zerop code)
                    (format t "~A [~A] Fetch ~A успешно~%"
                            (find-emo ws "success")
                            remote-name (repo-name ws))
                    (format t "~A [~A] Ошибка fetch ~A: ~A~%"
                            (find-emo ws "error")
                            remote-name (repo-name ws)
                            (or stderr stdout "неизвестная ошибка"))))
              
              ;; 2. Получаем список всех удалённых веток
              (multiple-value-bind (stdout stderr code)
                  (cl-git-tree/git-utils:git-run root "branch" "-r")
                (when (zerop code)
                  (let ((remote-branches
                          (remove-if #'uiop:emptyp
                                     (uiop:split-string stdout :separator '(#\Newline)))))
                    ;; 3. Для каждой удалённой ветки создаём локальную ветку отслеживания
                    (dolist (remote-branch remote-branches)
                      (let* ((trimmed (string-trim '(#\Space) remote-branch))
                             ;; Разбиваем "remote/branch" по первому слешу
                             (slash-pos (position #\/ trimmed))
                             (parts (when slash-pos
                                      (list (subseq trimmed 0 slash-pos)
                                            (subseq trimmed (1+ slash-pos))))))
                        (when (and parts
                                   (string= (car parts) remote-name)
                                   (not (string= (cadr parts) "HEAD"))
                                   ;; пропускаем symbolic refs вида "HEAD -> branch"
                                   (not (search "->" (cadr parts))))
                          (let ((branch-name (cadr parts)))
                            ;; Проверяем, существует ли уже локальная ветка
                            (multiple-value-bind (out1 err1 code1)
                                (cl-git-tree/git-utils:git-run root "show-ref" "--verify" 
                                                               (format nil "refs/heads/~A" branch-name))
                              (declare (ignore err1))
                              (if (zerop code1)
                                  ;; Локальная ветка уже существует, делаем merge
                                  (multiple-value-bind (merge-out merge-err merge-code)
                                      (cl-git-tree/git-utils:git-run root "merge" 
                                                                     (format nil "~A/~A" remote-name branch-name))
                                    (if (zerop merge-code)
                                        (format t "~A [~A] Merge ~A успешно~%"
                                                (find-emo ws "success")
                                                remote-name branch-name)
                                        (format t "~A [~A] Ошибка merge ~A: ~A~%"
                                                (find-emo ws "error")
                                                remote-name branch-name
                                                (or merge-err merge-out "неизвестная ошибка"))))
                                  ;; Локальной ветки нет, создаём её и настраиваем отслеживание
                                  (multiple-value-bind (checkout-out checkout-err checkout-code)
                                      (cl-git-tree/git-utils:git-run root "checkout" "--track" 
                                                                     (format nil "~A/~A" remote-name branch-name))
                                    (if (zerop checkout-code)
                                        (format t "~A [~A] Создана ветка ~A~%"
                                                (find-emo ws "success")
                                                remote-name branch-name)
                                        (format t "~A [~A] Ошибка при создании ветки ~A: ~A~%"
                                                (find-emo ws "error")
                                                remote-name branch-name
                                                (or checkout-err checkout-out "неизвестная ошибка"))))))))))))))
          (error (e)
            (format t "~A [~A] Ошибка: ~A~%"
                    (find-emo ws "error")
                    remote-name
                    e)))))
    ;; Возвращаемся на исходную ветку, чтобы не менять рабочий контекст пользователя.
    (when (and original-branch (not (string= original-branch "HEAD")))
      (multiple-value-bind (checkout-out checkout-err checkout-code)
          (cl-git-tree/git-utils:git-run root "checkout" original-branch)
        (if (zerop checkout-code)
            (format t "~A Возврат на ветку ~A выполнен~%"
                    (find-emo ws "success")
                    original-branch)
            (format t "~A Не удалось вернуться на ветку ~A: ~A~%"
                    (find-emo ws "error")
                    original-branch
                    (or checkout-err checkout-out "неизвестная ошибка")))))
    ws))
