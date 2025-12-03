;;;; ./src/shell-utils/ssh-utils.lisp

(in-package :cl-git-tree/shell-utils)

(defun parse-ssh-url (url)
  "Парсит SSH URL в компоненты (user host path).
   
   Форматы:
   - ssh://user@host/path/to/repos
   - user@host:/path/to/repos
   - host:/path/to/repos (без user, используется текущий)
   
   Возвращает: (list user host path) или NIL если формат неправильный."
  (cond
    ;; ssh://user@host/path
    ((uiop:string-prefix-p "ssh://" url)
     (let* ((rest (subseq url 6)) ; убираем "ssh://"
            (at-pos (position #\@ rest))
            (slash-pos (position #\/ rest :start (or at-pos 0))))
       (when (and at-pos slash-pos)
         (list (subseq rest 0 at-pos)
               (subseq rest (1+ at-pos) slash-pos)
               (subseq rest slash-pos)))))
    
    ;; user@host:/path или host:/path
    ((and (position #\@ url)
          (position #\: url))
     (let* ((at-pos (position #\@ url))
            (colon-pos (position #\: url)))
       (when (< at-pos colon-pos)
         (list (subseq url 0 at-pos)
               (subseq url (1+ at-pos) colon-pos)
               (subseq url (1+ colon-pos))))))
    
    ;; Просто host:/path (user не указан, используется текущий)
    ((position #\: url)
     (let* ((colon-pos (position #\: url)))
       (list nil
             (subseq url 0 colon-pos)
             (subseq url (1+ colon-pos)))))
    
    (t nil)))

(defun ssh-run (user host command)
  "Выполняет команду на удалённом сервере через SSH.
   
   USER — имя пользователя (если nil, используется текущий).
   HOST — хост сервера.
   COMMAND — команда (строка или список аргументов).
   
   Возвращает: (values stdout stderr exit-code)"
  (let* ((cmd-str (if (listp command)
                      (format nil "~{~A~^ ~}" command)
                      command))
         ;; Экранируем одиночные кавычки и оборачиваем в них
         (escaped-cmd (format nil "'~A'" (cl-ppcre:regex-replace-all "'" cmd-str "'\\''")))
         (ssh-cmd (if user
                      (format nil "ssh ~A@~A ~A" user host escaped-cmd)
                      (format nil "ssh ~A ~A" host escaped-cmd))))
    (handler-case
        (multiple-value-bind (out err code)
            (uiop:run-program ssh-cmd
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (values out err code))
      (error (e)
        (values "" (format nil "SSH error: ~A" e) 1)))))

(defun ssh-cmd (user host &rest parts)
  "Вспомогательная функция для построения SSH команды.
   Возвращает готовый SSH вызов."
  (let* ((cmd-str (format nil "~{~A~^ ~}" parts))
         (escaped-cmd (format nil "'~A'" (cl-ppcre:regex-replace-all "'" cmd-str "'\\''")))
         (ssh-cmd (if user
                      (format nil "ssh ~A@~A ~A" user host escaped-cmd)
                      (format nil "ssh ~A ~A" host escaped-cmd))))
    ssh-cmd))
