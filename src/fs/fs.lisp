(in-package :cl-git-tree/fs)

(defun repo-name (repo-dir)
  "Возвращает имя каталога git‑репозитория по его пути.

ARGUMENTS:
  REPO-DIR — путь к каталогу репозитория (строка или pathname).
             Может содержать завершающий '/'.

Функция нормализует путь (через TRUENAME), убирает завершающий '/',
и возвращает только имя последнего каталога в виде строки.

Примеры:
  (repo-name #P\"/home/user/projects/cl-git-tree/\")
  ;; => \"cl-git-tree\"

  (repo-name \"./repos/test/\")
  ;; => \"test\""
  (let* ((truename (truename repo-dir))
         (dir-str  (string-right-trim "/" (namestring truename))))
    (namestring (path:basename dir-str))))

(defun git-repo-p (dir)
  "Проверяет, является ли каталог DIR git‑репозиторием.

Аргументы:
  DIR — путь к каталогу (строка или pathname).

Функция возвращает T, если внутри DIR существует подкаталог \".git\" —
это стандартный маркер git‑репозитория. В противном случае возвращает NIL.

Примеры:
  (git-repo-p #P\"/home/user/project/\")
  ;; => T, если /home/user/project/.git существует

  (git-repo-p \"./tmp/\")
  ;; => NIL, если ./tmp/.git отсутствует"
  (probe-file (merge-pathnames ".git/" dir)))

(defun find-git-repos (&optional (root (truename ".")))
  "Рекурсивно ищет все git‑репозитории, начиная с каталога ROOT.

Аргументы:
  ROOT — путь к корневому каталогу поиска (строка или pathname).
         По умолчанию используется текущий каталог (TRUENAME \".\").

Функция обходит все подкаталоги ROOT и возвращает список каталогов,
в которых обнаружен подкаталог \".git\" — то есть каталогов,
являющихся git‑репозиториями.

Возвращаемое значение:
  Список pathnames, каждый из которых указывает на корень git‑репозитория.

Примеры:
  ;; Найти все репозитории в текущем каталоге и ниже
  (find-git-repos)
  ;; => (#P\"/home/user/project1/\" #P\"/home/user/project2/\")

  ;; Найти репозитории в указанной директории
  (find-git-repos #P\"/srv/repos/\")
  ;; => (#P\"/srv/repos/app/\" #P\"/srv/repos/lib/\")"
  (labels ((scan (dir)
             (let ((results '()))
               (when (git-repo-p dir)
                 (push dir results))
               (dolist (sub (directory (merge-pathnames "*/" dir)))
                 ;; пропускаем .git, чтобы не углубляться внутрь
                 (unless (string= (file-namestring sub) ".git/")
                   (setf results (nconc results (scan sub)))))
               results)))
    (scan root)))

(defun with-repo (fn args)
  (dolist (repo-dir (find-git-repos))
    (funcall fn repo-dir args)))

;; Нормализация и раскрытие путей с тильдой

(defun normalize-pathname (pn)
  "Нормализует pathname, раскрывая . и .. компоненты директории.
Возвращает новый pathname без . и .. в directory-компонентах."
  (let* ((dir (pathname-directory pn))
         (cleaned-dir
          (when dir
            (let ((result (if (eq (car dir) :ABSOLUTE)
                              '(:ABSOLUTE)
                              '(:RELATIVE))))
              (dolist (component (cdr dir))
                (cond
                  ((equal component ".")  nil)
                  ((equal component "..")
                   (when (> (length result) 1)
                     (setf result (butlast result))))
                  (t
                   (setf result (append result (list component))))))
              (or result '(:ABSOLUTE))))))
    (make-pathname :directory cleaned-dir
                   :device (pathname-device pn)
                   :host (pathname-host pn)
                   :name (pathname-name pn)
                   :type (pathname-type pn))))

(defun expand-tilde-directory-path (path)
  "Раскрывает префикс ~ в начале PATH и возвращает полную путь-строку (namestring).
Нормализует ./ и ../ компоненты пути (/./ → /, path/../ → path/).
Если PATH не начинается с ~, возвращает нормализованный namestring директории."
  (let* ((home-path (uiop:ensure-directory-pathname (uiop:getenv "HOME")))
         (str (if (pathnamep path) (namestring path) path)))
    (let* ((expanded
            (if (and str (> (length str) 0) (char= (char str 0) #\~))
                (let* ((rest (subseq str 1))
                       (rel (if (and (> (length rest) 0)
                                      (member (char rest 0) '(#\/ #\\)))
                                 (subseq rest 1)
                                 rest)))
                  (merge-pathnames (uiop:parse-unix-namestring rel) home-path))
                (uiop:parse-unix-namestring str)))
           (normalized (normalize-pathname expanded))
           (as-dir (uiop:ensure-directory-pathname normalized)))
      (namestring as-dir))))

(defun expand-home (path)
  "Заменяет ведущий '~' на домашний каталог (Linux/MSYS2).
Нормализует ./ и ../ компоненты пути: /./ → /, path/../ → path/.
Общая обёртка над EXPAND-TILDE-DIRECTORY-ПATH для всего проекта."
  (expand-tilde-directory-path path))

(defun to-posix-path (windows-or-posix-path)
  "Преобразует Windows-путь в POSIX-путь используя cygpath для MSYS2.
Если путь уже POSIX, возвращает как есть."
  (let* ((path-str (if (pathnamep windows-or-posix-path)
                        (namestring windows-or-posix-path)
                        windows-or-posix-path))
         (is-windows (or (search ":" path-str)
                         (search "\\" path-str))))
    (if is-windows
        (string-trim '(#\Newline #\Return #\Space)
                    (uiop:run-program
                     (list "cygpath" "-u" path-str)
                     :output :string
                     :error-output nil
                     :ignore-error-status t))
        path-str)))


;;; ----------------------------------------------------------------------
;;; Утилиты для работы с tar.xz‑архивами git‑репозиториев
;;; ----------------------------------------------------------------------

(defun create-tar-xz-archive (repo-dir output-path)
  "Создаёт tar.xz‑архив git‑репозитория REPO-DIR в каталоге OUTPUT-PATH.

Архив содержит только голый git‑репозиторий (--bare, без рабочих файлов).
Возвращает (values ARCHIVE-NAME OUTPUT-PATH) при успехе или (values NIL NIL)
при ошибке. Путь OUTPUT-PATH может содержать тильду и будет раскрыт."
  (let* ((repo-name (repo-name repo-dir))
         (archive-name (format nil "~A.tar.xz" repo-name))
         ;; Раскрываем output-path в случае, если там есть тильда
         (expanded-output-path (expand-home output-path))
         (archive-path (merge-pathnames archive-name expanded-output-path))
         (bare-name (concatenate 'string repo-name ".git"))
         (temp-dir (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (make-pathname :directory (list :relative (format nil "tmp-git-tree-~A" (random 1000000))))
                     (uiop:temporary-directory)))))
    (ensure-directories-exist expanded-output-path)

    ;; Создаём голый клон во временной директории
    (multiple-value-bind (out1 err1 code1)
        (uiop:run-program
         (list "git" "clone" "--bare" (namestring repo-dir)
               (namestring (merge-pathnames bare-name temp-dir)))
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore out1))
      (format t "DEBUG: git clone code1=~A~%" code1)
      (when (> (length err1) 0)
        (format t "DEBUG: git clone stderr=~A~%" err1))

      (if (zerop code1)
          (progn
            ;; Архивируем голый репозиторий
            (let* ((temp-dir-posix (to-posix-path (namestring temp-dir)))
                   (archive-path-posix (to-posix-path (namestring archive-path)))
                   (tar-cmd-str (format nil "tar -C ~A -c -J -f ~A ~A"
                                        temp-dir-posix
                                        archive-path-posix
                                        bare-name)))
              (multiple-value-bind (out err code)
                  (uiop:run-program
                   tar-cmd-str
                   :output :string
                   :error-output :string
                   :ignore-error-status t)
                (declare (ignore out))

                ;; Очищаем временный каталог
                (delete-directory-tree temp-dir)

                (if (zerop code)
                    (values archive-name (namestring expanded-output-path))
                    (progn
                      (format t "❌ Ошибка при архивировании:~%~A~%" err)
                      (values nil nil)))))))
          (progn
            ;; Очищаем временный каталог при ошибке
            (ignore-errors (delete-directory-tree temp-dir))
            (format t "❌ Ошибка при создании голого клона:~%~A~%" err1)
            (values nil nil)))))

(defun delete-directory-tree (target-path)
  "Удаляет каталог с содержимым через shell команду (rm -rf) вместо UIOP для совместимости с MSYS2.

ARGUMENTS:
  TARGET-PATH — путь к каталогу, который нужно удалить (строка или pathname).

DESCRIPTION:
  Функция использует shell команду 'rm -rf' вместо UIOP:DELETE-DIRECTORY-TREE,
  так как последняя работает нестабильно под MSYS2. Команда выполняется через
  CL-GIT-TREE/SHELL-UTILS:SHELL-RUN-SINGLE.

  Удаление выполняется из корневого каталога (текущей директории) для избежания
  проблем с путями в MSYS2.

RETURNS:
  T если операция успешна, NIL в случае ошибки.

EXAMPLE:
  (delete-directory-tree #P\"/tmp/test-dir/\")
  ;; => T"
  (let ((path-str (namestring (pathname target-path))))
    (handler-case
        (progn
          (cl-git-tree/shell-utils:shell-run-single "." "rm" "-rf" path-str)
          t)
      (error (e)
        (format t "❌ Ошибка при удалении ~A: ~A~%" path-str e)
        nil))))
