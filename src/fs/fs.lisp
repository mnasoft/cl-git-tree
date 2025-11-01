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

(defun with-each-repo (loc-key fn)
  "Находит все git‑репозитории для локации LOC-KEY и вызывает функцию FN для каждого.

Аргументы:
  LOC-KEY — ключ (обычно строка или символ), по которому ищется объект <location>
            в глобальной таблице cl-git-tree/loc:*locations*.
  FN      — функция, которая будет вызвана для каждого найденного репозитория.

Вызов FN происходит с тремя аргументами:
  (repo-dir loc-key base-url)

  repo-dir — pathname корня git‑репозитория;
  loc-key  — тот же ключ, что был передан в with-each-repo;
  base-url — базовый git‑URL, полученный из объекта <location>.

Если LOC-KEY не найден в *locations* или у него нет git‑URL,
функция выводит сообщение об ошибке и не вызывает FN.

Примеры:
  ;; Вывести список всех репозиториев для локации \"gh\"
  (with-each-repo \"gh\"
    (lambda (repo loc-key base-url)
      (format t \"~&[~A] ~A (base ~A)~%\"
              loc-key repo base-url))))

  ;; Использовать для массового pull
  (with-each-repo \"work\"
    (lambda (repo loc-key base-url)
      (uiop:run-program (list \"git\" \"-C\" (namestring repo) \"pull\"))))"
  (let* ((loc (gethash loc-key cl-git-tree/loc:*locations*))
         (base-url (and loc (cl-git-tree/loc:<location>-url-git loc))))
    (if (null base-url)
        (format t "Неизвестная локация: ~A~%" loc-key)
        (dolist (repo-dir (find-git-repos))
          (funcall fn repo-dir loc-key base-url)))))


(defun with-each-repo-simple (fn)
  "Вызывает функцию FN для каждого найденного git‑репозитория.

Аргументы:
  FN — функция одного аргумента, которая будет вызвана для каждого
       найденного git‑репозитория. Аргументом передаётся pathname
       корневого каталога репозитория.

Функция использует FIND-GIT-REPOS для поиска всех репозиториев,
и для каждого из них вызывает FN.

Возвращаемое значение:
  NIL (результаты вычислений FN игнорируются).

Примеры:
  ;; Вывести список всех найденных репозиториев
  (with-each-repo-simple
    (lambda (repo-dir)
      (format t \"~&Repo: ~A~%\" repo-dir)))

  ;; Выполнить git pull во всех репозиториях
  (with-each-repo-simple
    (lambda (repo-dir)
      (uiop:run-program (list \"git\" \"-C\" (namestring repo-dir) \"pull\"))))"
  (dolist (repo-dir (find-git-repos))
    (funcall fn repo-dir)))


(defun with-repo (fn args)
  (dolist (repo-dir (find-git-repos))
    (funcall fn repo-dir args)))
