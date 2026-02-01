(in-package :cl-git-tree/loc)

;;; Метод expand-path для раскрытия тильды в путях в контексте workspace

 (defmethod expand-path ((workspace <workspace>) (path string))
   "Раскрыть '~' в пути PATH в контексте WORKSPACE.
 Заменяет ведущий '~' на домашний каталог. Нормализует ./ и ../ компоненты пути.
 Делегирует базовой функции из cl-git-tree/fs:expand-home."
  (cl-git-tree/fs:expand-home path))

 (defmethod expand-path ((workspace <workspace>) (path pathname))
   "Раскрыть '~' в pathname в контексте WORKSPACE.
 Преобразует pathname в строку, раскрывает тильду и возвращает результат."
  (cl-git-tree/fs:expand-home (namestring path)))

;;; Специализация для MSYS2: добавляет префикс диска для абсолютных POSIX-путей

(defun %get-msys2-root-default ()
  "Определяет корень MSYS2 через вызов cygpath -m /"
  (let ((result (string-trim '(#\Space #\Newline #\Return)
                            (uiop:run-program "cygpath -m /"
                                             :output :string
                                             :error-output nil
                                             :ignore-error-status t))))
    (if (plusp (length result))
        (if (char= (char result (1- (length result))) #\/)
            result
            (concatenate 'string result "/"))
        "c:/msys64/")))

(defparameter *msys2-root-default* (%get-msys2-root-default)
  "Значение корня MSYS2 по умолчанию, если MSYS2_ROOT не задан.")

(defun %msys2-root-prefix ()
  "Возвращает префикс корня MSYS2. Можно переопределить через MSYS2_ROOT.
   Если переменная пуста, используется *MSYS2-ROOT-DEFAULT*."
  (let* ((env (uiop:getenv "MSYS2_ROOT"))
         (root (if (and env (plusp (length env))) env *msys2-root-default*)))
    (if (char= (char root (1- (length root))) #\/)
        root
        (concatenate 'string root "/"))))

(defun %msys2-ensure-prefix (path)
  "Если PATH начинается с '//', сохранить как UNC-путь. Если с '/', добавить префикс корня MSYS2. Иначе вернуть как есть."
  (let* ((has-drive (and (> (length path) 1)
                         (char= (char path 1) #\:)))
         (starts-with-double-slash (and (> (length path) 1)
                                        (char= (char path 0) #\/)
                                        (char= (char path 1) #\/)))
         (starts-with-single-slash (and (> (length path) 0)
                                        (char= (char path 0) #\/)
                                        (not starts-with-double-slash))))
    (cond
      (has-drive path)
      (starts-with-double-slash path)  ; UNC-пути: //host/share/... сохраняем как есть
      (starts-with-single-slash (concatenate 'string (%msys2-root-prefix)
                                              (subseq path 1)))
      (t path))))

(defmethod expand-path ((workspace <workspace-msys2>) (path string))
  "MSYS2: раскрыть '~', нормализовать ./ и ../ и добавить Windows-префикс для POSIX-пути '/'."
  (let ((expanded (cl-git-tree/fs:expand-home path)))
    (%msys2-ensure-prefix expanded)))

(defmethod expand-path ((workspace <workspace-msys2>) (path pathname))
  "MSYS2: раскрыть '~' в pathname и добавить префикс MSYS2 для абсолютного POSIX-пути."
  (expand-path workspace (namestring path)))
