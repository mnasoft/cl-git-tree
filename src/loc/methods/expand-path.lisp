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
