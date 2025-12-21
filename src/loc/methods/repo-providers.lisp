(in-package :cl-git-tree/loc)

(defmethod repo-providers ((ws <workspace>))
  "Определяет локации-провайдеры git-репозитория для рабочего пространства
WS по его remotes.

Возвращает список объектов <location>, соответствующих найденным remotes."
  (loop :for pr-key :in (repo-provider-keys ws)
        :for loc := (find-location pr-key)
        :when loc
          :collect loc))

