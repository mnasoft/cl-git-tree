(in-package :cl-git-tree/loc)

(defmethod repo-provider-keys ((ws <workspace>))
  "Определяет ключи (имена remotes), которые соответствуют зарегистрированным локациям
для рабочего пространства WS.

Возвращает список строк-ключей локаций (например, (\"lc\" \"gh\"))."
  (let ((repo-dir (or (git-root ws)
                      (<workspace>-path ws))))
    (when repo-dir
      (loop :for remote :in (cl-git-tree/git-utils:repo-remotes repo-dir)
            :when (find-location remote)
            :collect remote))))
