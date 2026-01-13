;;;; ./src/commands/single-repo/merge.lisp

(defpackage :cl-git-tree/commands/single-repo/merge
  (:use :cl)
  (:export cmd-merge))

(in-package :cl-git-tree/commands/single-repo/merge)

(defun cmd-merge (&rest args)
  "CLI-команда: объединить две или более веток разработки.
Использование:
  git-tree merge [опции] <branch>
  git-tree merge (--continue | --abort | --quit)"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Объединить две или более веток разработки.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree merge [опции] <branch>~%")
     (format t "  git-tree merge --continue~%")
     (format t "  git-tree merge --abort~%")
     (format t "  git-tree merge --quit~%~%")
     (format t "Опции:~%")
     (format t "  --no-commit            не создавать коммит автоматически~%")
     (format t "  --squash               сжать все коммиты в один~%")
     (format t "  --no-edit              не редактировать сообщение коммита~%")
     (format t "  --no-verify            пропустить pre-merge и commit-msg хуки~%")
     (format t "  -s <strategy>          стратегия слияния (ours, recursive)~%")
     (format t "  -X <option>            опции стратегии (theirs, ours)~%")
     (format t "  -S[<keyid>]            подписать коммит GPG~%")
     (format t "  --allow-unrelated-histories разрешить несвязанные истории~%")
     (format t "  --stat                 показать диффстат~%")
     (format t "  --no-stat              не показывать диффстат~%")
     (format t "  --compact-summary      компактная сводка~%")
     (format t "  -m <message>           сообщение коммита~%")
     (format t "  --continue             продолжить после разрешения конфликтов~%")
     (format t "  --abort                прервать слияние~%")
     (format t "  --quit                 выйти из процесса слияния~%")
     (format t "~%Примеры:~%")
     (format t "  git-tree merge feature/new~%")
     (format t "  git-tree merge --no-commit develop~%")
     (format t "  git-tree merge --continue~%"))
    ((null args)
     (format t "~A Укажите ветку для слияния или опцию (--continue, --abort, --quit)~%"
             (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "error")))
    (t
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            (continue (member "--continue" args :test #'string=))
            (abort (member "--abort" args :test #'string=))
            (quit (member "--quit" args :test #'string=))
            (no-commit (member "--no-commit" args :test #'string=))
            (squash (member "--squash" args :test #'string=))
            (no-edit (member "--no-edit" args :test #'string=))
            (no-verify (member "--no-verify" args :test #'string=))
            (allow-unrelated-histories (member "--allow-unrelated-histories" args :test #'string=))
            (no-rerere-autoupdate (member "--no-rerere-autoupdate" args :test #'string=))
            (stat (member "--stat" args :test #'string=))
            (no-stat (member "--no-stat" args :test #'string=))
            (compact-summary (member "--compact-summary" args :test #'string=))
            ;; Ищем -s <strategy>
            (strategy-pos (position "-s" args :test #'string=))
            (strategy (when (and strategy-pos (< (1+ strategy-pos) (length args)))
                        (nth (1+ strategy-pos) args)))
            ;; Ищем -X <option>
            (strategy-option-pos (position "-X" args :test #'string=))
            (strategy-option (when (and strategy-option-pos 
                                       (< (1+ strategy-option-pos) (length args)))
                              (nth (1+ strategy-option-pos) args)))
            ;; Ищем -S или -S<keyid>
            (keyid-arg (find-if (lambda (arg)
                                  (cl-ppcre:scan "^-S" arg))
                               args))
            (keyid (when keyid-arg
                     (if (> (length keyid-arg) 2)
                         (subseq keyid-arg 2)
                         t)))
            ;; Ищем -m <message>
            (message-pos (position "-m" args :test #'string=))
            (message (when (and message-pos (< (1+ message-pos) (length args)))
                       (nth (1+ message-pos) args)))
            ;; Ищем --into-name <branch>
            (into-name-pos (position "--into-name" args :test #'string=))
            (into-name (when (and into-name-pos (< (1+ into-name-pos) (length args)))
                         (nth (1+ into-name-pos) args)))
            ;; Source - первый аргумент, который не является опцией
            (source (unless (or continue abort quit)
                      (find-if-not (lambda (arg)
                                     (or (string= arg "--no-commit")
                                         (string= arg "--squash")
                                         (string= arg "--no-edit")
                                         (string= arg "--no-verify")
                                         (string= arg "--allow-unrelated-histories")
                                         (string= arg "--no-rerere-autoupdate")
                                         (string= arg "--stat")
                                         (string= arg "--no-stat")
                                         (string= arg "--compact-summary")
                                         (string= arg "-s")
                                         (string= arg "-X")
                                         (string= arg "-m")
                                         (string= arg "--into-name")
                                         (and strategy (string= arg strategy))
                                         (and strategy-option (string= arg strategy-option))
                                         (and message (string= arg message))
                                         (and into-name (string= arg into-name))
                                         (and keyid-arg (string= arg keyid-arg))))
                                   args))))
       (handler-case
           (cl-git-tree/loc:repo-merge ws source
                                       :continue continue
                                       :abort abort
                                       :quit quit
                                       :no-commit no-commit
                                       :squash squash
                                       :no-edit no-edit
                                       :no-verify no-verify
                                       :strategy strategy
                                       :strategy-option strategy-option
                                       :keyid keyid
                                       :allow-unrelated-histories allow-unrelated-histories
                                       :no-rerere-autoupdate no-rerere-autoupdate
                                       :message message
                                       :into-name into-name
                                       :stat stat
                                       :no-stat no-stat
                                       :compact-summary compact-summary)
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "merge" #'cmd-merge "Объединить две или более веток разработки"))
