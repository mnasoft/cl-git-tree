(in-package :cl-git-tree/tests)

(def-suite make-workspace
  :description "Тесты для подсистемы cl-git-tree/workspace"
  :in workspace)

(in-suite make-workspace)

(def-test create-with-explicit-description ()
  "Создание workspace с явным описанием."
  (let ((ws (cl-git-tree/loc:make-workspace "/tmp/ws" :description "Тестовый WS")))
    (is (typep ws 'cl-git-tree/loc:<workspace>))
    (is (equal (namestring (cl-git-tree/loc::<workspace>-path ws))
               (namestring (truename "/tmp/ws"))))
    (is (string= (cl-git-tree/loc::<workspace>-description ws)
                 "Тестовый WS"))))

(def-test create-without-description ()
  "Создание workspace без описания — берётся имя каталога."
  (let ((ws (cl-git-tree/loc:make-workspace "/tmp/ws")))
    (is (typep ws 'cl-git-tree/loc:<workspace>))
    (is (string= (cl-git-tree/loc:<workspace>-description ws)
                 "ws"))))

(def-test description-type ()
  "Описание всегда строка."
  (let ((ws (cl-git-tree/loc:make-workspace "/tmp/ws")))
    (is (stringp (cl-git-tree/loc:<workspace>-description ws)))))

(def-test path-type ()
  "Path всегда pathname."
  (let ((ws (cl-git-tree/loc:make-workspace "/tmp/ws")))
    (is (pathnamep (cl-git-tree/loc:<workspace>-path ws)))))
