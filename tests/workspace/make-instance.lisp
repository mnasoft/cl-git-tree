(in-package :cl-git-tree/tests)

(def-suite workspace-make-instance
  :description "Тесты для подсистемы cl-git-tree/workspace связанные с make-instance"
  :in workspace)

(in-suite workspace-make-instance)

(def-test create-with-path ()
  "Создание workspace с явным path."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace>
                           :path #P"/tmp/ws")))
    (is (typep ws 'cl-git-tree/loc:<workspace>))
    (is (equal (namestring (cl-git-tree/loc:<workspace>-path ws))
               "/tmp/ws"))
    (is (string= (cl-git-tree/loc:<workspace>-description ws) ""))))

(def-test create-with-description ()
  "Создание workspace с описанием."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace>
                           :path #P"/tmp/ws"
                           :description "Тестовый WS")))
    (is (string= (cl-git-tree/loc:<workspace>-description ws)
                 "Тестовый WS"))))

(def-test default-initforms ()
  "Проверка initform’ов."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace>)))
    (is (null (cl-git-tree/loc:<workspace>-path ws)))
    (is (string= (cl-git-tree/loc:<workspace>-description ws) ""))))

(def-test path-type ()
  "Проверка типа path."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace>
                           :path #P"/tmp/ws")))
    (is (pathnamep (cl-git-tree/loc:<workspace>-path ws)))))

(def-test description-type ()
  "Проверка типа description."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace>
                           :description "Описание")))
    (is (stringp (cl-git-tree/loc:<workspace>-description ws)))))
