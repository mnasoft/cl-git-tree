;;;; tests/test-os-detection.lisp

(in-package :cl-git-tree/tests)

(def-suite os-detection
  :description "Тесты для определения ОС и создания workspace для разных платформ"
  :in all)

(in-suite os-detection)

(def-test detect-os-returns-symbol ()
  "Функция detect-os должна возвращать символ."
  (let ((os (cl-git-tree/loc:detect-os)))
    (is (symbolp os))
    (is (member os '(:linux :windows :msys2)))))

(def-test workspace-has-os-type ()
  "Workspace должен иметь слот os-type."
  (let ((ws (cl-git-tree/loc:make-workspace #P"/tmp/test")))
    (is (slot-boundp ws 'cl-git-tree/loc::os-type))
    (is (symbolp (cl-git-tree/loc:<workspace>-os-type ws)))
    (is (member (cl-git-tree/loc:<workspace>-os-type ws) '(:linux :windows :msys2)))))

(def-test make-workspace-creates-correct-class ()
  "make-workspace должен создавать экземпляр правильного класса в зависимости от ОС."
  (let* ((os (cl-git-tree/loc:detect-os))
         (ws (cl-git-tree/loc:make-workspace #P"/tmp/test" :description "Test WS"))
         (expected-class (case os
                           (:linux 'cl-git-tree/loc:<workspace-linux>)
                           (:windows 'cl-git-tree/loc:<workspace-windows>)
                           (:msys2 'cl-git-tree/loc:<workspace-msys2>)
                           (t 'cl-git-tree/loc:<workspace>))))
    (is (typep ws expected-class))
    (is (equal (cl-git-tree/loc:<workspace>-os-type ws) os))
    (is (string= (cl-git-tree/loc:<workspace>-description ws) "Test WS"))))

(def-test workspace-linux-inheritance ()
  "Проверка наследования для workspace-linux."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace-linux> 
                           :path #P"/tmp/test")))
    (is (typep ws 'cl-git-tree/loc:<workspace-linux>))
    (is (typep ws 'cl-git-tree/loc:<workspace>))))

(def-test workspace-windows-inheritance ()
  "Проверка наследования для workspace-windows."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace-windows> 
                           :path #P"/tmp/test")))
    (is (typep ws 'cl-git-tree/loc:<workspace-windows>))
    (is (typep ws 'cl-git-tree/loc:<workspace>))))

(def-test workspace-msys2-inheritance ()
  "Проверка наследования для workspace-msys2."
  (let ((ws (make-instance 'cl-git-tree/loc:<workspace-msys2> 
                           :path #P"/tmp/test")))
    (is (typep ws 'cl-git-tree/loc:<workspace-msys2>))
    (is (typep ws 'cl-git-tree/loc:<workspace>))))
