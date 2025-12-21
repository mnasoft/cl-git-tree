;;;; tests/test-expand-path-msys2.lisp

(in-package :cl-git-tree/tests)

(def-suite expand-path-msys2
  :description "Тесты для expand-path под <workspace-msys2>"
  :in all)

(in-suite expand-path-msys2)

(def-test expand-path-msys2-default-prefix ()
  "Абсолютный POSIX-путь '/' должен получить префикс c:/msys2/ по умолчанию."
  (let ((cl-git-tree/loc::*msys2-root-default* "c:/msys2/"))
    (let* ((ws (make-instance 'cl-git-tree/loc:<workspace-msys2>))
           (out (cl-git-tree/loc:expand-path ws "/foo/bar")))
      (is (string= "c:/msys2/foo/bar/" out)))))

(def-test expand-path-msys2-env-override ()
  "MSYS2_ROOT переопределяет префикс для абсолютных POSIX-путей."
  (let ((cl-git-tree/loc::*msys2-root-default* "d:/msys64/"))
    (let* ((ws (make-instance 'cl-git-tree/loc:<workspace-msys2>))
           (out (cl-git-tree/loc:expand-path ws "/proj"))
           (out-drive (cl-git-tree/loc:expand-path ws "d:/data")))
      (is (string= "d:/msys64/proj/" out))
      (is (string= "d:/data/" out-drive)))))
