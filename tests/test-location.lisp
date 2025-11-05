(defpackage :cl-git-tree/tests/location
  (:use :cl :fiveam
        :cl-git-tree/loc))

(in-package :cl-git-tree/tests/location)

(defsuite location-tests
  :description "Тесты для подсистемы cl-git-tree/loc")

(in-suite location-tests)

(test register-and-find-location
  "Проверяем, что register-location добавляет локацию и find-location её находит."
  (let ((loc (make-instance '<location>
                :id "test"
                :description "Тестовая локация"
                :provider :local
                :url-git #P"/tmp/repos/")))
    (register-location loc)
    (let ((found (find-location "test")))
      (is (typep found '<location>))
      (is (string= (<location>-id found) "test"))
      (is (string= (<location>-description found) "Тестовая локация")))))

(test duplicate-location-overwrites
  "Проверяем, что повторная регистрация с тем же id перезаписывает локацию."
  (let ((loc1 (make-instance '<location>
                 :id "dup"
                 :description "Первая версия"
                 :provider :local
                 :url-git #P"/tmp/one/"))
        (loc2 (make-instance '<location>
                 :id "dup"
                 :description "Вторая версия"
                 :provider :local
                 :url-git #P"/tmp/two/")))
    (register-location loc1)
    (register-location loc2)
    (let ((found (find-location "dup")))
      (is (string= (<location>-description found) "Вторая версия"))
      (is (equal (<location>-url-git found) #P"/tmp/two/")))))

(test all-locations-returns-list
  "Проверяем, что all-locations возвращает список зарегистрированных локаций."
  (let ((loc (make-instance '<location>
                :id "list-test"
                :description "Локация для списка"
                :provider :local
                :url-git #P"/tmp/list/")))
    (register-location loc)
    (let ((all (all-locations)))
      (is (listp all))
      (is (find "list-test" (mapcar #'<location>-id all) :test #'string=)))))
