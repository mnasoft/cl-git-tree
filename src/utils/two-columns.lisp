(defpackage :cl-git-tree/utils/two-columns
  (:use :cl)
  (:export 
   #:print-two-columns
   #:format-two-columns))

(in-package :cl-git-tree/utils/two-columns)

(defun print-two-columns (items &key (column-width 40) (separator "  ") (stream t))
  "Вывести список ITEMS в две колонки с заданной шириной колонок.
  
  Аргументы:
    items - список элементов для вывода
    column-width - ширина одной колонки (по умолчанию 40)
    separator - разделитель между колонками (по умолчанию два пробела)
    stream - поток для вывода (по умолчанию стандартный вывод)
    
  Пример:
    (print-two-columns '(\"apple\" \"banana\" \"cherry\" \"date\" \"elderberry\" \"fig\"))
    
    apple                                   banana
    cherry                                  date
    elderberry                              fig"
  
  (let* ((items-list (if (listp items) items (list items)))
         (sorted (sort (copy-list items-list) #'string<))
         (count (length sorted))
         (rows (ceiling count 2)))
    
    (dotimes (row rows)
      (let ((left-idx row)
            (right-idx (+ row rows)))
        
        ;; Левая колонка
        (if (< left-idx count)
            (let ((left-item (format nil "~a" (nth left-idx sorted))))
              (format stream "~a" left-item)
              ;; Выравнивание до ширины колонки
              (dotimes (i (- column-width (length left-item)))
                (format stream " ")))
            ;; Если элемента нет, пропускаем место
            (dotimes (i column-width)
              (format stream " ")))
        
        ;; Разделитель
        (format stream "~a" separator)
        
        ;; Правая колонка
        (if (< right-idx count)
            (format stream "~a" (nth right-idx sorted)))
        
        ;; Новая строка
        (format stream "~%")))))


(defun format-two-columns (items &key (column-width 40) (separator "  "))
  "Вернуть строку с форматированием списка ITEMS в две колонки.
  
  Аргументы:
    items - список элементов
    column-width - ширина одной колонки (по умолчанию 40)
    separator - разделитель между колонками (по умолчанию два пробела)
    
  Возвращает:
    строку с отформатированным списком"
  
  (with-output-to-string (stream)
    (print-two-columns items 
                       :column-width column-width 
                       :separator separator
                       :stream stream)))


;; Альтернативная версия без сортировки (сохраняет исходный порядок)
(defun print-two-columns-unsorted (items &key (column-width 40) (separator "  ") (stream t))
  "Вывести список ITEMS в две колонки без сортировки (в исходном порядке).
  
  Аргументы:
    items - список элементов для вывода
    column-width - ширина одной колонки (по умолчанию 40)
    separator - разделитель между колонками (по умолчанию два пробела)
    stream - поток для вывода (по умолчанию стандартный вывод)"
  
  (let* ((items-list (if (listp items) items (list items)))
         (count (length items-list))
         (rows (ceiling count 2)))
    
    (dotimes (row rows)
      (let ((left-idx row)
            (right-idx (+ row rows)))
        
        ;; Левая колонка
        (if (< left-idx count)
            (let ((left-item (format nil "~a" (nth left-idx items-list))))
              (format stream "~a" left-item)
              ;; Выравнивание до ширины колонки
              (dotimes (i (- column-width (length left-item)))
                (format stream " ")))
            ;; Если элемента нет, пропускаем место
            (dotimes (i column-width)
              (format stream " ")))
        
        ;; Разделитель
        (format stream "~a" separator)
        
        ;; Правая колонка
        (if (< right-idx count)
            (format stream "~a" (nth right-idx items-list)))
        
        ;; Новая строка
        (format stream "~%")))))
