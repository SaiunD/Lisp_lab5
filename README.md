<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/> 
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">
    <strong>Студентка</strong>: <em><strong>Саюн Дарина Миколаївна</strong></em>
</p>
<p align="right">
    <strong>Група</strong>: <em><strong>КВ-13</strong></em>
</p>
<p align="right">
    <strong>Рік</strong>: <em><strong>2024</strong></em>
</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом 
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею. 
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом). 
2. Розробити утиліту(-и) для зчитування таблиць з файлів. 
3. Розробити функцію `select` , яка отримує на вхід шлях до файлу з таблицею, а 
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція
і т. і. За потреби параметрів може бути кілька. `select` повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у `select` . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів. 
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від варіанту): 
- структури у геш-таблиці 
- геш-таблиці у асоціативні списки 
- асоціативні списки у геш-таблиці 
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 4 (16)
|  Варіант  |         База даних           |  Тип записів   |
|-----------|------------------------------|----------------|
|     4     | Проєкти із застосуванням ШІ  |    Структура   |

|    Назва    |  Таблиці  |                  Опис                  |
|-------------|-----------|----------------------------------------|
| Проєкти із застосуванням ШІ | 1. Проєкти 2. Моделі штучного інтелекту | База даних моделей штучного інтелекту та проєктів, в яких вони використовуються. |

## Вміст таблиць csv
 projects.csv
| ID  | Name                 | AI Model ID | Description                     |
|-----|----------------------|-------------|---------------------------------|
| 1   | AI for Healthcare    | 2           | System for medical diagnostics |
| 2   | Autonomous Vehicles  | 3           | Self-driving car algorithms    |
| 3   | NLP Chatbot          | 4           | Conversational AI for customer support |
 
 ai_models.csv
| ID  | Name           | Type              | Details                                     |
|-----|----------------|-------------------|---------------------------------------------|
| 1   | CNN            | Neural Network    | Convolutional Neural Network for image processing |
| 2   | Decision Tree  | Tree              | Tree-based model for classification         |
| 3   | Transformer    | Neural Network    | Advanced model for language tasks           |
| 4   | Markov Chain   | Probabilistic     | Statistical model for sequences             |

## Лістинг реалізації завдання
```lisp
;; --- Структури ---

(defstruct project
  id
  name
  ai-model-id
  description)

(defstruct ai-model
  id
  name
  type
  details)

;;; --- Утиліти для створення записів ---

(defun make-project-from-list (fields)
  "Створює структуру PROJECT зі списку полів."
  (make-project
   :id (parse-integer (nth 0 fields))
   :name (string-trim '(#\Space) (nth 1 fields))
   :ai-model-id (parse-integer (nth 2 fields))
   :description (string-trim '(#\Space) (nth 3 fields))))

(defun make-ai-model-from-list (fields)
  "Створює структуру AI-MODEL зі списку полів."
  (make-ai-model
   :id (parse-integer (nth 0 fields))
   :name (string-trim '(#\Space) (nth 1 fields))
   :type (string-trim '(#\Space) (nth 2 fields))
   :details (string-trim '(#\Space) (nth 3 fields))))

;;; --- Утиліти для зчитування таблиць ---

(defun split-string-custom (string delimiter)
  "Розбиває рядок STRING на частини, використовуючи символ DELIMITER."
  (let ((result '())
        (start 0)
        (length (length string)))
    (dotimes (i (1+ length))
      (let ((char (if (< i length) (char string i) nil)))
        (if (or (eql char delimiter) (null char))
            (progn
              (when (< start i)
                (push (subseq string start i) result))
              (setf start (1+ i))))))
    (nreverse result)))

(defun read-table (filename make-record-fn)
  "Загальна функція зчитування таблиць. 
FILENAME - ім'я файлу, MAKE-RECORD-FN - функція для створення записів."
  (let ((records '()))
    (with-open-file (stream filename :direction :input)
      (read-line stream) ;; Пропускаємо заголовок
      (loop for line = (read-line stream nil nil)
            while line
            do (push (funcall make-record-fn (split-string-custom line #\,)) records)))
    (nreverse records)))

(defun read-projects-table (filename)
  "Зчитує таблицю з файлу і повертає список структур PROJECT."
  (read-table filename #'make-project-from-list))

(defun read-ai-models-table (filename)
  "Зчитує таблицю з файлу і повертає список структур AI-MODEL."
  (read-table filename #'make-ai-model-from-list))


;;; --- Функція SELECT ---

(defun select (filename table-reader)
  "Зчитує дані з файлу FILENAME за допомогою TABLE-READER і повертає лямбда-функцію для фільтрації структур за ключовими параметрами."
  (let ((records (funcall table-reader filename)))
    (lambda (&rest filters)
      (remove-if-not
       (lambda (record)
         (loop for (key value) on filters by #'cddr
               always (equal value
                             (slot-value record
                                         (intern (symbol-name key) *package*)))))
       records))))

;;; --- Конвертація записів ---

(defun project-to-hashtable (project)
  "Перетворює структуру PROJECT у геш-таблицю."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'id ht) (project-id project)
          (gethash 'name ht) (project-name project)
          (gethash 'ai-model-id ht) (project-ai-model-id project)
          (gethash 'description ht) (project-description project))
    ht))

(defun convert-projects-to-hashtables (projects)
  "Перетворює список структур PROJECT у список геш-таблиць."
  (mapcar #'project-to-hashtable projects))


(defun ai-model-to-hashtable (ai-model)
  "Перетворює структуру AI-MODEL у геш-таблицю."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash 'id ht) (ai-model-id ai-model)
          (gethash 'name ht) (ai-model-name ai-model)
          (gethash 'type ht) (ai-model-type ai-model)
          (gethash 'details ht) (ai-model-details ai-model))
    ht))

(defun convert-ai-models-to-hashtables (ai-models)
"Перетворює список структур AI-MODEL у список геш-таблиць."
  (mapcar #'ai-model-to-hashtable ai-models))

;;; --- Утиліта для виводу ---

(defun print-projects-table (projects)
  "Виводить таблицю проектів."
  (format t "~%~A~%" (make-string 103 :initial-element #\-))
  (format t "| ~5A | ~23A | ~12A | ~50A |~%" "ID" "Name" "AI Model ID" "Description")
  (format t "~A~%" (make-string 103 :initial-element #\-))
  (dolist (project projects)
    (format t "| ~5A | ~23A | ~12A | ~50A |~%"
            (project-id project)
            (project-name project)
            (project-ai-model-id project)
            (project-description project)))
  (format t "~A~%" (make-string 103 :initial-element #\-))
  (format t "~%"))

(defun print-ai-models-table (ai-models)
  "Виводить таблицю моделей AI."
  (format t "~%~A~%" (make-string 108 :initial-element #\-))
  (format t "| ~5A | ~20A | ~20A | ~50A |~%" "ID" "Name" "Type" "Details")
  (format t "~A~%" (make-string 108 :initial-element #\-))
  (dolist (model ai-models)
    (format t "| ~5A | ~20A | ~20A | ~50A |~%"
            (ai-model-id model)
            (ai-model-name model)
            (ai-model-type model)
            (ai-model-details model)))
  (format t "~A~%" (make-string 108 :initial-element #\-))
  (format t "~%"))

;;; --- Запис у файл ---

(defun write-projects-to-csv (file-path records &optional write-headers)
  "Записує список структур PROJECT у файл CSV.
WRITE-HEADERS - якщо T, додає заголовки до CSV."
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (when write-headers
      (format stream "ID,Name,AI Model ID,Description~%"))
    (dolist (record records)
      (format stream "~A,~A,~A,~A~%"
              (project-id record)
              (project-name record)
              (project-ai-model-id record)
              (project-description record)))))

(defun write-ai-models-to-csv (file-path records &optional write-headers)
  "Записує список структур AI-MODEL у файл CSV.
WRITE-HEADERS - якщо T, додає заголовки до CSV."
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (when write-headers
      (format stream "ID,Name,Type,Details~%"))
    (dolist (record records)
      (format stream "~A,~A,~A,~A~%"
              (ai-model-id record)
              (ai-model-name record)
              (ai-model-type record)
              (ai-model-details record)))))
```
### Тестові набори та утиліти
```lisp
;;; --- Тестові утиліти ---

(defun test-read-from-tabels ()
  "Тестує зчитування таблиць і виводить їх."
  (let ((projects (read-projects-table "projects/lab5/projects.csv"))
        (ai-models (read-ai-models-table "projects/lab5/ai_models.csv")))
    (format t "~&--- Projects ---~%")
    (dolist (project projects)
      (print project))
    (format t "~&~%")
    (format t "--- AI Models ---~%")
    (dolist (ai-model ai-models)
      (print ai-model))))

(defun test-select ()
  "Тестує функцію SELECT з ключовими параметрами."
  (let* ((projects-file "projects/lab5/projects.csv")
         (ai-models-file "projects/lab5/ai_models.csv")
         (filter-projects (select projects-file #'read-projects-table))
         (filter-ai-models (select ai-models-file #'read-ai-models-table)))
    
    (format t "~&--- Filtered Projects (AI Model ID = 2) ---~%")
    (dolist (project (funcall filter-projects :ai-model-id 2))
      (print project))

    (format t "~&~%")
    (format t "~&--- Filtered AI Models (Type = Neural Network) ---~%")
    (dolist (ai-model (funcall filter-ai-models :type "Neural Network"))
      (print ai-model))))

(defun test-write-to-csv ()
  "Тестує функції запису даних у CSV для проектів та моделей AI."
  (let* ((projects-file "projects/lab5/projects_output.csv")
         (ai-models-file "projects/lab5/ai_models_output.csv")
         (filter-projects (select projects-file #'read-projects-table))
         (filter-ai-models (select ai-models-file #'read-ai-models-table)))

    (write-projects-to-csv projects-file (funcall filter-projects :ai-model-id 2) t)
    (write-ai-models-to-csv ai-models-file (funcall filter-ai-models :type "Neural Network") t)
    
    ;; Перевірка: виводимо вміст записаних файлів
    (format t "~&--- Written Projects CSV ---~%")
    (with-open-file (stream projects-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))

    (format t "~&~%")
    (format t "~&--- Written AI Models CSV ---~%")
    (with-open-file (stream ai-models-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))))

(defun test-transform-to-hash ()
  "Тестує перетворення даних проектів та AI-моделей у геш-таблиці."
  (format t "~&---  Projects ---~%")
  (let* ((projects (select "projects/lab5/projects.csv" #'read-projects-table))
         (hashtables (convert-projects-to-hashtables (funcall projects))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht)))

  (format t "~&~%")
  (format t "~&---  AI-models ---~%")
  (let* ((ai-models (select "projects/lab5/ai_models.csv" #'read-ai-models-table))
         (hashtables (convert-ai-models-to-hashtables (funcall ai-models))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht))))

(defun test-print ()
  "Тестує фільтрацію проектів і моделей AI із використанням функції `select`."
  (let* ((projects-file "projects/lab5/projects.csv")
         (ai-models-file "projects/lab5/ai_models.csv")
         
         ;; Отримуємо лямбда-функції фільтрації для обох файлів
         (filter-projects (select projects-file #'read-projects-table))
         (filter-ai-models (select ai-models-file #'read-ai-models-table))
         
         ;; Фільтруємо дані
         (filtered-projects (funcall filter-projects :id 1))
         (filtered-ai-models (funcall filter-ai-models :name "CNN" )))
    
    ;; Вивід відфільтрованих проектів
    (format t "~&--- Filtered Projects (ID = 1) ---~%")
    (print-projects-table filtered-projects)

    ;; Вивід відфільтрованих моделей AI
    (format t "~&--- Filtered AI Models (Name = CNN) ---~%")
    (print-ai-models-table filtered-ai-models)))

;;; --- Тестування ---

(defun test-full ()
  (format t "~&1) -------> TEST-READ-FROM-TABLE~%")
  (format t "~&~%")
  (test-read-from-tabels)
  (format t "~&~%")
  (format t "~&2) -------> TEST-SELECT~%")
  (format t "~&~%")
  (test-select)
  (format t "~&~%")
  (format t "~&3) -------> TEST-WRITE-TO-CSV~%")
  (format t "~&~%")
  (test-write-to-csv)
  (format t "~&~%")
  (format t "~&4) -------> TEST-PRINT~%")
  (format t "~&~%")
  (test-print)
  (format t "~&~%")
  (format t "~&5) -------> TEST-TRANSFORM-TO-HASHTABLES~%")
  (format t "~&~%")
  (test-transform-to-hash))
```

### Тестування
```lisp
CL-USER> (test-full)

1) -------> TEST-READ-FROM-TABLE

--- Projects ---

#S(PROJECT
   :ID 1
   :NAME "AI for Healthcare"
   :AI-MODEL-ID 2
   :DESCRIPTION "System for medical diagnostics
") 
#S(PROJECT
   :ID 2
   :NAME "Autonomous Vehicles"
   :AI-MODEL-ID 3
   :DESCRIPTION "Self-driving car algorithms
") 
#S(PROJECT
   :ID 3
   :NAME "NLP Chatbot"
   :AI-MODEL-ID 4
   :DESCRIPTION "Conversational AI for customer support
") 

--- AI Models ---

#S(AI-MODEL
   :ID 1
   :NAME "CNN"
   :TYPE "Neural Network"
   :DETAILS "Convolutional Neural Network for image processing
") 
#S(AI-MODEL
   :ID 2
   :NAME "Decision Tree"
   :TYPE "Tree"
   :DETAILS "Tree-based model for classification
") 
#S(AI-MODEL
   :ID 3
   :NAME "Transformer"
   :TYPE "Neural Network"
   :DETAILS "Advanced model for language tasks
") 
#S(AI-MODEL
   :ID 4
   :NAME "Markov Chain"
   :TYPE "Probabilistic"
   :DETAILS "Statistical model for sequences
") 

2) -------> TEST-SELECT

--- Filtered Projects (AI Model ID = 2) ---

#S(PROJECT
   :ID 1
   :NAME "AI for Healthcare"
   :AI-MODEL-ID 2
   :DESCRIPTION "System for medical diagnostics
") 

--- Filtered AI Models (Type = Neural Network) ---

#S(AI-MODEL
   :ID 1
   :NAME "CNN"
   :TYPE "Neural Network"
   :DETAILS "Convolutional Neural Network for image processing
") 
#S(AI-MODEL
   :ID 3
   :NAME "Transformer"
   :TYPE "Neural Network"
   :DETAILS "Advanced model for language tasks
") 

3) -------> TEST-WRITE-TO-CSV

--- Written Projects CSV ---
ID,Name,AI Model ID,Description
1,AI for Healthcare,2,System for medical diagnostics


--- Written AI Models CSV ---
ID,Name,Type,Details
1,CNN,Neural Network,Convolutional Neural Network for image processing

3,Transformer,Neural Network,Advanced model for language tasks


4) -------> TEST-PRINT

--- Filtered Projects (ID = 1) ---

-------------------------------------------------------------------------------------------------------
| ID    | Name                    | AI Model ID  | Description                                        |
-------------------------------------------------------------------------------------------------------
| 1     | AI for Healthcare       | 2            | System for medical diagnostics
                    |
-------------------------------------------------------------------------------------------------------

--- Filtered AI Models (Name = CNN) ---

------------------------------------------------------------------------------------------------------------
| ID    | Name                 | Type                 | Details                                            |
------------------------------------------------------------------------------------------------------------
| 1     | CNN                  | Neural Network       | Convolutional Neural Network for image processing
 |
------------------------------------------------------------------------------------------------------------


5) -------> TEST-TRANSFORM-TO-HASHTABLES

---  Projects ---
ID: 1
NAME: AI for Healthcare
AI-MODEL-ID: 2
DESCRIPTION: System for medical diagnostics

ID: 2
NAME: Autonomous Vehicles
AI-MODEL-ID: 3
DESCRIPTION: Self-driving car algorithms

ID: 3
NAME: NLP Chatbot
AI-MODEL-ID: 4
DESCRIPTION: Conversational AI for customer support


---  AI-models ---
ID: 1
NAME: CNN
TYPE: Neural Network
DETAILS: Convolutional Neural Network for image processing

ID: 2
NAME: Decision Tree
TYPE: Tree
DETAILS: Tree-based model for classification

ID: 3
NAME: Transformer
TYPE: Neural Network
DETAILS: Advanced model for language tasks

ID: 4
NAME: Markov Chain
TYPE: Probabilistic
DETAILS: Statistical model for sequences

NIL
```
