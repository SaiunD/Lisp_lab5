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

(defun read-projects-table (filename)
  "Зчитує таблицю з файлу і повертає список структур PROJECT."
  (let ((projects '()))
    (with-open-file (stream filename :direction :input)
      (read-line stream)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((fields (split-string-custom line #\,))
                      (id (parse-integer (nth 0 fields))) 
                      (name (string-trim '(#\Space) (nth 1 fields)))
                      (ai-model-id (parse-integer (string-trim '(#\Space) (nth 2 fields)))) 
                      (description (string-trim '(#\Space) (nth 3 fields)))) 
                 (push (make-project :id id
                                     :name name
                                     :ai-model-id ai-model-id
                                     :description description)
                       projects))))
    (nreverse projects)))

(defun read-ai-models-table (filename)
  "Зчитує таблицю з файлу і повертає список структур AI-MODEL."
  (let ((ai-models '()))
    (with-open-file (stream filename :direction :input)
      (read-line stream)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((fields (split-string-custom line #\,))
                      (id (parse-integer (nth 0 fields))) 
                      (name (string-trim '(#\Space) (nth 1 fields))) 
                      (type (string-trim '(#\Space) (nth 2 fields))) 
                      (details (string-trim '(#\Space) (nth 3 fields))))
                 (push (make-ai-model :id id
                                      :name name
                                      :type type
                                      :details details)
                       ai-models))))
    (nreverse ai-models)))

;;; --- Функція SELECT ---

(defun select (filename filter-fn)
  "Зчитує таблицю з файлу, а потім повертає лямбда-вираз для фільтрації записів за допомогою FILTER-FN."
  (lambda ()
    (let ((records (if (search "projects.csv" filename)
                       (read-projects-table filename)
                       (read-ai-models-table filename))))
      (remove-if-not filter-fn records))))

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
  "Тестує функцію SELECT з умовами фільтрації."
  (let* ((projects-file "projects/lab5/projects.csv")
         (ai-models-file "projects/lab5/ai_models.csv")
         (filter-projects (select projects-file
                                  (lambda (project)
                                    (= (project-ai-model-id project) 2))))
         (filter-ai-models (select ai-models-file
                                   (lambda (ai-model)
                                     (string= (ai-model-type ai-model) "Neural Network")))))
    
    (format t "~&--- Filtered Projects (AI Model ID = 2) ---~%")
    (dolist (project (funcall filter-projects))
      (print project))

    (format t "~&--- Filtered AI Models (Type = Neural Network) ---~%")
    (dolist (ai-model (funcall filter-ai-models))
      (print ai-model))))


(defun test-write-to-csv ()
  "Тестує функції запису даних у CSV для проектів та моделей AI."
  (let* ((projects-file "projects/lab5/projects_output.csv")
         (ai-models-file "projects/lab5/ai_models_output.csv")
         (filter-projects (select "projects/lab5/projects.csv"
                                  (lambda (project)
                                    (= (project-ai-model-id project) 2))))
         (filter-ai-models (select "projects/lab5/ai_models.csv"
                                   (lambda (ai-model)
                                     (string= (ai-model-type ai-model) "Neural Network")))))

    (write-projects-to-csv projects-file (funcall filter-projects) t)
    (write-ai-models-to-csv ai-models-file (funcall filter-ai-models) t)
    
    ;; Перевірка: виводимо вміст записаних файлів
    (format t "~&--- Written Projects CSV ---~%")
    (with-open-file (stream projects-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))
    
    (format t "~&--- Written AI Models CSV ---~%")
    (with-open-file (stream ai-models-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~A~%" line)))))


(defun test-transform-to-hash ()
  "Тестує перетворення даних проектів та AI-моделей у геш-таблиці."
  (format t "~&---  Projects ---~%")
  (let* ((projects (select "projects/lab5/projects.csv" #'identity))
         (hashtables (convert-projects-to-hashtables (funcall projects))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht)))

  (format t "~&~%")
  (format t "~&---  AI-models ---~%")
  (let* ((ai-models (select "projects/lab5/ai_models.csv" #'identity))
         (hashtables (convert-ai-models-to-hashtables (funcall ai-models))))
    (dolist (ht hashtables)
      (maphash (lambda (key value) 
                 (format t "~a: ~a~%" key value)) 
               ht))))

(defun test-print ()
  (let* ((projects-file "projects/lab5/projects.csv")
         (ai-models-file "projects/lab5/ai_models.csv")
         (filter-projects (select projects-file
                                  (lambda (project)
                                    (> (project-id project) 1))))
         (filter-ai-models (select ai-models-file
                                   (lambda (ai-model)
                                     (string= (ai-model-name ai-model) "CNN")))))
    
    (format t "~&--- Filtered Projects (ID > 1) ---~%")
    (print-projects-table (funcall filter-projects))

    (format t "~&--- Filtered AI Models (Name = CNN) ---~%")
    (print-ai-models-table (funcall filter-ai-models))))

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
  (test-print))
