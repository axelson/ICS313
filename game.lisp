;; RPG Game
;; Author: Jason Axelson
;; ested on CMUCL version 19f-1

;; If using plists
;; (progn
;;   (defparameter bart "Bart")
;;   (setf (get 'bart 'age) 7) 
;;   (setf (get 'bart 'hair-color) 'yellow) 
;;   (setf (get 'bart 'name) "Bart Simpson"))

(defparameter bart '((name "Bart Simpson")
                     (age 7)
                     (hair-color yellow)))

(progn
  (defparameter pouch '((description "a small coin pouch") (contents (coins iou-note))))
  (defparameter bag '((description "a paper bag") (contents (sandwich apple soda))))
  (defparameter box `((description "small blue box") (contents (,pouch feather))))
  (defparameter backpack `((description "leather backpack") (contents (,box ,bag)))))


;; Define a function or macro (num-props obj) which returns the number of properties in a list. Hint, you may want to use the Common Lisp function length.
(defun num-props (obj)
  "Returns number of properties of object"
  (length obj))

;; Define a function or macro (get-prop obj property) which returns the value of a property.
(defun get-prop (obj property)
  "Gets property of object"
  (cadr (find-if #'(lambda (x) (eql x property))
           obj
           :key #'car)))


;; Define a function or macro (set-prop obj property value) which will set the value of a property.
(defun set-prop (obj property value)
  "Sets value of property on object"
  (nsubstitute-if (list property value) #'(lambda (x) (eql x property))
                 obj
                 :key #'car)
  ;; Return nil always
  nil)


;; Write a function or macro (prop-with-value obj value) which will return the property name which has the given value. Return nil if no such property exists.
(defun prop-with-value (obj value)
  "Returns property name with the given value"
  (car (find-if #'(lambda (x) (eql x value))
               obj
               :key #'cadr)))


;; Helper function for containers
(defun containerp (obj)
  "Returns true if obj is a container, nil otherwise"
  (cond
    ((listp obj)
     (when (listp (car obj))
       (eql 'description (caar obj))))))

;; Write a recursive function (contains? obj thing) which will return t if a container or one of its contents contains the thing, otherwise returns nil.
(defun contains? (obj itemname)
  "Searches container for an object, return true if found, nil otherwise"
  (cond
    ((null obj) nil)
    ((containerp obj)
     (loop for item in (cadadr obj)
        do
          (cond
            ((containerp item)
             (if (contains? item itemname)
                 (return t)))
            ((eql item itemname)
             (return t)))))))


;; We can also store functions in our lists, and use them to give objects behaviors. For example:
(setf bart    
      (cons (list
             'talk #'(lambda ()
                       (format t "Don't have a cow, man!~%")))
            bart))

;; Write a function or macro (do-action obj action) which will call the property's function, and test it by making bart talk. Hint: you may want to use funcall.
(defun do-action (obj action)
  "Calls function stored in action property on object"
  (funcall (get-prop obj action)))


;; Modify the talk function so that it takes the object itself as an argument, and make it print the following
(set-prop bart 'talk
          #'(lambda (obj)
              (let ((age (get-prop obj 'age)))
                (cond
                  ((< age 10) (format t "Don't have a cow, man!~%"))
                  ((<= age 18) (format t "Render unto me a fracture!~%"))
                  (t (format t "Are we having fun yet?~%"))))))


;; Also modify do-action accordingly
(defun do-action (obj action)
  "Calls function stored in action property on object, passing it the object itself"
  (funcall (get-prop obj action) obj))


;; Add a function property called update to bart, which will increase his age by one and make him talk.
(setf
 bart
 (cons (list
        'update #'(lambda (obj)
                    (set-prop obj 'age
                              (1+ (get-prop obj 'age)))
                    (do-action obj 'talk)))
       bart))

;; Write a function (run-bart) which will set bart's age to 7 and update bart through age 21.
(defun run-bart ()
  "Exercises some of bart's functions"
  (loop
     initially (set-prop bart 'age 7)
     do (do-action bart 'update)
     until (= (get-prop bart 'age) 21)))


;; Write a macro (add-prop symbol name default-value) which will generate the following code:
;; (add-prop bart 'appetite 'large)
;; ==>
;; (setf bart (cons (list 'appetite 'large) bart))
(defmacro add-prop (symbol name default-value)
  "Adds a new property to an object"
  `(setf ,symbol (cons (list ,name ,default-value) ,symbol)))


