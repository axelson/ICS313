; RPG Game
; functions: get-prop set-prop num-props
; macros: add-prop new-prop
(defun num-props (obj)
  (length obj))

(defun get-prop (obj property)
  (cadr (find-if #'(lambda (x) (eql x property))
           obj
           :key #'car)))

(defun set-prop (obj property value)
  (nsubstitute-if (list property value) #'(lambda (x) (eql x property))
                 obj
                 :key #'car)
  nil)

(defun prop-with-value (obj value)
  (car (find-if #'(lambda (x) (eql x value))
               obj
               :key #'cadr)))

(defun containerp (obj)
  (cond
    ((listp obj)
     (when (listp (car obj))
       (eql 'description (caar obj))))))

(defun contains? (obj itemname)
  (cond
    ((null obj) nil)
    ((containerp obj)
     (loop for item in (cadadr obj)
        do
        ;;(format t "item: ~A~%" item)
        (cond
          ((containerp item)
           (if (contains? item itemname)
               (return t)))
          ((eql item itemname)
           (return t))))
     )))

(setf bart    
      (cons (list
             'talk #'(lambda ()
                       (format t "Don't have a cow, man!~%")))
            bart))

;; Write a function or macro (do-action obj action) which will call the property's function, and test it by making bart talk. Hint: you may want to use funcall.
(defun do-action (obj action)
  (funcall (get-prop obj action)))

(set-prop bart 'talk
          #'(lambda (obj)
              (let ((age (get-prop obj 'age)))
                (cond
                  ((< age 10) (format t "Don't have a cow, man!~%"))
                  ((<= age 18) (format t "Render unto me a fracture!~%"))
                  (t (format t "Are we having fun yet?~%"))))))

(defun do-action (obj action)
  (funcall (get-prop obj action) obj))

(setf
 bart
 (cons (list
        'update #'(lambda (obj)
                    (set-prop obj 'age
                              (1+ (get-prop obj 'age)))
                    (do-action obj 'talk)))
       bart))

(defun run-bart ()
  (loop
     initially (set-prop bart 'age 7)
     do (do-action bart 'update)
     until (= (get-prop bart 'age) 21)))

(defmacro add-prop (symbol name default-value)
  `(setf ,symbol (cons (list ,name ,default-value) ,symbol)))


; If using plists
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

