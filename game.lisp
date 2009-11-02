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
  
(defun contains? (obj item)
  (cond
    ((null obj) nil)
    ((listp obj)
     (if (eql 'description (caar obj))
         (format t "It's a good list")))))


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

