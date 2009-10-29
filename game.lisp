; RPG Game
; functions: get-prop set-prop num-props
; macros: add-prop new-prop
(defun num-props (obj)
  (/ (length (symbol-plist obj))
     2))

(defmacro num-propsm (obj)
  `(/ (length (symbol-plist (quote ,obj)))
     2))

(defun get-prop (obj property)
  (get obj property))

(defmacro get-propm (obj property)
  `(get (quote ,obj) ,property))

(defun set-prop (obj property value)
  (setf (get obj property) value))

(defmacro set-propm (obj property value)
  `(setf (get (quote ,obj) ,property) ,value))

(defun prop-with-value (obj value)
  (let ((position (search (list value) (symbol-plist obj))))
    (if position
        (nth (1- position) (symbol-plist obj)))))

(progn
  (defparameter bart "Bart")
  (setf (get 'bart 'age) 7) 
  (setf (get 'bart 'hair-color) 'yellow) 
  (setf (get 'bart 'name) "Bart Simpson"))

(progn
  (defparameter pouch '((description "a small coin pouch") (contents (coins iou-note))))
  (defparameter bag '((description "a paper bag") (contents (sandwich apple soda))))
  (defparameter box `((description "small blue box") (contents (,pouch feather))))
  (defparameter backpack `((description "leather backpack") (contents (,box ,bag)))))

