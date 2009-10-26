; RPG Game
(defun num-props (obj)
  (/ (length (symbol-plist obj))
     2))

(defun get-prop (obj property)
  (get obj property))

(defun set-prop (obj property value)
  (setf (get obj property) value))

(defun prop-with-value (obj value)
  (let ((position (search (list value) (symbol-plist obj))))
    (if position
        (nth (1- position) (symbol-plist obj)))))

(progn
  (defparameter bart "Bart")
  (setf (get 'bart 'age) 7) 
  (setf (get 'bart 'hair-color) 'yellow) 
  (setf (get 'bart 'name) "Bart Simpson"))