; RPG Game
(defun num-props (obj)
  (/ (length (symbol-plist obj))
     2))

(defun get-prop (obj property)
  (get obj property))

(progn
  (defparameter bart "Bart")
  (setf (get 'bart 'age) 7) 
  (setf (get 'bart 'hair-color) 'yellow) 
  (setf (get 'bart 'name) "Bart Simpson"))