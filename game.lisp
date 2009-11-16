; Assignment 06
; RPG Game

;; If using plists
;; (progn
;;   (defparameter bart "Bart")
;;   (setf (get 'bart 'age) 7) 
;;   (setf (get 'bart 'hair-color) 'yellow) 
;;   (setf (get 'bart 'name) "Bart Simpson"))

;;;;;;;;;;;;;;;;;;;;;;
; From Assignment 05 ;
;;;;;;;;;;;;;;;;;;;;;;

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


;; Write a macro (new-prop name getter setter) which will generate a get- and set- function for the property, as follows:
;; (new-prop age get-age set-age) ;; note the lack of quotes
;; ==>
;; (defun get-age (obj)
;;   (get-prop obj 'age))
;; (defun set-age (obj value)
;;   (set-age obj 'age value))
(defmacro new-prop (name getter setter)
  "Generates a getter and setter for a property"
  `(progn
    (defun ,getter (obj)
       (get-prop obj ',name))
    (defun ,setter (obj value)
       (set-prop obj ',name value))))

;;;;;;;;;;;;;;;;;;;;;
; End Assignment 05 ;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Global Objects ;
;;;;;;;;;;;;;;;;;;

(progn
  (defparameter game-state '((door1-opened t)
			     (current-room lobby)
			     (age 7)
			     (hair-color yellow)))
  (defparameter pouch '((description "a small coin pouch")
			(contents (coins marbles))))
  (defparameter player '((age 9)
                         (inventory pouch)))
  (defparameter rooms '((lobby ((describe (if (= (get-state 'age) 7)
					         "A dead person hangs motionless from the roof.  The police officer stands next to the door with a stern look on his face."
					      "Where did the dead guy go?"))
				(west kitchen)
				(north ballroom)
				(east bathroom)
				(south elevator)))
			(kitchen ((description "A typical kitchen")
				  (east lobby)))
			(ballroom ((description "A ballroom large enough to fit a hundred people.")
				   (south lobby)))
			(bathroom ((description "A luxurious bathroom.  Something seems odd...")
				   (west lobby)
				   (south basement)))
			(basement ((description "The basement.  Why was the entrance hidden?")
				   (north bathroom)))
			(elevator ((description "An elevator with three buttons for each level of the mansion")
				   (first lobby)
				   (second hallway2)
				   (third hallway3)))
			(hallway2 ((description "The second floor hallway.")
				   (north hallway2north)
				   (east yourroom)
				   (west vacantroom1)
				   (south elevator)))
			(hallway2north ((description "The north-side of the second floor hallway.  Huge paintings are on the sides of the walls.")
					(north yrwroom)
					(east fpbroom)
					(south hallway2)))
			(yrwroom ((description "")
				  (south hallway2north)))
			(fpbroom ((description "")
				  (west hallway2north)))
			(yourroom ((description "Your room.")
				   (west hallway2)))
			(vacantroom1((description "A vacant room.")
				     (east hallway2)))
			(hallway3 ((description "The third floor hallway.  It splits into two different directions: north and east.")
				   (north hallway3north)
				   (east mcroom)
				   (west broom)
				   (south elevator)))
			(hallway3north ((description "The north-side of the third floor hallway.")
					(east vacantroom2)
					(north storageroom)))
			(vacantroom2 ((description "A vacant room.")
				      (west hallway3north)))
			(storageroom ((description "A large storage room.  It looks messy.")
				      (south hallway3north)
				      (north attic)))
			(mcroom ((description "")
				 (west hallway3)))
			(broom ((description "")
				(east hallway3)))
			(attic ((description "The mansion's attic.  The entrance was left open.")
				(south storageroom)))
			)))


;;;;;;;;;;;;;;;;;;;;;;
; End Global Objects ;
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
; Functions ;
;;;;;;;;;;;;;

(defun player-has? (item)
  "Checks if player is currently holding item in their inventory"
  (contains? (symbol-value (get-prop player 'inventory)) item))

(defun get-state (prop)
  "Returns the state of the property prop"
  (get-prop game-state prop))

(defun get-room (room)
  (get-prop rooms room))

;;;;;;;;;;;;;;;;;
; End Functions ;
;;;;;;;;;;;;;;;;;

;;;;;;;;
; Game ;
;;;;;;;;

(defun game ()
  "Runs RPG game"
  (show-intro)
  ; Process following commands
  (loop
     do (format t "What now? ")
     until (handle-input (read-line))))

;; TODO make room optional
(defun describe-room (room)
  (if room
      (format t "~A~%" (eval (get-prop (get-prop rooms room) 'describe)))
      (format t "~A~%" (eval (get-prop (get-room (get-state 'current-room)) 'describe)))))

(defun handle-input (input)
  (cond ((find input '("win game" "win") :test #'equalp)
	 (format t "You won!") t)
	((find input '("quit" "exit" "q" "e") :test #'equalp)
	 (format t "You Fail the Game!~%") t)
	((find input '("N" "north" "up") :test #'equalp)
	 (move 'north))
	((find input '("S" "south" "down") :test #'equalp)
	 (move 'south))
	((find input '("E" "east" "right") :test #'equalp)
	 (move 'east))
	((find input '("W" "west" "left") :test #'equalp)
	 (move 'west))
	((equalp input "help")
	 (format t "there is no help for you here"))
	(t
	 (format t "I don't know what to do with this command: ~A~%" input)
	 (describe-room nil))))
	 

(defun move (direction)
    (case direction
      (NORTH (format t "North~%"))
      (SOUTH (format t "South~%"))
      (EAST (format t "East~%"))
      (WEST (format t "West~%"))))

(defun show-intro ()
  (format t "Welcome to Mystery Mansion! (For in-game instructions, enter \"help\").~%")
  (format t "~%Press Enter to continue...~%") (read-line)
  (format t "~%It is a dark and stormy night.  Although you are celebrating at a party in a large and elegant mansion, for some reason you can't shake off this feeling of uneasiness.  As the night progresses, you feel a chill run down your back as if someone has been watching you the entire time.  Guests begin to leave and you notice that your friend has left without you.  The host asks you where your friend went and you explain your situation.  The host smiles and offers a room to stay for the night.  Seeing that you have no other means of returning you gladly accept the offer.  As you enter the room , you feel extremely exhausted from all the chatter and head right to bed.")
  (format t "~%~%Press Enter to continue...") (read-line)
  (format t "~%A sharp shriek resonates throughout the hallways, startling you from your sleep.  You dash out of the room to investigate what had happened and as you walk into the lobby, you gasp in terror as you see the host, dead, hanging from the roof.  A woman, dressed in black, is on the floor trembling as if she had seen a ghost.  A couple also gasp as they enter the lobby.  You hear a snort next to you, and a somewhat large man begins ranting about how the host had it coming to him.  The butler comes in, looks at the host's dead body frantically, calls the police and rushes right back out.  Not much time passes when you hear a knock on the door and a policeman walks in.  The policeman explains that due to the heavy rain and wind, there will be no backup for awhile.  Ten minutes later the butler bursts through the lobby door and says that the cameras did not catch anyone entering or exiting the premises.  You ask the butler if there is anyone else in the mansion, and he replies that everyone here is all that is left from yesterday's party.  A cold silence.  Eyes begin searching throughout the room, as if judging who could have been the killer.  Seeing how jumping to conclusions is not a wise idea, you suggest that everyone head back to their rooms for now.~%")
  (format t "~%Press Enter to continue...") (read-line))
