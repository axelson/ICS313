;; Assignment 06
;; RPG Game
;; Authors: Jason Axelson, Robert Kim, Chris Ho

;;;;;;;;;;;;;;;;;;;;;;
; From Assignment 05 ;
;;;;;;;;;;;;;;;;;;;;;;

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
     (when (and (listp (car obj)) (get-prop obj 'contents))
       t))))

;; Write a recursive function (contains? obj thing) which will return t if a container or one of its contents contains the thing, otherwise returns nil.
(defun contains? (obj itemname)
  "Searches container for an object, return true if found, nil otherwise"
  (cond
    ((null obj) nil)
    ((containerp obj)
     (loop for item in (get-prop obj 'contents)
           do (cond
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
  (defparameter characters '(
			     (police ((describe "The police officer")
				      (state 0)
				      (talk nil)))
			     (married-couple ((describe "The married couple")
					      (state 0)
					      (talk nil)))
			     (fat-pompous-bastard ((describe "The fat, pompous bastard")
						   (state 0)
						   (talk nil)))
			     (young-rich-widow ((describe "The young, rich widow")
						(state 0)
						(talk nil)))
			     (butler ((describe "The butler")
				      (state 0)
				      (talk nil)))
			      ))

  (set-prop (get-prop characters 'police) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				(t (format t "I am here to ensure everyone's safety.~%"))))))

  (set-prop (get-prop characters 'married-couple) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				(t (format t "I am just worried about the safety of my family.~%"))))))

  (set-prop (get-prop characters 'fat-pompous-bastard) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				(t (format t "I just got this new suit.~%"))))))

  (set-prop (get-prop characters 'young-rich-widow) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				(t (format t "I may need some comforting.~%"))))))
  (set-prop (get-prop characters 'butler) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				(t (format t "I used to take care of Batman.~%"))))))


  (defparameter rooms '((lobby ((describe (if (= (get-state 'age) 7)
					      "A dead person hangs motionless from the roof.  The police officer stands next to the body with a stern look on his face.  The ballroom is up ahead and the elevator is behind you.  There are two doors to the left and right."
					      "A rope hangs from the roof.  It looks as if the rope was cut.  Where did the dead guy go?"))
				(west kitchen)
				(north ballroom)
				(east bathroom)
				(south elevator)
                                (contents (police))))
			(kitchen ((displayname "the kitchen")
				  (describe "A typical kitchen")
				  (east lobby)))
			(ballroom ((displayname "the ballroom")
				   (describe "A ballroom large enough to fit a hundred people.")
				   (south lobby)))
			(bathroom ((displayname "the bathroom")
				   (describe "A luxurious bathroom.  Something seems odd...")
				   (west lobby)
				   (south basement)))
			(basement ((displayname "the basement")
				   (describe "The basement.  Why was the entrance hidden?")
				   (north bathroom)))
			(elevator ((displayname "the elevator")
				   (describe "An elevator with three buttons for each level of the mansion")
				   (first lobby)
				   (second hallway2)
				   (third hallway3)))
			(hallway2 ((displayname "the second floor hallway")
				   (describe "The second floor hallway.")
				   (north hallway2north)
				   (east yourroom)
				   (west vacantroom1)
				   (south elevator)))
			(hallway2north ((displayname "the north-side of the second floor hallway")
					(describe "The north-side of the second floor hallway.  Huge paintings are on the sides of the walls.")
					(north yrwroom)
					(east fpbroom)
					(south hallway2)))
			(yrwroom ((displayname "the young rich widow's room")
				  (describe "")
				  (south hallway2north)))
			(fpbroom ((displayname "the fat pompous bastard's room")
				  (describe "")
				  (west hallway2north)))
			(yourroom ((displayname "your room")
				   (describe "Your room.")
				   (west hallway2)))
			(vacantroom1((displayname "a vacant room")
				     (description "A vacant room.")
				     (east hallway2)))
			(hallway3 ((displayname "the third floor hallway")
				   (describe "The third floor hallway.  It splits into two different directions: north and east.")
				   (north hallway3north)
				   (east mcroom)
				   (west broom)
				   (south elevator)))
			(hallway3north ((displayname "the north side of the third floor hallway")
					(describe "The north-side of the third floor hallway.")
					(east vacantroom2)
					(north storageroom)))
			(vacantroom2 ((displayname "a vacant room")
				      (describe "A vacant room.")
				      (west hallway3north)))
			(storageroom ((displayname "the storage room")
				      (describe "A large storage room.  It looks messy.")
				      (south hallway3north)
				      (north attic)))
			(mcroom ((displayname "the married couple's room")
				 (describe "")
				 (west hallway3)))
			(broom ((displayname "the butler's room")
				(describe "")
				(east hallway3)))
			(attic ((displayname "the attic")
				(describe "The mansion's attic.  The entrance was left open.")
				(south storageroom)))
			)))


;;;;;;;;;;;;;;;;;;;;;;
; End Global Objects ;
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
; Riddles ;
;;;;;;;;;;;

(defparameter riddles '(
			; First floor riddles
			(
			 ((Ice-Riddle " A man is found hanging in a room 30 feet off the ground. There is nothing else in the room except for a large puddle of water on the ground. The police can't see any way the man could have climbed the walls to get to where he is hanging.~%How did this man hang himself? ")
			  (Answer Ice)
			  (Hint "Think."))
			 ((Birthday-Riddle "What is the least number of people that need to be in a room such that there is greater than a 50% chance that at least two of the people have the same birthday?")
			  (Answer 23)
			  (Hint "What is the general formula for finding the probability that no people in the room have the same birthday?"))
			 ((Rainy-Day-Riddle "A man lives on the 44th floor of his building. On rainy days, when he gets home from work, he takes the elevator all the way up to his floor. But on sunny days, he goes up to floor 20 and walks the rest of the way. Why does he do this?")
			  (Answer Umbrella)
			  (Hint "Think."))
			 ((Quarter-Dime-Riddle "You have two normal U.S. coins that add up to 35 cents. One of the coins is not a quarter. What are the two coins?")
			  (Answer (Quarter Dime))
			  (Hint "Think.")))
			; Second floor riddles
			(
			 ((Children-Age-Riddle " A deliveryman comes to a house to drop off a package. He asks the woman who lives there how many children she has.~%\"Three,\" she says. \"And I bet you can't guess their ages.\"~%\"Ok, give me a hint,\" the deliveryman says.~%\"Well, if you multiply their ages together, you get 36,\" she says. \"And if you add their ages together, the sum is equal to our house number.\"~%The deliveryman looks at the house number nailed to the front of her house. \"I need another hint,\" he says.~%The woman thinks for a moment. \"My youngest son will have a lot to learn from his older brothers,\" she says.~%The deliveryman's eyes light up and he tells her the ages of her three children. What are their ages?")
			  (Answer (1 6 6))
			  (Hint "Think."))
			 (Second-Place-Riddle "In the final stretch of a road race, you pass the 2nd-place runner right before crossing the finish line. What place do you finish in?")
			 (Answer Second)
			 (Hint "Think."))
			; Third floor riddles
			(
			 ((Twins-Riddle "Two girls are born to the same mother, on the same day, at the same time, in the same month and year and yet they're not twins. How can this be?")
			  (Answer Triplets)
			  (Hint "Think.")))))
;;;;;;;;;;;;;;;
; End Riddles ;
;;;;;;;;;;;;;;;

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

(defun string-split (split-string string)
  "Returns a list containing items in 'string' split from occurrences of 'split-string'."
  (loop with l = (length split-string)
        for n = 0 then (+ pos l)
        for pos = (search split-string string :start2 n)
        if pos collect (subseq string n pos)
          else collect (subseq string n)
        while pos))

;;;;;;;;;;;;;;;;;
; End Functions ;
;;;;;;;;;;;;;;;;;

(defmacro char-talk (character)
  `(do-action (get-prop characters ',character) 'talk))

(defun char-talkf (character)
  (do-action (get-prop characters character) 'talk))

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

(defun describe-room (&optional (room nil))
  "Describes the contents of a room"
  (if room
      (format t "~A~%" (eval (get-prop (get-prop rooms room) 'describe)))
      (format t "~A~%" (eval (get-prop (get-current-room) 'describe)))))

(defun get-current-room ()
  (get-room (get-state 'current-room)))

(defun handle-input (input)
  "Handles user input"
  (cond
    ;; Commands
    ((find input '("win game" "win") :test #'equalp)
     (format t "You won!") t)
    ((find input '("quit" "exit" "q") :test #'equalp)
     (format t "You Fail the Game!~%") t)
    ((find input '("look" "l") :test #'equalp)
     (describe-room))
    ((search "talk" input)
     (talk (string-left-trim "talk " input)))
    ((equalp input "help")
     (format t "COMMAND LIST:~%")
     (format t "---DIRECTIONS---~%")
     (format t "1. Head North - \"N\", \"north\", \"up\"~%")
     (format t "2. Head South - \"S\", \"south\", \"down\"~%")
     (format t "3. Head East - \"E\", \"east\", \"right\"~%")
     (format t "4. Head West - \"W\", \"west\", \"left\"~%")
     (format t "~%---ACTIONS---~%")
     (format t "1. Look/Check the current room - \"look\"~%")
     (format t "2. Talk to a person in the room - \"talk\"~%"))
    ((find input '("eval") :test #'equalp)
     (format t "~A~%" (eval (read-from-string (read-line)))))
    ;; Directions
    ((find input '("N" "north" "up") :test #'equalp)
     (move 'north))
    ((find input '("S" "south" "down") :test #'equalp)
     (move 'south))
    ((find input '("E" "east" "right") :test #'equalp)
     (move 'east))
    ((find input '("W" "west" "left") :test #'equalp)
     (move 'west))
    ;; Elevator
    ((and (equalp input "1") (equalp (get-prop game-state 'current-room) 'elevator))
     (move 'first))
    ((and (equalp input "2") (equalp (get-prop game-state 'current-room) 'elevator))
     (move 'second))
    ((and (equalp input "3") (equalp (get-prop game-state 'current-room) 'elevator))
     (move 'third))
    (t
     (format t "I don't know what to do with this command: ~A~%Maybe you should try running \"help\"~%" input))))

(defun talk (character-string)
  "Try to talk to the specified character"
  (cond
    ((= 0 (length character-string))
     (format t "You talk to the wall, the wall does not talk back, perhaps you should try talking to a person~%"))
    ((contains? (get-current-room) (find-character character-string))
     (char-talkf (find-character character-string)))
    (t (format t "Sorry \"~A\" does not exist. Maybe they're only in your head?~%" character-string))))

(defun find-character (character-string)
  (cond
    ((not (stringp character-string)) (format t "this requires a string~%"))
    ((search "police" character-string) 'police)
    ((search "officer" character-string) 'police)
    ((search "fat" character-string) 'fat-pompous-bastard)
    ))

(defun move (direction)
  "Try to move to a different room"
  (if (get-prop (get-current-room) direction)
      (progn
        (set-prop game-state 'current-room (get-prop (get-current-room) direction))
        (format t "You moved ~A, you are now in the ~(~s~).~%" direction (get-prop game-state 'current-room)))
      (move-error)))


(defun move-error ()
  "Shows user error when they try to move in a restricted direction"
  (format t "There is a wall.  You cannot go in that direction.~%"))

(defun show-intro ()
  "Shows introduction when game starts"
  (format t "Welcome to Mystery Mansion! (For in-game instructions, enter \"help\").~%")
  (format t "~%Press Enter to continue...") (read-line)
  (format t "~%It is a dark and stormy night.  Although you are celebrating at a party in a large and elegant mansion, for some reason you can't shake off this feeling of uneasiness.  As the night progresses, you feel a chill run down your back as if someone has been watching you the entire time.  Guests begin to leave and you notice that your friend has left without you.  The host asks you where your friend went and you explain your situation.  The host smiles and offers a room to stay for the night.  Seeing that you have no other means of returning you gladly accept the offer.  As you enter the room , you feel extremely exhausted from all the chatter and head right to bed.")
  (format t "~%~%Press Enter to continue...") (read-line)
  (format t "~%A sharp shriek resonates throughout the hallways, startling you from your sleep.  You dash out of the room to investigate what had happened and as you walk into the lobby, you gasp in terror as you see the host, dead, hanging from the roof.  A woman, dressed in black, is on the floor trembling as if she had seen a ghost.  A couple also gasp as they enter the lobby.  You hear a snort next to you, and a somewhat large man begins ranting about how the host had it coming to him.  The butler comes in, looks at the host's dead body frantically, calls the police and rushes right back out.  Not much time passes when you hear a knock on the door and a policeman walks in.  The policeman explains that due to the heavy rain and wind, there will be no backup for awhile.  Ten minutes later the butler bursts through the lobby door and says that the cameras did not catch anyone entering or exiting the premises.  You ask the butler if there is anyone else in the mansion, and he replies that everyone here is all that is left from yesterday's party.  A cold silence.  Eyes begin searching throughout the room, as if judging who could have been the killer.  Seeing how jumping to conclusions is not a wise idea, you suggest that everyone head back to their rooms for now.~%")
  (format t "~%Press Enter to continue...") (read-line)
  )
