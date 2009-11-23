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
(defun get-prop (obj &rest properties)
  "Gets property of object"
  (if (listp (car properties))
      (setf properties (car properties)))
  (let ((value
	 (cadr (find-if #'(lambda (x) (eql x (car properties)))
			obj
			:key #'car))))
    (if (= (length properties) 1)
	value
	(get-prop value (cdr properties)))))

(defun flatten (list)
  (loop for i in list if (listp i) append (flatten i) else collect i))


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
(defun do-action-original (obj action)
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

;; Conversation boolean
(defparameter convo T)

(defparameter places '(place-1 place-2 place-3))
(defparameter questions '(q-1 q-2 q-3 q-4 q-5 q-6 q-7 q-8 q-9))
(defparameter answers '(a-1 a-2 a-3 a-4 a-5 a-6 a-7 a-8 a-9))
(defparameter responses '(r-1 r-2 r-3 r-4 r-5 r-6 r-7 r-8 r-9))

(defparameter riddles nil)


(progn
  (defparameter game-state '((door1-opened t)
			     (current-room lobby)
			     (age 7)
			     (hair-color yellow)))
  (defparameter pouch '((describe "a small pouch")
			(contents ())))
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
			     (poo ((describe "A rancid smell")
				   (state 1)
				   (talk nil)))
			      ))

; Interative sctructure
;; (defparameter conversation-engine
;;   '(
;;     ;Police
;;     (Police
;;      ('(a b) (lambda () (let ((mood (char-get-prop Police 'mood)))
;; 			  (cond
;; 			    ((= mood 0) (format t "I am happy."))

;; Character talk functions

  (set-prop (get-prop characters 'police) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (case state
				(0 (format t "Police Officer: \"I got here as soon as I got the call.\"~%"))
				(1 (progn (format t "I've sent everyone up to their rooms, if you find out more information, let me know.")
					  (conv-engine police 1 1)))
				(otherwise (format t "Police Officer: \"I am here to ensure everyone's safety.\"~%"))))))

  (set-prop (get-prop characters 'married-couple) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (case state
				(0 (format t "Married Couple: \"We were just about to go to bed when we heard the commotion.\"~%"))
				(1 (format t "Excuse me, should you be looking around?"))
				(otherwise (format t "Married Couple: \"I am just worried about the safety of my family.\"~%"))))))

  (set-prop (get-prop characters 'fat-pompous-bastard) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (case state
				(0 (format t "Fat Pompous Bastard: \"I did not come down here to chit chat with you.\"~%"))
				(1 (format t "Fat Pompous Bastard: \"Oh, you again.\"~%"))
				(otherwise (format t "Fat Pompous Bastard: \"I just got this new suit.\"~%"))))))

  (set-prop (get-prop characters 'young-rich-widow) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (case state
				(0 (format t "Young Rich Widow: \"Do you know anymore information about what is going on?\"~%"))
				(1 (format t "Young Rich Widow: \"Excuse me, may I help you?\"~%"))
				(otherwise (format t "Young Rich Widow: \"I may need some comforting.\"~%"))))))
  (set-prop (get-prop characters 'butler) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (case state
				(0 (format t "Butler: \"Hello sir, may I be of service to you?\"~%"))
				(otherwise (format t "Butler: \"I used to take care of Batman.\"~%"))))))

  (set-prop (get-prop characters 'poo) 'talk
	    #'(lambda (obj) (let ((state (get-prop obj 'state)))
			      (cond
				((= state 0) (format t "Poo: \"Hello!\"~%"))
				((= state 1) (access-struct riddles 'Quarter-Dime-Riddle 'riddle))
				(t (format t "Poo: \"I want a friend, please!\"~%"))))))

  (defparameter rooms '(
			(lobby ((state 0)
				(displayname "the lobby")
				(describe (cond 
					    ((= (get-prop rooms 'lobby 'state) 0)
					     "A dead person lays on the ground.  It seems as if he was stabbed numerous times.~%The police officer, young rich widow, fat pompous bastard, butler, and married couple are all in the room.~%The ballroom is up ahead and the elevator is behind you.  There are two doors to the left and right.")
					    ((= (get-prop rooms 'lobby 'state) 1) 
					      "A dead person lays on the ground.  It seems as if he was stabbed numerous times.~%The police officer stands next to the body with a stern look on his face.~%The ballroom is up ahead and the elevator is behind you.  There are two doors to the left and right.")
					    (t
					      "Where did the dead guy go?  There are no traces of his body.
The ballroom is up ahead and the elevator is behind you.  There are two doors to the left and right.")))
				(west kitchen)
				(north ballroom)
				(east bathroom)
				(south elevator)
                                (contents (police butler young-rich-widow married-couple fat-pompous-bastard))
				))
			(kitchen ((displayname "the kitchen")
				  (describe "A gorgeous kitchen with top-of-the-line kitchenware.  Doesn't look like anyone tampered with anything here.~%You notice a newspaper on the table.~%The lobby is to the right.")
				  (contents (newspaper))
				  (east lobby)))
			(ballroom ((displayname "the ballroom")
				   (describe "A ballroom large enough to fit a hundred people.  
The lobby is behind you.")
				   (south lobby)))
			(bathroom ((displayname "the bathroom")
				   (describe "A luxurious bathroom.  Something seems odd...  
The lobby is to the left.")
				   (west lobby)
				   (south basement)))
			(basement ((displayname "the basement")
				   (describe "The basement.  Why was the entrance hidden?  
The bathroom is up the stairs.")
				   (north bathroom)))
			(elevator ((locked 1)
				   (displayname "the elevator")
				   (describe "An elevator with three buttons for each level of the mansion: 1 2 3.  Which button do you press? (1, 2, or 3)")
				   (first lobby)
				   (second hallway2)
				   (third hallway3)))
			(hallway2 ((displayname "the second floor hallway")
				   (describe "The second floor hallway.~%Your room is to the right and there is another room to the left.  The hallway extends north.")
				   (north hallway2north)
				   (east yourroom)
				   (west vacantroom1)
				   (south elevator)))
			(hallway2north ((displayname "the north-side of the second floor hallway")
					(describe "The north-side of the second floor hallway.  Huge paintings are on the sides of the walls.~%There is a room to the right and a room at the end of the hall.")
					(north yrwroom)
					(east fpbroom)
					(south hallway2)))
			(yrwroom ((displayname "the young rich widow's room")
				  (describe "The young rich widow's room.~%The hallway is behind you.")
				  (south hallway2north)))
			(fpbroom ((displayname "the fat pompous bastard's room")
				  (describe "It stenches of alcohol.  Empty bottles lie on the ground throughout the room.~%The hallway is to the left.")
				  (west hallway2north)))
			(yourroom ((displayname "your room")
				   (describe "Your room.")
				   (west hallway2)))
			(vacantroom1((displayname "a vacant room")
				     (description "A vacant room.~%The hallway is to the right.")
				     (east hallway2)))
			(hallway3 ((displayname "the third floor hallway")
				   (describe "The third floor hallway.~%There are two rooms to the left and right.  The hallway extends north.")
				   (north hallway3north)
				   (east mcroom)
				   (west broom)
				   (south elevator)))
			(hallway3north ((displayname "the north side of the third floor hallway")
					(describe "The north-side of the third floor hallway.~%There is a room to the right and a room at the end of the hall.")
					(east vacantroom2)
					(north storageroom)))
			(vacantroom2 ((displayname "a vacant room")
				      (describe "A vacant room.~%The hallway is to the left.")
				      (west hallway3north)))
			(storageroom ((displayname "the storage room")
				      (describe "A large storage room.  It looks messy.~%The hallway is behind you.")
				      (south hallway3north)
				      (north attic)))
			(mcroom ((displayname "the married couple's room")
				 (describe "The married couple's room.~%The hallway is to the left.")
				 (west hallway3)))
			(broom ((displayname "the butler's room")
				(describe "The butler's room.~%The hallway is to the right.")
				(east hallway3)))
			(attic ((displayname "the attic")
				(describe "The mansion's attic.  The entrance was left open.~%The storage room back down.")
				(south storageroom)))
			)))


(defparameter items
  '(
    (newspaper ((describe (lambda () (format t "The headline says, \"Suicide or Murder?! Police stumped at death of hanging man!\"~%")
				  (format t "Read on? ")
				  (if (y-or-n-p)
                                      (try-answer-riddle 'ice-riddle))
                                  ))))
    (writing-on-wall ((describe (lambda () (format t "If life had a reset button, it would be in the ballroom.~%")))))
    (ice ((describe (lambda () (format t "This would be great for making cold drinks.~%")))))
    )
)


;;;;;;;;;;;;;;;;;;;;;;
; End Global Objects ;
;;;;;;;;;;;;;;;;;;;;;;

(defparameter police
  '(
    (place-1
     (q-1
      (lambda () (format t "Police officer: Did you find anything useful?~%~%You say:~%")))
     (a-1
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) Do you have anything helpful?~%"))
      (lambda () (format t "(3) No, nothing substantial, yet.~%")))
     (r-1
      (lambda () (convo-end))
      (lambda () (format t "Sorry, nothing new.  It's best you stay put.~%"))
      (lambda () (format t "Well, it may be best to stay in your room.  It could be dangerous wandering about.~%")))
)))

(defparameter married-couple
  '(
    (place-1
     (q-1
      (lambda () (format t "Married couple: Oh, it's you.  What gives you the right to wander around?~%~%You say:~%")))
     (a-1
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) I am sorry to bother you, but I was looking for some answers.~%"))
      (lambda () (format t "(3) Let me in, please.~%")))
     (r-1
      (lambda () (convo-end))
      (lambda () (format t "Married couple: So are we.~%") (conv-engine married-couple 1 2))
      (lambda () (format t "Excuse me? I do not think it would be wise of us to let you in.  Our first concern is our safety.  Please leave.~%")))
     (q-2
      (lambda () (format t "Married couple: But our first and utmost concern is on keeping our family safe.~%~%You say:~%")))
     (a-2
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) Do you think there is any way I can help?~%"))
      (lambda () (format t "(3) I think if I had a look around I could dig up something.~%")))
     (r-2
      (lambda () (convo-end))
      (lambda () (format t "Married couple: I think you should leave that to the police officer, it is his job after all.~%"))
      (lambda () (format t "Married couple: Let you in?!~%") (conv-engine married-couple 1 3)))
     (q-3
      (lambda () (format t "Married couple: How does that go along with us trying to keep safe?~%~%You say:~%")))
     (a-3
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) It will if I find out who is behind this!  I need to search your room.~%"))
      (lambda () (format t "(3) You have to trust me.~%")))
     (r-3
      (lambda () (convo-end))
      (lambda () (conv-engine married-couple 1 4))
      (lambda () (format t "I think you should stay put and keep yourself safe.. besides, the killer is out there.~%")))
     (q-4
      (lambda () (format t "Married couple: Before we let you in, you have to show that you have concern for our family.~%~%You say:~%")))
     (a-4
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) Please, continue, anything you wish.~%"))
      (lambda () (format t "(3) Okay, only if it is a riddle!~%")))
     (r-4
      (lambda () (convo-end))
      (lambda () (format t "Married couple: Wow.. you seem a little too eager. Sorry, we do not want to jeopardize our safety!~%"))
      (lambda () (format t "Married couple: Ooh!  We love riddles!  We have just the one.~%") (format t "Riddle here.")))
      
)))

;(defparameter fat-pompous-bastard)

;(defparameter young-rich-widow)

;(defparameter butler)

(defparameter poo
  '(
    (place-1
     (q-1
      (lambda () (format t "Poo: Hello kind sir, would you like to produce some company for me?~%~%You say:~%")))
     (a-1
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) Oh! A talking shit!~%"))
      (lambda () (format t "(3) I may not be able to produce a friend for you, but will my company do?~%")))
     (r-1
      (lambda () (convo-end))
      (lambda () (format t "The poo looks insulted.~%Although you are distracted by the smell and the fact that it has no face, you notice it crinkle its brow.~%Obligingly, the poo continues...~%~%") (conv-engine poo 1 2))
      (lambda () (conv-engine poo 1 3)))
     (q-2
      (lambda () (format t "Do you believe the presence of power yields the power of persuasion?~%~%You say:~%")))
     (a-2
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) What nonsense!~%"))
      (lambda () (format t "(3) I am not too sure what that means.. please tell me more.~%")))
     (r-2
      (lambda () (convo-end))
      (lambda () (format t "Poo: Fine, you suck.~%"))
      (lambda () (format t "Poo: So you are curious...~%") (conv-engine poo 1 4)))
     (q-3
      (lambda () (format t "How kind. Please sit down.~%~%You say:~%")))
     (a-3
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) Sure thing, poop!~%"))
      (lambda () (format t "(3) No thanks, I have more important things to do.~%")))
     (r-3
      (lambda () (convo-end))
      (lambda () (format t "You sit down on the cold porcelain throne making sure to leave enough light for the poo.~%") (conv-engine poo 1 4))
      (lambda () (format t "Well at least I tried.~%")))
     (q-4
      (lambda () (format t "Poo: Well I have a nice little riddle for you.~%Considering I have no brain, I have been baffled ever since my father made me here and left me with the question he was mumbling to himself as he walked out.~%~%You say:~%")))
     (a-4
      (lambda () (format t "(1) **Leave**~%"))
      (lambda () (format t "(2) I apologize poo, I have realize a sad truth in all this.~%Talking to a piece of poo jeopardizes my sanity.~%I better leave quickly and carry on with the investigation!~%"))
      (lambda () (format t "(3) Sure, poo.~%It will be interesting to see where this leads.~%")))
     (r-4
      (lambda () (convo-end))
      (lambda () (format t "Poo: Well thank you for your patience.  Good luck on your endeavors.~%"))
      (lambda () (format t "Riddle goes here."))))))

(defun convo-end ()
    (format t "The conversation ends.~%"))

(defun end-convo ()
  (setf convo nil))

(defun reset-convo ()
  (setf convo nil))

; e.g., (run-convo police (char-get-prop police 'state))

(defun run-convo (convo-char place-no)
  (let ((question-no 1) (input 0))
    ;(setq place-no (char-get-prop convo-char 'state))
    ; First iteration
    (access-convo-all convo-char (get-from-list places place-no) (get-from-list questions question-no))
    (access-convo-all convo-char (get-from-list places place-no) (get-from-list answers question-no))
    ; Start conversation
    ; The first option always ends the conversation
    (loop while (> (setf input (parse-input (read-line))) 1) do
	 (access-convo-resp convo-char (get-from-list places place-no) (get-from-list responses question-no) input)
	 (if convo
	     (progn
	       ; Update counters
	       (setf question-no (1+ question-no))
	       (access-convo-all convo-char (get-from-list places place-no) (get-from-list questions question-no))
	       (access-convo-all convo-char (get-from-list places place-no) (get-from-list answers question-no)))
	     (return-from run-convo)))
    (format t "~%The conversation ends.~%") (reset-convo)))

(defun conv-engine (character place-no &optional (question-no 1))
  ; Execute question
  (access-convo-all character (get-from-list places place-no) (get-from-list questions question-no))
  ; Execute corresponding set of answers
  (access-convo-all character (get-from-list places place-no) (get-from-list answers question-no))
  ; Execute corresponding response
  (access-convo-resp character (get-from-list places place-no) (get-from-list responses question-no) (parse-input (read-line))))


(defun parse-input (input)
  (loop
     (case (if (string= input "")
               nil
               (progn (format t "input:[~A]" input) (parse-integer input)))
       (1 (return 1))
       (2 (return 2))
       (3 (return 3))
       (otherwise (progn (format t "~%Please choose a valid option.~%") (setf input (read-line)))))))

(defun get-from-list (lname item-no)
  (let ((no 1))
    (loop for i in lname do
	 (if (eq no item-no) (return i) ())
	 (incf no))))

(defun ssc (arg1 arg2)
  (if (stringp arg1) () (setq arg1 (write-to-string arg1)))
  (if (stringp arg2) () (setq arg2 (write-to-string arg2)))
  ; concatenate
  (return-from ssc (concatenate 'string arg1 arg2)))


(defmacro run-convo-helper (character place question-no)
  `(access-convo-all ,character ',place ',question-no))
     

(defun access-convo-all (character dialog-name item-list)
  (loop for i in character do
       (if (eq (car i) dialog-name)
	   (loop for j in (cdr i) do
		(if (eq (car j) item-list) (exec-list (cdr j)) ())) ())))

(defun access-convo-resp (character dialog-name item-list item-no)
  (loop for i in character do
       (if (eq (car i) dialog-name)
	   (loop for j in (cdr i) do
		(if (eq (car j) item-list) (exec-list-specific (cdr j) item-no) ())) ())))

(defun exec-list (inlist)
  (loop for i in inlist do
       (funcall (eval i))))

(defun exec-list-specific (inlist number)
  (let ((no 1))
    (loop for i in inlist do
	 (progn
	   (if (eq number no)
	       (return-from exec-list-specific (funcall (eval i))) ())
	   (setf no (1+ no))))))



;;;;;;;;;;;
; Riddles ;
;;;;;;;;;;;

(defparameter riddles '(
			(Test-Riddle
			 (Riddle (lambda ()
				     (format t "Test riddle")))
			 (Answer (lambda ()
				   '(a test list)))
			 (Hint (lambda ()
				   (format t "A hint string"))))
			; First floor riddles
			(Ice-Riddle
			 (Riddle (lambda () (format t "\"A man was found hanging in a room 30 feet off the ground. There wass nothing else in the room except for a large puddle of water on the ground. At this point, investigators can't see any way the man could have climbed the walls to get to where he is hanging without it being a murder, but there are no signs of resistance.\"~%~%You think about the riddle for awhile and realize that it had to be suicide!  But how did the victim do it?~%Your answer: ")))
			 (Answer (lambda () "Ice"))
			 (Hint (lambda () (format t "Think.")))
                         (Result (lambda ()
                                   (format t "I got it!  He stood on ice with a rope around his neck and waited for the ice to melt!~%As you thought this, ice appeared in your pouch.~%")
                                   (set-prop pouch 'contents '(ice)))))
			(Birthday-Riddle
			 (Riddle (lambda () (format t "What is the least number of people that need to be in a room such that there is greater than a 50% chance that at least two of the people have the same birthday?")))
			 (Answer (lambda () '(23)))
			 (Hint (lambda () (format t "What is the general formula for finding the probability that no people in the room have the same birthday?"))))
			(Rainy-Day-Riddle
			 (Riddle (lambda () (format t "A man lives on the 44th floor of his building. On rainy days, when he gets home from work, he takes the elevator all the way up to his floor. But on sunny days, he goes up to floor 20 and walks the rest of the way. Why does he do this?")))
			 (Answer (lambda () '(Umbrella)))
			 (Hint (lambda () (format t "Think."))))
			(Quarter-Dime-Riddle
			 (Riddle (lambda () (format t "You have two normal U.S. coins that add up to 35 cents. One of the coins is not a quarter. What are the two coins?")))
			 (Answer (lambda () '(Quarter Dime)))
			 (Hint (lambda () (format t "Think."))))
			; Second floor riddles
			(Children-Age-Riddle
			 (Riddle (lambda () (format t "A deliveryman comes to a house to drop off a package. He asks the woman who lives there how many children she has.~%\"Three,\" she says. \"And I bet you can't guess their ages.\"~%\"Ok, give me a hint,\" the deliveryman says.~%\"Well, if you multiply their ages together, you get 36,\" she says. \"And if you add their ages together, the sum is equal to our house number.\"~%The deliveryman looks at the house number nailed to the front of her house. \"I need another hint,\" he says.~%The woman thinks for a moment. \"My youngest son will have a lot to learn from his older brothers,\" she says.~%The deliveryman's eyes light up and he tells her the ages of her three children. What are their ages?")))
			 (Answer (lambda () '(1 6 6)))
			 (Hint (lambda () (format t "Think."))))
			(Second-Place-Riddle
			 (Riddle (lambda () (format t "In the final stretch of a road race, you pass the 2nd-place runner right before crossing the finish line. What place do you finish in?")))
			 (Answer (lambda () '(Second)))
			 (Hint (lambda () (format t "Think."))))
			; Third floor riddles
			(Twins-Riddle
			 (Riddle (lambda () (format t "Two girls are born to the same mother, on the same day, at the same time, in the same month and year and yet they're not twins. How can this be?")))
			 (Answer (lambda () '(Triplets)))
			 (Hint (lambda () (format t "Think."))))))

;; Riddle accessor function
;; e.g., (access-riddle riddles 'twins-riddle 'hint)
;;
;;(defmacro access-riddle (list-name riddle-name item)
;;  `(loop for i in ,list-name do
;;	(if (eq (car i) ,riddle-name)
;;	    (loop for j in (cdr i) do
;;		 (if (eq (car j) ,item) (format t (cadr j)) ())) ())))
;;
;;(defmacro access-struct (struct-name group-name item)
;; `(loop for i in ,struct-name do
;;	(if (eq (car i) ,group-name)
;;	    (loop for j in (cdr i) do
;;		 (if (eq (car j) ,item) (progn (funcall (cadr j)) (return-from access-struct)) ())) ())))

(defun access-struct (struct-name group-name item)
  (loop for i in struct-name do
       (if (eq (car i) group-name)
	   (loop for j in (cdr i) do
		(if (eq (car j) item) (return-from access-struct (funcall (eval (cadr j)))) ())) ())))

;;(defun answer-test-riddle ()
;;  (return-from answer-test-riddle 1))

(defun split-line (line)
  (let ((start 0) (line (concatenate 'string line " ")) (parsed '()))
    (loop for i from 0 to (1- (length line)) do
	 (if (equalp #\Space (char line i))
	     (progn (setf parsed (append parsed (list (subseq line start i)))) (setf start (1+ i))) ()))
    (return-from split-line parsed)))

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

(defun search-string (key-list search-string)
  "Searches search-string for words matching string-list and returns number of matches"
  (let ((matches 0))
    (loop for key in (string-split " " key-list)
       do (loop for string in (string-split " " search-string)
	     do (when (equalp key string)
		  (incf matches)
		  (return))))
    (if (= 0 matches)
        nil
	matches)))

;; Riddle functions
(defun try-answer-riddle (riddle)
  "User can try to answer the riddle"
  ;; Show riddle
  (access-struct riddles riddle 'riddle)
  (if (search-string (access-struct riddles riddle 'answer)
                     (read-line))
      ;; Execute result of getting riddle correct
      (access-struct riddles riddle 'result)
      ;; Answer was incorrect
      (format t "Hm.. I don't think that could've been possible.  I guess I'll give up for now.~%")))

;;;;;;;;;;;;;;;;;
; End Functions ;
;;;;;;;;;;;;;;;;;

(defmacro char-talk (character)
  `(do-action (get-prop characters ',character) 'talk))

(defun char-talkf (character)
  (do-action (get-prop characters character) 'talk))

(defmacro char-get-prop (character property)
  `(get-prop characters ',character ,property))

(defmacro char-set-prop (character property value)
  `(set-prop (get-prop characters ,character) ,property ,value))

(defmacro char-set-state (character new-state)
  `(char-set-prop ',character 'state ,new-state))

(defmacro char-incr-state (character)
  `(char-set-state ,character (1+ (char-get-prop ,character 'state)))) 

(defmacro char-decr-state (character)
  `(char-set-state ,character (1- (char-get-prop ,character 'state))))

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
      (format t (eval (get-prop (if room
                                    (get-room room)
                                    (get-current-room))
                                'describe)))
      (format t "~%~%"))

(defun get-current-room (&optional (property nil))
  "Gets the the current room object, with an optional property"
  (if property
      (get-prop (get-current-room) property)
      (get-room (get-state 'current-room))))

(defun handle-input (input)
  "Handles user input"
  (cond
    ;; Commands
    ((search-string "win" input)
     (format t "You won!")
     (reset-state) t)
    ((search-string "quit exit q" input)
     (format t "You Fail the Game!~%")
     (reset-state) t)
    ((search-string "inventory i" input)
     (check-inventory))
    ((search-string "examine" input)
     (examine input))
    ((search-string "look l" input)
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
     (format t "2. Initiate a conversation with a character in the room - \"talk\" + character description (i.e. \"talk to young widow\")~%")
     (format t "3. Examine an item/object in the room - \"examine\" + item description (i.e. \"examine newspaper\")~%")
     (format t "4. Look at what is in your inventory - \"inventory\"~%"))
    ((find input '("eval") :test #'equalp)
     (format t "~A~%" (eval (read-from-string (read-line)))))
    ;; Directions
    ((search-string "n north up" input)
     (move 'north))
    ((search-string "s south down" input)
     (move 'south))
    ((search-string "e east right" input)
     (move 'east))
    ((search-string "w west left" input)
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

(defun check-inventory ()
  "Shows user what is contained in their inventory"
  (if (get-prop pouch 'contents)
      (progn
	(format t "You have ~(~A~) in your inventory.~%"  (get-prop pouch 'contents)))
      (format t "There is nothing in your inventory.~%")))

(defun talk (character-string)
  "Try to talk to the specified character"
  (cond
    ((= 0 (length character-string))
     (format t "You talk to the wall, the wall does not talk back, perhaps you should try talking to a person~%"))
    ((find-character character-string)  ;Trying to talk to a character
     (if (contains? (get-current-room) (find-character character-string))
         (char-talkf (find-character character-string)) ;Talk to character
         (format t "Sorry, \"~A\" cannot hear through walls~%" character-string)))
    (t (format t "Your imaginary friend ~A responds!~%" character-string))))

(defun examine (item-string)
  "Describes the item that is in a room"
  (let ((item (find-item item-string)))
    (cond
      ((= 0 (length item-string))
       (format t "You examine nothing, and look stupid doing it.~%"))
      ((contains? (get-current-room) item)
       (funcall (eval (get-prop items item 'describe))))
      ((player-has? item)
       (format t "You rummage about your pouch and see an item~%")
       (funcall (eval (get-prop items item 'describe))))
      (t (format t "Sorry, there is nothing special to examine about that.~%")))))

(defun find-character (character-string)
  "Matches user input with characters" 
  (cond
    ((not (stringp character-string)) (format t "this requires a string~%"))
    ((search-string "police officer" character-string) 'police)
    ((search-string "fat pompous bastard" character-string) 'fat-pompous-bastard)
    ((search-string "young rich widow" character-string) 'young-rich-widow)
    ((search-string "married couple" character-string) 'married-couple)
    ((search-string "butler" character-string) 'butler)
    (t nil)
    ))

(defun find-item (item-string)
  "Matches user input with items" 
  (cond
    ((not (stringp item-string)) (format t "this requires a string~%"))
    ((search-string "newspaper" item-string) 'newspaper)
    ((search-string "writing wall" item-string) 'writing-on-wall)
    ((search-string "ice" item-string) 'ice)
    (t nil)
    ))

(defun move (direction)
  "Try to move to a different room"
  (if (get-prop (get-current-room) direction)
      (progn
	(cond
	  ((equalp (get-prop (get-current-room) 'state) 0)
	   (format t "Are you sure you want to leave the lobby? (y/n) ")
	   (if (y-or-n-p)
	       (progn
		 (show-intro2)
		 (set-prop (get-room 'lobby) 'state 1)
		 (set-prop (get-room 'lobby) 'contents '(police))
		 )
	       )
	   )
	  ((and (equalp (get-prop (get-current-room) direction) 'elevator) (= (get-prop (get-room 'elevator) 'locked) 1))
	   (format t "It appears the elevator is locked...~%"))
	  ((equalp (get-prop game-state 'current-room) 'elevator)
	   (set-prop game-state 'current-room (get-prop (get-current-room) direction))
	   (format t "You moved to the ~A FLOOR, you are now in ~A.~%" direction (eval (get-prop (get-current-room) 'displayname)))
	   )
	  (t   
	   (set-prop game-state 'current-room (get-prop (get-current-room) direction))
	   (format t "You moved ~A, you are now in ~A.~%" direction (eval (get-prop (get-current-room) 'displayname))))
	  )
	)
      (move-error)))


(defun move-error ()
  "Shows user error when they try to move in a restricted direction"
  (format t "There is a wall.  You cannot go in that direction.~%"))

(defun show-intro ()
  "Shows introduction when game starts"
  (format t "Welcome to Mystery Mansion!~%(To progress through the dialogue, press ENTER whenever you see '...'.  To skip the dialogue, type \"skip\")~%")
  (if (enter-to-continue) (return-from show-intro))
  (format t "~%It is a dark and stormy night.  Although you are celebrating at a party in a large and elegant mansion, for some reason you can't shake off this feeling of uneasiness.  As the night progresses, you feel a chill run down your back as if someone has been watching you the entire time.  Guests begin to leave and you notice that your friend has left without you.  The host asks you where your friend went and you explain your situation.  The host smiles and offers a room to stay for the night.  Seeing that you have no other means of returning you gladly accept the offer.  As you enter the room , you feel extremely exhausted from all the chatter and head right to bed.~%")
  (if (enter-to-continue) (return-from show-intro))
  (format t "~%A sharp shriek resonates throughout the hallways, startling you from your sleep.  You dash out of the room to investigate what had happened and as you walk into the lobby you gasp in terror as you see the host, dead on the floor.  A woman, dressed in black, is on the floor trembling as if she had seen a ghost.  A couple also gasp as they enter the lobby.  You hear a snort next to you, and a somewhat fat, pompous bastard begins ranting about how the host had it coming to him.  The butler comes in, looks at the host's dead body frantically, calls the police and rushes right back out.  Not much time passes when you hear a knock on the door and a policeman walks in.  The policeman explains that due to the heavy rain and wind, there will be no backup for awhile.  Ten minutes later the butler bursts through the lobby door and says that the cameras did not catch anyone entering or exiting the premises during the night.  You ask the butler if there is anyone else in the mansion, and he replies that everyone here is all that is left from yesterday's party.  A cold silence.  Eyes begin searching throughout the room, as if judging who could have been the killer.~%")
  (if (enter-to-continue) (return-from show-intro))
  )

(defun show-intro2 ()
  "Shows the second part of the introduction when you try to leave the lobby"
  (format t "~%You try to exit the lobby when the young rich widow screams, \"Where do you think you're going?!  Trying to hide the evidence?!  HE'S THE KILLER!!\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "Startled, you retort, \"I'm no killer!  And nothing's going to get accomplished just standing here!  I'm gonna get to the bottom of this!\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"Hold on guys, calm down.\" says the police officer.  \"I got some bad news.  I just called for backup and it seems that due to the storm, they won't be able to send anyone up until next morning...\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"Wait a minute.. Are you saying that we have to sleep here overnight with the killer still in this house?!\" yelled the wife of the married couple.  \"I'm sorry but that's not possible!  I'm leaving now!\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "As the wife heads towards the elevator, the officer walks right in front of her path.  \"I'm sorry ma'am.  But I'm afraid I cannot let you go.\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"AND WHY IS THAT?!\" demanded the wife.~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"Seeing how bad the storm has gotten, I'm afraid I can't let you go.  Also, I would like for everyone to remain in this house to gather eye-witnesses or clues for this murder.\"~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"So are you telling me to stay here and DIE?!\" screamed the wife.~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "\"No.  But I do say that the probably of that happening is higher if you left this mansion now, ma'am.  With the killer is still on the loose, it's more likely that he's waiting for us to panic and leave the house.  I think it's better if we all go back into our rooms, and if anything happens, I'll be here.\" calmly said the officer.~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "With that, everyone somewhat agreed to return to their rooms and meet in the lobby the next morning.~%")
  (if (enter-to-continue) (return-from show-intro2))
  (format t "You, on the other hand, couldn't shake this feeling that the killer was someone in the room.  You had to find out who did it and why.~%")
  (if (enter-to-continue) (return-from show-intro2))
)

(defun reset-state()
  (set-prop (get-room 'lobby) 'state 0)
  (set-prop (get-room 'lobby) 'contents '(police butler married-couple fat-pompous-bastard young-rich-widow))
  )

(defun enter-to-continue ()
  (format t "...")
  (if (search-string "skip s" (read-line))
      t
      nil))
