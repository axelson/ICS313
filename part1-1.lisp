;;Sebesta, Chapter 8, 9th Ed - Statement Level Control (10 points) 
;;Programming Exercise #4, write the code in Lisp (without goto's). p. 388-389.

;; j = -3;
;; for (i = 0; i< 3; i++) {
;;   switch (j + 2) {
;;     case 3:
;;     case 2: j--; break;
;;     case 0: j += 2; break;
;;     default: j = 0;
;;   }
;;   if (j > 0) break;
;;   j = 3 -i
;; }

;; top: i=0, j=-3
;; mid: i=0, j=0
;; bot: i=0, j=3
;; top: i=1, j=3
;; mid: i=1, j=0
;; bot: i=1, j=2
;; top: i=2, j=2
;; mid: i=2, j=0
;; bot: i=2, j=1

;; Ignoring missing semicolon on second-to-last line of C program

(loop for i from 0 upto 2
   with j = -3
   do
   ;;(format t "top: i=~A j=~A~%" i j)
   (cond
     ((or (= j 0) (= j 1))
      (decf j))
     ((= j 0) (setf j (+ j 2)))
     (t (setf j 0)))
   ;;(format t "mid: i=~A j=~A~%" i j)
   (if (> j 0) return)
   (setf j (- 3 i))
   ;;(format t "bot: i=~A j=~A~%" i j)
   )