(defparameter *mutation* 95)
(defparameter *switch* 98)
(defparameter *cream* 20)
;(defparameter *fresh-blood* 10)
(defparameter *no-players* 100)
(defparameter *no-generations* 50)
(defparameter *win* 10)
(defparameter *draw* 5)
(defparameter *loss* 0)

(defun rotate (state)
  (let ((new-state (copy-seq state)))
    (setf (char new-state 0) (char state 6))
    (setf (char new-state 1) (char state 3))
    (setf (char new-state 2) (char state 0))
    (setf (char new-state 3) (char state 7))
    (setf (char new-state 4) (char state 4))
    (setf (char new-state 5) (char state 1))
    (setf (char new-state 6) (char state 8))
    (setf (char new-state 7) (char state 5))
    (setf (char new-state 8) (char state 2))
    (return-from rotate new-state)))

(defun mirror-horiz (state)
  (let ((new-state (copy-seq state)))
    (setf (char new-state 0) (char state 2))
    (setf (char new-state 1) (char state 1))
    (setf (char new-state 2) (char state 0))
    (setf (char new-state 3) (char state 5))
    (setf (char new-state 4) (char state 4))
    (setf (char new-state 5) (char state 3))
    (setf (char new-state 6) (char state 8))
    (setf (char new-state 7) (char state 7))
    (setf (char new-state 8) (char state 6))
    (return-from mirror-horiz new-state)))

(defun rotate90 (state)
  (rotate state))

(defun rotate180 (state)
  (rotate (rotate state)))

(defun rotate270 (state)
  (rotate (rotate (rotate state))))

(defun is-state-valid (state state-hash)
  (let ( (countX (count #\X state)) (countO (count #\O state)))
    (and
     (<= (abs (- countX countO)) 1)
;     (< countO countX)
     (< (+ countX countO) 9)
     (not (gethash state state-hash))
     (not (gethash (rotate90 state) state-hash))
     (not (gethash (rotate180 state) state-hash))
     (not (gethash (rotate270 state) state-hash))
     (not (gethash (mirror-horiz state) state-hash))
     (not (gethash (mirror-horiz (rotate90 state)) state-hash))
     (not (gethash (mirror-horiz (rotate180 state)) state-hash))
     (not (gethash (mirror-horiz (rotate270 state)) state-hash)))))

(defun next-player (state)
  (if (< (count #\O state) (count #\X state))
      (return-from next-player #\o)
      (return-from next-player #\x)))

(defun show-next-moves (state)
  (string-upcase (substitute #\. #\O (substitute #\. #\X (substitute (next-player state) #\. state)))))

(defun generate-states ()
  (let ((state "") (state-hash (make-hash-table :test 'equal)))
    (dotimes (x (expt 3 9))
      (setf state (substitute #\. #\0 (substitute #\X #\1 (substitute #\O #\2 (format nil "~3,9,'0,r" x)))))
      (if (is-state-valid state state-hash)
	  (progn
;	    (print state)
	    (setf (gethash state state-hash) (show-next-moves state)))))
    (format t "No. of unique tables: ~d~%" (hash-table-count state-hash))
    (return-from generate-states state-hash)))

(defun print-states (state-hash)
  (maphash #'(lambda (k v) (format t "~a : ~a~%" k v))  state-hash)
  (format t "Number of states: ~d~%" (hash-table-count state-hash)))

(defun random-move (state move-state)
  (let* ((new-state (copy-seq state)) (position) (sequence 0) (next-player (find #\. move-state :test-not #'equal)) (no-options (count next-player move-state)))
    (setf sequence (+ 1 (random no-options)))
    (setf position -1)
    (dotimes (x sequence)
      (setf position (position next-player move-state :start (+ 1 position))))
    (setf (char new-state position) next-player)
;    (format t "~a ~a ~a ~d ~d ~a ~d ~d~%" state move-state new-state sequence no-options next-player sequence position)
    (return-from random-move new-state)))

(defun make-ai-player (state-hash)
  ;; Loop through all possible states and just choose a random value for each, this player will be RUBBISH but we'll make lots of them and breed the
  ;; more successful ones
  (let ((player-hash (make-hash-table :test 'equal)))
    (maphash #'(lambda (k v) (setf (gethash k player-hash) (random-move k v))) state-hash)
    (return-from make-ai-player player-hash)))

(defun empty-game ()
  (return-from empty-game "........."))

(defun display-game (game turn)
  (format t "End of turn: ~d~%~a ~a ~a~%~a ~a ~a~%~a ~a ~a~%~%" (+ 1 turn) 
	  (char game 0) (char game 1) (char game 2) 
	  (char game 3) (char game 4) (char game 5) 
	  (char game 6) (char game 7) (char game 8)))

(defun return-next-move (game player-hash)
  ;; Find this game and see what this player is bred to do
  ;; We keep rotating till we find a match
  (let ((next-move NIL))
    ; Normal
    (setf next-move (gethash game player-hash))
    (when next-move (return-from return-next-move next-move))
    ; Rot 90
    (setf next-move (gethash (rotate90 game) player-hash))
    (when next-move (return-from return-next-move (rotate270 next-move)))
    ; Rot 180
    (setf next-move (gethash (rotate180 game) player-hash))
    (when next-move (return-from return-next-move (rotate180 next-move)))
    ; Rot 270
    (setf next-move (gethash (rotate270 game) player-hash))
    (when next-move (return-from return-next-move (rotate90 next-move)))

    ; Normal/Mirror
    (setf next-move (gethash (mirror-horiz game) player-hash))
    (when next-move (return-from return-next-move next-move))
    ; Rot 90
    (setf next-move (gethash (mirror-horiz (rotate90 game)) player-hash))
    (when next-move (return-from return-next-move (rotate270 (mirror-horiz next-move))))
    ; Rot 180
    (setf next-move (gethash (mirror-horiz (rotate180 game)) player-hash))
    (when next-move (return-from return-next-move (rotate180 (mirror-horiz next-move))))
    ; Rot 270
    (setf next-move (gethash (mirror-horiz (rotate270 game)) player-hash))
    (when next-move (return-from return-next-move (rotate90 (mirror-horiz next-move))))

    (print "No match found, something terrible has happened :(")
    (return-from return-next-move nil)))

(defun check-won (game type)
  (or
   (and (string= (char game 0) type) (string= (char game 1) type) (string= (char game 2) type))
   (and (string= (char game 3) type) (string= (char game 4) type) (string= (char game 5) type))
   (and (string= (char game 6) type) (string= (char game 7) type) (string= (char game 8) type))
   (and (string= (char game 0) type) (string= (char game 3) type) (string= (char game 6) type))
   (and (string= (char game 1) type) (string= (char game 4) type) (string= (char game 7) type))
   (and (string= (char game 2) type) (string= (char game 5) type) (string= (char game 8) type))
   (and (string= (char game 0) type) (string= (char game 4) type) (string= (char game 8) type))
   (and (string= (char game 2) type) (string= (char game 4) type) (string= (char game 6) type))))

(defun check-for-winner (game)
  (when (check-won game #\X) (return-from check-for-winner 1))
  (when (check-won game #\O) (return-from check-for-winner 2))
  (return-from check-for-winner 0))

(defun play-game (player1 player2)
  (let ((player 1) (game (empty-game)) (winner) (next-move))
    (dotimes (turn 9)

      ;; Let the player have a go
      (if (= player 1)
	  (progn 
;	    (print "It's player 1's turn")
	    (setf next-move (return-next-move game player1))
	    (setf game next-move))
	  (progn 
;	    (print "It's player 2's turn")
	    (setf next-move (return-next-move game player2))
	    (setf game next-move)))
      
      ;; Let's update the display
;      (display-game game turn)

      ;; Do we have a winner?
      (setf winner (check-for-winner game))
      (if (not (= 0 winner))
	  (progn
;	    (format t "Player ~d is the winner!~%" winner)
	    (return-from play-game winner)))

      ;; Switch players over
      (if (= player 1) (setf player 2) (setf player 1)))

    ;; Nobody won!
;    (format t "It was a draw!")
    (return-from play-game 0)))

;;; Mutation only....

(defun mutate-player (player state-hash)
  (let ((new-player-hash (make-hash-table :test 'equal)) (choice) (new-choice))
    (maphash #'(lambda (k v) (progn
			       (setf new-choice v)
			       (setf choice (random 100))
			       (if (> choice *mutation*)
				   (setf new-choice (random-move k (gethash k state-hash))))

			       ;; Set next in sequence
			       (setf (gethash k new-player-hash) new-choice))) player)
    (return-from mutate-player new-player-hash)))

(defun breed-all-players (no-players player-array player-result-array state-hash)
  (let ((new-player-array (make-array no-players :fill-pointer 0)) 
	(temp-player-array (make-array no-players :fill-pointer 0)) 
	(breed-amount (/ no-players 10)))

    ;; Lets start by picking off the best and transferring to a new array
    (dotimes (x breed-amount)
      (let ((player-score) (highest 0) (highest-index -1) (highest-value))
	(dotimes (player no-players)
	  (setf player-score (aref player-result-array player))
	  (unless (= player-score -1)
	    (when (> player-score highest)
	      (setf highest player-score)
	      (setf highest-index player)
	      (setf highest-value (aref player-array player)))))
	(setf (aref player-result-array highest-index) -1)
	(vector-push highest-value temp-player-array)))

    ;; We then make 10 copies of each of these leaders, each with a single mutation
    (dotimes (x breed-amount)
      (dotimes (y 10)
	(let ((parent (aref temp-player-array x)))
	  (vector-push (mutate-player parent state-hash) new-player-array))))

    ;; Return new players
    (return-from breed-all-players new-player-array)))

;;; Mutation and crossover

(defun choose-bred-hash-mixall (player state hash1 hash2 choices)
  ;; Loop through all hashes and then choose either player 1/player 2 or new mutation
  (let ((choice (random 100)) (new-choice))
    (cond ((< choice 49) (setf new-choice hash1))
	  ((< choice 98) (setf new-choice hash2))
	  ((>= choice 98) (setf new-choice (random-move state choices))))
;    (format t "~a <-> ~a or ~a or ~a = ~a (~d)~%" state hash1 hash2 choices new-choice choice)
    (return-from choose-bred-hash-mixall new-choice)))

(defun choose-bred-hash (player state hash1 hash2 choices)
  ;; Loop through all hashes and then choose either player 1/player 2 or new mutation
  (let ((choice (random 100)) (new-choice))

    ;; Get next state and return
    (cond ((= player 1) (setf new-choice hash1))
	  ((= player 2) (setf new-choice hash2)))

    ;; Introduce some mutation
    (if (> choice *mutation*) (setf new-choice (random-move state choices)))
;    (if (>= choice *mutation*) (format t "~a <-> ~a or ~a or ~a = ~a (~d) (~d)~%" state hash1 hash2 choices new-choice choice player))

    (return-from choose-bred-hash new-choice)))

(defun breed-players (player1 player2 state-hash)
  (let ((player 1) (new-player-hash (make-hash-table :test 'equal)) (choice))
    (maphash #'(lambda (k v) (progn
			       (setf choice (random 100))

			       ;; A % chance or switching between player1 and player2 for the next sequence of moves
			       (if (> choice *switch*) 
				   (cond ((= player 1) (setf player 2))
					 ((= player 2) (setf player 1))))
			       
			       ;; Set next in sequence
			       (setf (gethash k new-player-hash) (choose-bred-hash player k v (gethash k player2) (gethash k state-hash))))) player1)
    (return-from breed-players new-player-hash)))

(defun breed-all-players-crossover (no-players player-array player-result-array state-hash)
  (let* ((amount-to-breed (/ no-players *cream*))
	(new-player-array (make-array no-players :fill-pointer 0)) 
	(temp-player-array (make-array amount-to-breed :fill-pointer 0)))

    ;; Lets start by picking off the best and transferring to a new array
    (dotimes (x amount-to-breed)
      (let ((player-score) (highest 0) (highest-index -1) (highest-value))
	(dotimes (player no-players)
	  (setf player-score (aref player-result-array player))
	  (unless (= player-score -1)
	    (when (> player-score highest)
	      (setf highest player-score)
	      (setf highest-index player)
	      (setf highest-value (aref player-array player)))))
	(setf (aref player-result-array highest-index) -1)
	(vector-push highest-value temp-player-array)))

    ;; Now breed these top choices with each other to produce the new population
    (dotimes (x *no-players*)
      (let ((breed1 (random amount-to-breed)) (breed2 (random amount-to-breed)))
	(vector-push (breed-players (aref temp-player-array breed1) (aref temp-player-array breed2) state-hash) new-player-array)))
         
    ;; Return new players
    (return-from breed-all-players-crossover new-player-array)))

(defun display-results (generation no-players player-result-array)
  (let ((sorted-array (sort (copy-seq player-result-array) #'>)))
    (format t "Generation: ~d, range: ~d-~d~%" (+ 1 generation) (aref sorted-array (- no-players 1)) (aref sorted-array 0))))
;    (when (= (aref sorted-array 0) 50)
;   (dotimes (player no-players)
;      (format t "Player ~d points: ~d~%" (+ 1 player) (aref player-result-array player)))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun play-best-one (player)
  (let ((game (empty-game)) (next-pos) (new-game))

    ;; Do each turn!
    (dotimes (turn 9)
      (if (= (mod (+ 1 turn) 2) 1)
	  (setf game (return-next-move game player))
	  (progn
	    (setf next-pos (- (parse-integer (prompt-read "Next position?")) 1))
	    (setf new-game (copy-seq game))
	    (setf (char new-game next-pos) #\O)
	    (setf game new-game)))

      ;; Display boaed at end of turn
      (display-game game turn)

      ;; Do we have a winner?
      (when (check-won game #\O) (format t "You won, boo!~%~%") (return))
      (when (check-won game #\X) (format t "The computer won, yay!~%~%") (return)))

;    (print-states player)
    (return-from play-best-one)))

(defun brain-e()
  (let ((state-hash (generate-states)) (result) 
	(player-array (make-array *no-players* :fill-pointer 0)) 
	(player-result-array (make-array *no-players*)))
    (setf *random-state* (make-random-state t))
    
    (format t "No. of players: ~d~%Generations: ~d~%Mutation rate: ~d%~%Switch rate: ~d%~%Cream to take: ~d~%~%" 
	    *no-players* *no-generations* (- 100 *mutation*) (- 100 *switch*) (/ *no-players* *cream*))

    ;; Lets create some players!!
    (dotimes (player *no-players*)
      (vector-push (make-ai-player state-hash) player-array))

    ;; We need a tournament, so let's play them against each other, and breed the most successful
    (dotimes (generation *no-generations*)

      ;; Zero out results
      (dotimes (player *no-players*)
	(setf (aref player-result-array player) 0))

      ;; Take each player in turn
      (dotimes (player1 *no-players*)

	;; Play against all the others (including itself)
	(dotimes (player2 *no-players*)
;	  (format t "1:~d 2:~d~%" (aref player-result-array player1) (aref player-result-array player2))
	  (setf result (play-game (aref player-array player1) (aref player-array player2)))
	  (cond ((= result 0)
		 (progn
		   (setf (aref player-result-array player1) (+ *draw* (aref player-result-array player1)))
		   (setf (aref player-result-array player2) (+ *draw* (aref player-result-array player2)))))
		((= result 1)
		 (setf (aref player-result-array player1) (+ *win* (aref player-result-array player1))))
		((= result 2)
		 (setf (aref player-result-array player2) (+ *win* (aref player-result-array player2)))))))

      ;; Display the results
      (display-results generation *no-players* player-result-array)

      ;; Now we need to breed (if not final generation)
      (when (/= generation (- *no-generations* 1))
	(setf player-array (breed-all-players-crossover *no-players* player-array player-result-array state-hash))))

    ;; Lets play the best one!
    (let ((player-score) (highest 0) (highest-index -1) (highest-value))
      (dotimes (player *no-players*)
	(setf player-score (aref player-result-array player))
	(when (> player-score highest)
	  (setf highest player-score)
	  (setf highest-index player)
	  (setf highest-value (aref player-array player))))
      (do () ((play-best-one highest-value))))))
    
