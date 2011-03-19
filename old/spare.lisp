(defparameter *switch* 90)
(defparameter *fresh-blood* 10)
(defun mirror-horiz (state)
  (let ((new-state (copy-seq state)))
    (setf (char new-state 0) (char state 2))
    (setf (char new-state 2) (char state 0))
    (setf (char new-state 3) (char state 5))
    (setf (char new-state 5) (char state 3))
    (setf (char new-state 6) (char state 8))
    (setf (char new-state 8) (char state 6))
    (return-from mirror-horiz new-state)))

(defun mirror-vert (state)
  (let ((new-state (copy-seq state)))
    (setf (char new-state 0) (char state 6))
    (setf (char new-state 1) (char state 7))
    (setf (char new-state 2) (char state 8))
    (setf (char new-state 6) (char state 0))
    (setf (char new-state 7) (char state 1))
    (setf (char new-state 8) (char state 2))
    (return-from mirror-vert new-state)))


  ;; Loop through all hashes and then choose either player 1/player 2 or new mutation
  (let ((choice (random 100)) (new-choice))
    (cond ((< choice 45) (setf new-choice hash1))
	  ((< choice 90) (setf new-choice hash2))
	  ((>= choice 90) (setf new-choice (random-move state choices))))))
;    (format t "~a <-> ~a or ~a or ~a = ~a (~d)~%" state hash1 hash2 choices new-choice choice)
 ;   (return-from choose-bred-hash-mixall new-choice)))

(defun choose-bred-hash-mixall (player state hash1 hash2 choices)
  ;; Loop through all hashes and then choose either player 1/player 2 or new mutation
  (let ((choice (random 100)) (new-choice))
    (cond ((< choice 45) (setf new-choice hash1))
	  ((< choice 90) (setf new-choice hash2))
	  ((>= choice 90) (setf new-choice (random-move state choices))))))
;    (format t "~a <-> ~a or ~a or ~a = ~a (~d)~%" state hash1 hash2 choices new-choice choice)
 ;   (return-from choose-bred-hash-mixall new-choice)))

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
			       (Setf choice (random 100))

			       ;; A % chance or switching between player1 and player2 for the next sequence of moves
			       (if (> choice *switch*) 
				   (cond ((= player 1) (setf player 2))
					 ((= player 2) (setf player 1))))
			       
			       ;; Set next in sequence
			       (setf (gethash k new-player-hash) (choose-bred-hash player k v (gethash k player2) (gethash k state-hash))))) player1)
    (return-from breed-players new-player-hash)))

(defun copy-hash-table (ht)
  (let ((nht (make-hash-table :size (hash-table-size ht))))
    (maphash (lambda (key value) (setf (gethash key nht) (if (listp value) (copy-list value) value))) ht) nht))

