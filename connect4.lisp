;;; Title:       Connect4
;;; Description: Using genetic algorithms to play connect4
;;; Author:      Daryl Dudey
;;; Date:        28th October 2010

(defconstant +blank+ #\.)
(defconstant +player1+ #\1)
(defconstant +player2+ #\2)

(defclass game-state ()
  ((board :accessor game-state-board
	  :initform "................"
	  :initarg :board)))

(defun is-game-state-valid (state-string)
  (let ( (count1 (count +player1+ state-string)) (count2 (count +player2+ state-string)))
    (and
     (<= (abs (- count1 count2)) 1)
     (< (+ count1 count2) 16)
     (< count2 count1))))

(defun create-all-game-states ()
  (let ((state-string "") (new-game-state) (game-state-hash (make-hash-table :test 'equal)))
    (dotimes (x (expt 3 16))

      ; Convert base 3 into 16 character string
      (setf state-string (substitute +blank+ #\0 (format nil "~3,16,'0,r" x)))

      ; Is that a valid board? If so, create a game-state object and add to hash
      (if (is-game-state-valid state-string)
	  (progn
	    (setf new-game-state (make-instance 'game-state :board state-string))
	    (setf (gethash state-string game-state-hash) new-game-state))))
    
    ; Return hash
    (return-from create-all-game-states game-state-hash)))

(defun print-all-game-states (game-state-hash)
  (maphash #'(lambda (k v) (format t "~a : ~a~%" k v)) game-state-hash)
  (format t "Number of states: ~d~%" (hash-table-count game-state-hash)))

(defun connect4 ()
  (let ((game-state-hash))
    
    ;; Create all game states
    (setf game-state-hash (create-all-game-states))))

 ;   (print-all-game-states game-state-hash)))

;(ASDF:OPERATE 'ASDF:LOAD-OP :LISPBUILDER-SDL)
(require 'lispbuilder-sdl)
(defun test()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:draw-surface (sdl:load-image "lisp.bmp"))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))