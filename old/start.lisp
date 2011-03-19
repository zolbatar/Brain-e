(defparameter empty ".")
(defparameter cross "X")
(defparameter nought "O")
(defparameter pos-types (list cross nought))
(defparameter *states* ())

(Defun build-state (r1c1 r1c2 r1c3 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3)
  (concatenate 'string r1c1 r1c2 r1c3 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3))

(defun build-empty ()
  (build-state empty empty empty empty empty empty empty empty empty))

(defun print-state (state)
  (format t "~a ~a ~a~%~a ~a ~a~%~a ~a ~a" 
	  (char state 0) (char state 1) (char state 2)
	  (char state 3) (char state 4) (char state 5)
	  (char state 6) (char state 7) (char state 8)))

(defun print-states ()
;;  (dolist (state *states*)
;;    (format t "~a " state))
  (format t "Number of states: ~d~%" (list-length *states*)))

(defun check-nought-won (state)
;;  (= 0 1))
  (or
   (and (string= (char state 0) nought) (string= (char state 1) nought) (string= (char state 2) nought))
   (and (string= (char state 3) nought) (string= (char state 4) nought) (string= (char state 5) nought))
   (and (string= (char state 6) nought) (string= (char state 7) nought) (string= (char state 8) nought))
   (and (string= (char state 0) nought) (string= (char state 3) nought) (string= (char state 6) nought))
   (and (string= (char state 1) nought) (string= (char state 4) nought) (string= (char state 7) nought))
   (and (string= (char state 2) nought) (string= (char state 5) nought) (string= (char state 8) nought))
   (and (string= (char state 0) nought) (string= (char state 5) nought) (string= (char state 8) nought))
   (and (string= (char state 2) nought) (string= (char state 5) nought) (string= (char state 6) nought))))

(defun build-possible-states (parent previous state depth max-positions max-depth)
  (if (<= depth max-depth)
      (progn 
	(dotimes (pos max-positions)
	  (dolist (type pos-types)
	    (let ((new-state (replace (copy-seq state) type :start1 pos)))
	      (unless (or (eql previous type) (string/= (char state pos) empty) (check-nought-won new-state))
		(when (string= type cross)
		  (push (format nil "~a:~a" parent new-state) *states*))
		(build-possible-states state type new-state (+ depth 1) max-positions max-depth)))))
	(return-from build-possible-states))
      (progn 
	(return-from build-possible-states))))

(defun test()
  (defparameter *states* ())
  (build-possible-states (build-empty) empty (build-empty) 0 9 8)
;;  (build-possible-states (build-empty) empty (build-empty) 0 9 5)
  (print-states))
