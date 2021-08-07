(defpackage tic-tac-toe
  (:use :cl))
(in-package :tic-tac-toe)

(defun display-board (board)
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
	  (format t "~A~%" (aref board x y))
	  (format t "~A | " (aref board x y)))))
  (force-output))

(defun update-board (board coords player)
  (setf
   (aref board (getf coords :x) (getf coords :y))
   player))

(defun valid-position-p (board coords)
  (eql '- (aref board (getf coords :x) (getf coords :y))))


(defun cpu-turn (board)
  (let* ((x (random (array-dimension board 0) (make-random-state t)))
	 (y (random (array-dimension board 0) (make-random-state t)))
	 (coords `(:x ,x :y ,y)))
    (if (valid-position-p board coords)
	coords
	(cpu-turn board))))

(defun player-turn (board)
  (format t "Please enter X: ")

  (let ((x (parse-integer (read-line) :junk-allowed t)))
    (unless (member x '(0 1 2))
      (player-turn board))
    
    (let ((y (parse-integer (read-line) :junk-allowed t)))
      (unless (member y '(0 1 2))
	(player-turn board))

      (let ((coords `(:x ,x :y ,y)))
	(if (valid-position-p board coords)
	    coords
	    (player-turn board ))))))

(defun game-over-p (board)
  (flet ((draw-p ()
	     (dotimes (x 3)
	       (dotimes (y 3)
		 (when (eql '- (aref board x y))
		   (return-from draw-p))))

	     t))
    (cond
      ;; Rows
      ((and (eql
	     (aref board 0 0)
	     (aref board 0 1))

	    (eql
	     (aref board 0 0)
	     (aref board 0 2))

	    (not (eql (aref board 0 0)
			'-)))
       t)
      
      ((and (eql
	     (aref board 1 0)
	     (aref board 1 1))

	    (eql
	     (aref board 1 0)
	     (aref board 1 2))

	    (not (eql (aref board 1 0)
			'-)))
       t)

      ((and (eql
	     (aref board 2 0)
	     (aref board 2 1))

	    (eql
	     (aref board 2 0)
	     (aref board 2 2))

	    (not (eql (aref board 2 0)
			'-)))
       t)
      
      ;; Columns
      ((and (eql
	     (aref board 0 0)
	     (aref board 1 1))

	    (eql
	     (aref board 0 0)
	     (aref board 2 0))

	    (not (eql (aref board 0 0)
			'-)))
       t)
      
      ((and (eql
	     (aref board 0 1)
	     (aref board 1 1))

	    (eql
	     (aref board 0 1)
	     (aref board 2 1))

	    (not (eql (aref board 0 1)
			'-)))
       t)

      ((and (eql
	     (aref board 0 2)
	     (aref board 1 2))

	    (eql
	     (aref board 0 2)
	     (aref board 2 2))

	    (not (eql (aref board 0 2)
			'-)))
       t)

      ;; Diagonals
      ((and (eql
	     (aref board 0 0)
	     (aref board 1 1))

	    (eql
	     (aref board 0 0)
	     (aref board 2 2))

	    (not (eql (aref board 0 0)
			'-)))
       t)

      ((and (eql
	     (aref board 0 2)
	     (aref board 1 1))

	    (eql
	     (aref board 0 2)
	     (aref board 2 0))

	    (not (eql (aref board 0 2)
			'-)))
       t)

      ((draw-p) t)
      
      ; Otherwise nil
      (t nil))))

(defclass player ()
  ((icon :initarg :icon :initform (error "Must provide an icon"))))

(defclass human (player)
  ())

(defclass cpu (player)
  ())

(defgeneric turn (player board)
  (:documentation "Execute a player turn"))

(defmethod turn ((player cpu) board)
  (do* ((x (random (array-dimension board 0)) (random (array-dimension board 0)))
	(y (random (array-dimension board 0)) (random (array-dimension board 0)))
	(coords `(:x ,x :y ,y) `(:x ,x :y ,y)))
       ((valid-position-p board coords)
	coords)))

(defmethod turn ((player human) board)
  (flet ((get-pos (character)
	   (format t "Please enter ~A: " character)
	   (force-output)
	   (parse-integer (read-line) :junk-allowed t)))
    (do* ((x (get-pos "X") (get-pos "X"))
	  (y (get-pos "Y") (get-pos "Y"))
	  (coords `(:x ,x :y ,y) `(:x ,x :y ,y)))
	 ((and (member x '(0 1 2)) (member y '(0 1 2)) (valid-position-p board coords))
	  coords))))


(defun game (&key (board (make-array '(3 3) :initial-element '-)))
  (let ((turn-counter (1+ (random 2 (make-random-state t)))))
    (do ()
	((game-over-p board))

      (display-board board)

      (if (evenp turn-counter)
	  (let ((coords (player-turn board)))
	    (update-board board coords "x"))

	  (let ((coords (cpu-turn board)))
	    (update-board board coords "o")))

      (incf turn-counter)))
  
  (display-board board)
  
  (format t "~&Game Over!"))

(game)  
