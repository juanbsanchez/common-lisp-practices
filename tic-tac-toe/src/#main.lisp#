(defpackage tic-tac-toe
  (:use :cl))
(in-package :tic-tac-toe)

(defun display-board (board)
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
	  (format t "~A~%" (aref board x y))
	  (format t "~A | " (aref board x y)))))
  (format t "~%"))

(defun update-board (board coords player)
  (setf
   (aref board (getf coords :x) (getf coords :y))
   player))

(defun valid-position-p (board coords)
  (equal '- (aref board (getf coords :x) (getf coords :y))))


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

(defun game (&key (board (make-array '(3 3) :initial-element '-)))
  (update-board board '(:x 0 :y 0) "x")
  (display-board board)
  (format t "~A~%" (player-turn board)))

(game)  
