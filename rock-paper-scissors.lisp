
(defpackage rock-paper-scissors
  (:use :cl))
(in-package :rock-paper-scissors)

;;; A simple game to test some functions. Run game in REPL and enjoy! :)

(defparameter *options* '("rock" "paper" "scissors"))
(setf *score-board* '((user 0) (cpu 0)))


(defun get-player-choice (options)
  "prompts the user for an option and returns the option if it is valid"
  
  (format t "~&Please enter either -> ~{~A~^, ~}: " options)
  (force-output)
  (let ((choice (string-downcase (read-line))))
    (if (member choice options :test #'equal)
	choice
	(get-player-choice options))))

(defun get-cpu-choice ()
  "generate a random option for cpu and return the option"
  
  (nth (random (length options)
	       (make-random-state t)) *options*))

(defun check-result (player-choice cpu-choice)
  "check user choice and cpu choice and produces formatted output"
  
  (cond
    ((equal cpu-choice player-choice)
     (format t "~&Draw!"))
    ((and (equal player-choice "rock") (equal cpu-choice "scissors"))
     (format t "~&You win!") (incf (second (first *score-board*))))
    ((and (equal player-choice "paper") (equal cpu-choice "rock"))
     (format t "~&You win!") (incf (second (first *score-board*))))
    ((and (equal player-choice "scissors") (equal cpu-choice "paper"))
     (format t "~&You win!") (incf (second (first *score-board*))))
    (t (format t "~&You loose!")
       (incf (second (second *score-board*))))))

(defun reset-game ()
  "sets user and cpu score to 0"
  
  (setf *score-board* '((user 0) (cpu 0)))
  (format t "~&Game reset! ~&User: 0, CPU: 0"))

(defun print-score ()
  "print user and cpu score"
  
  (format t "~&User: ~A, CPU: ~A"
	  (second (first *score-board*))
	  (second (second *score-board*))))

(defun run-game ()
  (let ((cpu-choice (get-cpu-choice))
	(player-choice (get-player-choice *options*)))
    (format t "~&You entered: ~A, CPU entered: ~A" player-choice cpu-choice)
    (check-result player-choice cpu-choice))
  (print-score))

(run-game)
