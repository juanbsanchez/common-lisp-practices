
(defpackage coin-toss
  (:use :cl))
(in-package :coin-toss)

;;; A simple game to practice some functions
;;; Run game in REPL and see the result. Good Luck! :)

(defun check-result (number)
  "Check number and returns heads or tails"
  
  (cond ((zerop number)
	 (format t "~&The result is heads")
	 "heads")
	(t 
	 (format t "~&The result is tails")
	 "tails")))

(defun toss-coin ()
  "Generate a random heads or tails"
  
  (let ((number (random 2 (make-random-state t))))
    (check-result number)))

(toss-coin)

(defun prompt ()
  "Get user input and loop if it is not 'heads' or 'tails'"
  
  (format t "~&Please enter heads or tails: ")
  (force-output)

  (let ((guess (string-downcase (read-line))))
    (if (or (string= guess "heads")
	 (string= guess "tails"))
	guess
	(prompt))))

(defun game ()
  "Run the actual game"
  
  (if (string= (prompt)
	       (toss-coin))
      (format t "~&You win!")
      (format t "~&You loose!")))

(game)
