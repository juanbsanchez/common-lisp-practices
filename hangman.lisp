
(defpackage hangman
  (:use :cl))
(in-package :hangman)

(defun pick-sitcom (sitcoms)
  (nth (random (length sitcoms) (make-random-state t)) sitcoms))

(defun status (scrambled-sitcom lives guessed-letters)
  (format t "~&Lives: ~A ~&Letters: ~{~A~^, ~} ~&Sitcom: ~A"
	  lives guessed-letters scrambled-sitcom))

(defun scramble-sitcom (sitcom guessed-letters)
  (flet ((letter-or-underscore (letter)
	   (if (or (member letter guessed-letters) (equal letter #\Space))
	       letter
	       #\_)))
    (coerce (mapcar #'letter-or-underscore (coerce sitcom 'list)) 'string)))

(defun game-over-p (lives scrambled-sitcom)
  (if
   (or (<= lives 0) (eq nil (position #\_ scrambled-sitcom)))
   t
   nil))

(defun get-letter (guessed-letters)
  (format t "Please enter a letter: ")
  (let ((user-input (string-downcase (read-line))))
    (cond
      ((zerop (length user-input)) (get-letter guessed-letters))
      ((member (char user-input 0) guessed-letters)
       (get-letter guessed-letters))
      (t (char user-input 0)))))

(defun game (&key (sitcom nil) (lives 10) (guessed-letters '()))
  (unless sitcom
  (let ((sitcom (pick-sitcom '(
			       "cheers"
			       "friends"
			       "frasier"
			       "the big bang theory"
			       "the it crowd"
			       "how i met your mother"))))
    (game :sitcom sitcom)))
  (let ((game-over (game-over-p lives (scramble-sitcom sitcom guessed-letters))))
    (when game-over
      (return-from game "Game Over!"))
    (format t "~&~A" (status (scramble-sitcom sitcom guessed-letters) lives guessed-letters))
    (let ((letter (get-letter guessed-letters)))
      (if (equal nil (position letter sitcom))
	  (game :sitcom sitcom :lives (1- lives) :guessed-letters (cons letter guessed-letters))
	  (game :sitcom sitcom :lives lives      :guessed-letters (cons letter guessed-letters)))))) 
