(in-package :cl-user)

(defpackage advent
  (:use :cl)
  (:export nil))

(in-package :advent)

;; Then, a winner for that round is selected: Rock defeats Scissors,
;; Scissors defeats Paper, and Paper defeats Rock. If both players
;; choose the same shape, the round instead ends in a draw.

(defun winner (a b)
  (let (answer)
    (when (eq a b) (setq answer 'draw))
    (when (and (not answer)
	       (equal (cons 'rock 'scissors) (cons a b))
	       (setq answer 'first)))
    (when (and (not answer)
	       (equal (cons 'scissors 'rock) (cons a b))
	       (setq answer 'second)))
    (when (and (not answer)
	       (equal (cons 'scissors 'paper) (cons a b))
	       (setq answer 'first)))
    (when (and (not answer)
	       (equal (cons 'paper 'scissors) (cons a b))
	       (setq answer 'second)))
    (when (and (not answer)
	       (equal (cons 'paper 'rock) (cons a b))
	       (setq answer 'first)))
    (when (and (not answer)
	       (equal (cons 'rock 'paper) (cons a b))
	       (setq answer 'second)))
    answer))

(defparameter encryption-1st-column '((A . rock)
				      (B . paper)
				      (C . scissors)))

(defparameter encryption-2nd-column '((X . rock)
				      (Y . paper)
				      (Z . scissors)))

(defparameter shape-score '((rock . 1)
			    (paper . 2)
			    (scissors . 3)))

(defparameter winning-score '((lost . 0)
			      (draw . 3)
			      (won . 6)))


(defun one-round (opponent me)
  (when (member opponent '(a b c))
    (setq opponent (cdr (assoc opponent encryption-1st-column))))
  (when (member me '(x y z))
    (setq me (cdr (assoc me encryption-2nd-column))))
  (print (cons opponent me))
  (let* ((winner (winner opponent me))
	 (outcome (if (eq winner 'second)
		      'won
		      (if (eq winner 'first)
			  'lost
			  (if (eq winner 'draw)
			      'draw
			      (error "no meaningful output of WINNER")))))
	 (my-score 0))
    (print outcome)
    (setq my-score (+ my-score (cdr (assoc me shape-score))))
    (setq my-score (+ my-score (cdr (assoc outcome winning-score))))
    my-score)))

(defparameter day-02
  (alexandria:read-file-into-string
   "~/omnia/git/advent-of-code-2022/day-02.input"))

(defparameter newline (string (code-char 10)))

(setq day-02 (ppcre:split newline day-02))

(setq day-02 (loop for item in day-02
		   collect (cons (read-from-string (subseq item 0 1))
				 (read-from-string (subseq item 2 3)))))

(defparameter day-02-part-1-total-score (loop for item in day-02
					      collect (one-round (car item) (cdr item))))

(setq day-02-part-1-total-score (apply #'+ DAY-02-PART-1-TOTAL-SCORE))


;; X means you need to lose,
;; Y means you need to end the round in a draw,
;; and Z means you need to win. Good luck!"

(defparameter encryption-2nd-column-again '((X . lost)
					    (Y . draw)
					    (Z . won)))

(defparameter moves '(rock paper scissors))

(defun winner-translated (opponent-move my-move)
  (let ((out (winner opponent-move my-move)))
    (if (eq out 'first)
	(setq out 'lost)
	(if (eq out 'second)
	    (setq out 'won)))
    out))

(defun make-outcome (opponent-move outcome)
  (when (member opponent-move '(a b c))
    (setq opponent-move (cdr (assoc opponent-move encryption-1st-column))))
  (loop for move in moves
	when (eq (winner-translated opponent-move move) outcome)
	  return move))

(defparameter day-02-part-2
  (loop for item in day-02
	for my-move = (make-outcome (car item)
				    (cdr (assoc (cdr item) encryption-2nd-column-again)))
	collect (one-round (car item) my-move)))

(setq day-02-part-2 (apply #'+ day-02-part-2))
