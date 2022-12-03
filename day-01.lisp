(in-package :cl-user)

(defpackage advent
  (:use :cl)
  (:export nil))

(in-package :advent)

(defparameter day-01
  (alexandria:read-file-into-string
   "~/omnia/git/advent-of-code-2022/day-01.input"))

(defparameter newline (string (code-char 10)))

(setq day-01 (ppcre:split newline day-01))

(defparameter elves
  (let ((elf-list nil)
	(current-elf nil))
    (loop for item in day-01
	  do (if (equal item "")
		 (progn (push (apply #'+ current-elf) elf-list)
			(setq current-elf nil))
		 (push (read-from-string item) current-elf)))
    elf-list))

;; this solves the first part
(let ((biggest-elf 0))
  (loop for elf from 0 to (1- (length elves))
	when (> (nth elf elves) (nth biggest-elf elves))
	  do (setq biggest-elf elf))
  (nth biggest-elf elves))


;; now the second
(defparameter elves-with-index
  (loop for elf from 0 to (length elves)
	collect (cons elf (nth elf elves))))

(defparameter three-biggest-sum
  (apply #'+ (loop for elf in (subseq (sort elves-with-index #'> :key #'cdr) 0 3)
		   collect (cdr elf))))

