
(defun split-in-half (str)
  (declare (simple-string str))
  (let ((length (length str)))
    (cons (coerce (subseq str 0 (/ length 2)) 'list)
	  (coerce (subseq str (/ length 2))   'list))))

(defun priority (chr)
  (setq chr (char-code chr))
  (if (> chr 90)
      ;; if lowercase
      (- chr 96)
      ;; else, uppercase
      (- chr 38)))

(defun find-item-in-halves (str)
  (remove-duplicates
   (intersection (car (split-in-half str))
		 (cdr (split-in-half str)))))

(defun priority-of-item-in-halves (str)
  (priority (car (find-item-in-halves str))))

(defparameter newline (string (code-char 10)))

(defparameter day-03
  (ppcre:split newline (alexandria:read-file-into-string
			"~/omnia/git/advent-of-code-2022/day-03.input")))

(defun sum-of-priorities-from-coincident-items (list)
  (loop for item in list
	collect (priority-of-item-in-halves item) into out
	finally (return (apply #'+ out))))

;; second part

(defun find-item-in-threes (list-of-3)
  (remove-duplicates
   (intersection (intersection (coerce (nth 0 list-of-3) 'list)
			       (coerce (nth 1 list-of-3) 'list))
		 (coerce (nth 2 list-of-3) 'list))))

;; 2021-03-02, 2022-02-03
(defun slice (lst by-so-many &optional tmp)
  "(slice '(1 2 3 4 5 6 7 8 9 10) 3) → ((1 2 3) (4 5 6) (7 8 9) (10))"
  (when lst
      (if (>= (length lst) by-so-many)
          (progn (push (subseq lst 0 by-so-many) tmp)
                 (slice (subseq lst by-so-many) by-so-many tmp))
          (if (equal lst "") ;; this is to avoid having an empty last chunk
	      (reverse tmp)
	      (reverse (push lst tmp))))
      (reverse tmp)))

(defun sum-of-badges (list)
  (loop for threesome in (slice list 3)
	collect (priority (car (find-item-in-threes threesome))) into out
	finally (return (apply #'+ out))))
