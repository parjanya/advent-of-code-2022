(in-package :advent)

(defun find-between-numbers (first second)
  (declare (fixnum first)
	   (fixnum second))
  (loop for x from first to second
	collect x))

(defun list-contains-another-p (first second)
  (let ((merged (remove-duplicates
		 (sort (intersection (copy-list first)
				     (copy-list second))
		       #'<))))
    ;; shouldn’t sort be safe?
    (setq first (remove-duplicates first))
    (setq second (remove-duplicates second))
    (if (or (equalp merged first)
	    (equalp merged second))
	t
	nil)))

(defun read-file-as-line-list (file)
  (with-open-file (f file :if-does-not-exist nil)
    (when f
      (loop for line = (read-line f nil)
            while line collect line))))

(defparameter day-04
  (read-file-as-line-list "~/omnia/git/advent-of-code-2022/day-04.input"))

(defun parse-line (line)
  (let ((parsed (ppcre:split "[-,]" line)))
    (list (find-between-numbers (read-from-string (nth 0 parsed))
				(read-from-string (nth 1 parsed)))
	  (find-between-numbers (read-from-string (nth 2 parsed))
				(read-from-string (nth 3 parsed))))))

(defun day-04-part-1 ()
  (loop for line in day-04
	when (list-contains-another-p (nth 0 (parse-line line))
				      (nth 1 (parse-line line)))
	  collect 1 into out
	finally (return (apply #'+ out))))

;; part 2

(defun day-04-part-2 ()
  (loop for line in day-04
	when (intersection (nth 0 (parse-line line))
			   (nth 1 (parse-line line)))
	  collect 1 into out
	finally (return (apply #'+ out))))
