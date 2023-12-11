(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

(defun within (dig set)
    (cond ((null set) nil)
        ((= (nth 0 set) dig) t)
        (t (within dig (cdr set)))))

(defun readNumList (inp)
    (loop for num in (splitmi #\space inp) collect (parse-integer num)))

(defun score (time dist)
    (reduce '+ (loop for n from 0 to time
        collect (if (> (* n (- time n)) dist) 1 0))))

(defun acc-lines (lines)
    (reduce '+ (loop for line in lines collect (score line))))

(defun prepLine (line)
    (readNumList (string-trim " " (nth 1 (splitmi #\: line)))))

(let ((parts (loop for line in (get-file "input.txt") collect (prepLine line))))
    (print (reduce '* (loop for time in (nth 0 parts) for dist in (nth 1 parts)
       collect (score time dist)))))

