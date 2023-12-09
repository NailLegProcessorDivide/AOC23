(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

(defun readNumList (inp)
    (loop for num in (splitmi #\space inp) collect (parse-integer num)))

(defun head (l)
    (nth 0 l))
(defun sum (l)
    (reduce '+ l))

(defun all-zero (inp)
    (if inp
        (if (= (head inp) 0)
            (all-zero (cdr inp))
            nil)
        t))

(defun differences (inp)
    (loop for a in inp
        for b in (cdr inp)
        collect (- a b)))

(defun process-list (inp)
    (if (all-zero inp)
        0
        (+ (head inp) (process-list (differences inp)))))

(let ((parts (loop for line in (get-file "input.txt") collect (readNumList line))))
    (progn
        (print (sum (map 'list 'reverse 'process-list parts)))
        (print (sum (map 'list 'process-list parts)))))

