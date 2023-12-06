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
    (loop for num in (splitmi #\space inp) collect (parse-integer (print num))))

(defun parseParts (line)
    (let ((winners (readNumList (string-trim " " (nth 0 line)))) (mine (readNumList(string-trim " " (nth 1 line)))))
        (print first)))

(defun score (line)
    (parseParts (splitmi #\| (nth 1 (splitmi #\colon line)))))

(defun acc-lines (lines)
    (reduce '+ (loop for line in lines collect (score line))))

(print (acc-lines (get-file "input.txt")))
