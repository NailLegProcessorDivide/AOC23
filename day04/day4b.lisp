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

(defun scoreLine (winners mine score)
    (if mine
        (if (within (nth 0 mine) winners)
            (scoreLine winners (cdr mine) (+ score 1))
            (scoreLine winners (cdr mine) score))
        score))

(defun parseParts (line)
    (let ((winners (readNumList (string-trim " " (nth 0 line)))) (mine (readNumList(string-trim " " (nth 1 line)))))
        (scoreLine winners mine 0)))

(defun score (line)
    (parseParts (splitmi #\| (nth 1 (splitmi #\colon line)))))

(defun addCounts (counts n v)
    (if (and counts (> n 0))
        (cons (+ (nth 0 counts) v) (addCounts (cdr counts) (- n 1) v))
        counts))

(defun acc-lines (lines counts total)
    (if lines
        (acc-lines (cdr lines) (addCounts (cdr counts) (score (nth 0 lines)) (nth 0 counts)) (+ total (nth 0 counts)))
        total))

(let ((lines (get-file "input.txt")))
    (print (acc-lines lines (loop for l in lines collect 1) 0)))

