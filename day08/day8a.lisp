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

(defun mapMe (maps position dir)
    (if (string-equal (nth 0 (nth 0 maps)) position)
        (if dir (nth 1 (nth 0 maps)) (nth 2 (nth 0 maps)))
        (mapMe (cdr maps) position dir)))

(defun takeSteps (directions maps position)
    (let ((d 0) (n 0))
        (progn 
            (loop while (not (string-equal position "ZZZ"))
                do (progn
                    (setq position (mapMe maps position (nth d directions)))
                    (setq d (+ d 1))
                    (setq d (if (= d (length directions)) 0 d))
                    (setq n (+ n 1))))
            n
        )))

(defun readMapping (lines)
    (loop for line in lines collect (splitmi #\= line)))
(defun readDirections (dirs)
    (loop for idex from 0 to (- (length dirs) 1) collect (char-equal (aref dirs idex) #\L)))

(let ((lines (get-file "input.txt")))
    (let ((directions (readDirections (nth 0 lines))) (mapping (readMapping (cdr lines))))
    (print (takeSteps directions mapping "AAA"))))

