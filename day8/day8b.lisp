(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

(defun mapMe (maps position dir)
    (if (string-equal (nth 0 (nth 0 maps)) position)
        (if dir (nth 1 (nth 0 maps)) (nth 2 (nth 0 maps)))
        (mapMe (cdr maps) position dir)))

(defun done (positions)
    (if positions
        (if (char-equal (aref (nth 0 positions) 2) #\Z)
            (done (cdr positions))
            nil)
        t))

(defun takeSteps (directions maps positions)
    (let ((d 0) (n 0))
        (progn
            (loop while (not (done positions))
                do (progn
                    (setq positions (loop for position in positions collect (mapMe maps position (nth d directions))))
                    (setq d (+ d 1))
                    (setq d (if (= d (length directions)) 0 d))
                    (setq n (+ n 1))))
            n
        )))

(defun readMapping (lines)
    (loop for line in lines collect (splitmi #\= line)))
(defun readDirections (dirs)
    (loop for idex from 0 to (- (length dirs) 1) collect (char-equal (aref dirs idex) #\L)))

(defun findStarts (mapping)
    (if mapping
        (if (char-equal (print (aref (nth 0 (nth 0 mapping)) 2)) #\A)
            (cons (nth 0 (nth 0 mapping)) (findStarts (cdr mapping)))
            (findStarts (cdr mapping)))
        ()))

(let ((lines (get-file "input.txt")))
    (let ((directions (readDirections (nth 0 lines))) (mapping (readMapping (cdr lines))))
    (print (takeSteps directions mapping (print (findStarts mapping))))))

