(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

;537646 high

(let ((inp (splitmi #\, (nth 0 (get-file "input.txt")))))
    (print (reduce '+
        (loop for peice in inp collect
            (let ((tmp 0))
                (progn
                    (loop for i from 0 to (- (length peice) 1) do
                        (setq tmp (mod (* (+ (char-code (aref peice i)) tmp) 17) 256)))
                        tmp))))))
