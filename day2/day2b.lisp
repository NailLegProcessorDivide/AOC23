(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun is-numeric (num)
    (let ((res t))
        (progn (loop for c across num
            do (setq res (and res (>= (char-code c) 48) (<= (char-code c) 57))))
            res
        )
    )
)

(defun all-less (cols base)
    (let ((res t))
        (progn (loop for x in cols
            for y in base
            do (setq res (and res (<= x y))))
            res
        )
    )
)

(defun starts-with (pred inp)
    (string-equal pred (subseq inp 0 (min (length inp) (length pred)))))

(defun strip-col (line)
    (let ((base 0) (tmp 0) (red 0) (green 0) (blue 0) (mred 0) (mgreen 0) (mblue 0) (pos t))
        (progn
            (loop for idex from 0 to (- (length line) 1)
            do 
                (if (string-equal " " (aref line idex))
                    (progn
                        (let ((part (subseq line base idex)))
                            (progn
                                (cond
                                    ((is-numeric part) (setq tmp (parse-integer part)))
                                    ((starts-with "red" part) (setq red (+ red tmp)))
                                    ((starts-with "green" part) (setq green (+ green tmp)))
                                    ((starts-with "blue" part) (setq blue (+ blue tmp)))))
                                (if (string-equal ";" (aref line (- idex 1)))
                                    (progn
                                        (setq pos (and pos (all-less (list red green blue) (list 12 13 14))))
                                        (setq mred (max mred red))
                                        (setq mgreen (max mgreen green))
                                        (setq mblue (max mblue blue))
                                        (setq red 0)
                                        (setq green 0)
                                        (setq blue 0)
                                    )
                                )
                            )
                        (setq base (+ idex 1))
                    )
                )
            )
            (print (list red green blue))
            (* mred mgreen mblue)
        )
    )
)

(defun do-lines (filein)
    (let ((total 0))
        (progn
            (loop for i from 1 and line in filein
                do (setq total (+ total (strip-col line)))
            )
            total
        )
    )
)

(print (do-lines (get-file "input.txt")))
;(let ((file (get-file "input.txt"))))
