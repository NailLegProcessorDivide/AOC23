(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun access (file x y)
    (aref (nth y file) x))

(defun isSym (c)
    (and (not (digit-char-p c)) (not (string-equal "." c))))

(defun detectSymbol (file x y)
    (or (and (> x   0)           (isSym (access file (- x 1)       y)))
        (and (> x   0) (> y   0) (isSym (access file (- x 1) (- y 1))))
        (and (> x   0) (< y 139) (isSym (access file (- x 1) (+ y 1))))
        (and           (> y   0) (isSym (access file       x (- y 1))))
        (and           (< y 139) (isSym (access file       x (+ y 1))))
        (and (< x 139)           (isSym (access file (+ x 1)       y)))
        (and (< x 139) (> y   0) (isSym (access file (+ x 1) (- y 1))))
        (and (< x 139) (< y 139) (isSym (access file (+ x 1) (+ y 1))))
    ))

(defun iterateFun (file x y value valid)
    (if (and (< x 140) (digit-char-p (access file x y) 10))
        (iterateFun file
                    (+ x 1)
                    y
                    (+ (* 10 value) (parse-integer (string (access file x y))))
                    (or valid (detectSymbol file x y)))
        (if valid value 0)))

(defun detectNum (file x y)
    (if (or (= x 0) (not (digit-char-p (access file (- x 1) y) 10)))
        (iterateFun file x y 0 nil)
        0))

(defun iterateMatrix (file)
    (reduce '+ 
        (loop for x from 0 to 139
            collect (reduce '+
                (loop for y from 0 to 139
                    collect (detectNum file x y))))))

(let ((lines (get-file "input2.txt")))
    (print (iterateMatrix lines)))
