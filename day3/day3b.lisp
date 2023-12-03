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

(defun iterateFun (file x y value)
    (if (and (< x 140) (digit-char-p (access file x y) 10))
        (iterateFun file
                    (+ x 1)
                    y
                    (+ (* 10 value) (parse-integer (string (access file x y)))))
        value))

(defun getHead (file x y)
    (if (and (> x 0) (digit-char-p (access file (- x 1) y)))
        (getHead file (- x 1) y)
        (iterateFun file x y 0)
    )
)

(defun twoNay (file x y)
    (let ((nums 0) (prod 1))
        (progn
            (if (or (and (> x   0) (> y   0) (digit-char-p (access file (- x 1) (- y 1)))
                                (> y   0) (digit-char-p (access file       x (- y 1))))
                    (and (< x 139) (> y   0) (digit-char-p (access file (+ x 1) (- y 1)))
                                (> y   0) (digit-char-p (access file       x (- y 1)))))

                (progn (setq nums (+ nums 1))
                    (setq prod (* prod (getHead file x (- y 1)))))

                (progn
                    (if (and (> x   0) (> y   0) (digit-char-p (access file (- x 1) (- y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file (- x 1) (- y 1))))))
                    (if (and           (> y   0) (digit-char-p (access file       x (- y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file x (- y 1))))))
                    (if (and (< x 139) (> y   0) (digit-char-p (access file (+ x 1) (- y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file (+ x 1) (- y 1))))))
                )
            )

            (if (and (> x   0) (digit-char-p (access file (- x 1) y)))
                (progn (setq nums (+ nums 1))
                    (setq prod (* prod (getHead file (- x 1) y)))))
            (if (and (< x 139) (digit-char-p (access file (+ x 1) y)))
                (progn (setq nums (+ nums 1))
                    (setq prod (* prod (getHead file (+ x 1) y)))))

            (if (or (and (> x   0) (< y 139) (digit-char-p (access file (- x 1) (+ y 1)))
                                (< y 139) (digit-char-p (access file       x (+ y 1))))
                    (and (< x 139) (< y 139) (digit-char-p (access file (+ x 1) (+ y 1)))
                                (< y 139) (digit-char-p (access file       x (+ y 1)))))

                (progn (setq nums (+ nums 1))
                    (setq prod (* prod (getHead file x (+ y 1)))))

                (progn
                    (if (and (> x   0) (< y 139) (digit-char-p (access file (- x 1) (+ y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file (- x 1) (+ y 1))))))
                    (if (and           (< y 139) (digit-char-p (access file       x (+ y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file x (+ y 1))))))
                    (if (and (< x 139) (< y 139) (digit-char-p (access file (+ x 1) (+ y 1))))
                        (progn (setq nums (+ nums 1))
                            (setq prod (* prod (getHead file (+ x 1) (+ y 1))))))
                )
            )
            (print "____")
            (print nums)
            (print prod)
            (if (= nums 2) prod 0)
        )
    )
)

(defun detectAst (file x y)
    (if (string-equal "*" (access file x y))
        (twoNay file x y)
        0))

(defun iterateMatrix (file)
    (reduce '+ 
        (loop for x from 0 to 139
            collect (reduce '+
                (loop for y from 0 to 139
                    collect (detectAst file x y))))))

(let ((lines (get-file "input.txt")))
    (print (iterateMatrix lines)))
