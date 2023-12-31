(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun section (line)
    (let ((digs ()))
        (progn
            (loop for idex from 0 to (- (length line) 1)
            do 
                (let ((c (aref line idex)) (linepart (subseq line idex)))
                    (if (digit-char-p c 10)
                        (setq digs (append digs (list (parse-integer (string c)))))
                        (if (string-equal "one" (subseq linepart 0 (min (length linepart) 3)))
                            (setq digs (append digs (list 1)))
                            (if (string-equal "two" (subseq linepart 0 (min (length linepart) 3)))
                                (setq digs (append digs (list 2)))
                                (if (string-equal "three" (subseq linepart 0 (min (length linepart) 5)))
                                    (setq digs (append digs (list 3)))
                                    (if (string-equal "four" (subseq linepart 0 (min (length linepart) 4)))
                                        (setq digs (append digs (list 4)))
                                        (if (string-equal "five" (subseq linepart 0 (min (length linepart) 4)))
                                            (setq digs (append digs (list 5)))
                                            (if (string-equal "six" (subseq linepart 0 (min (length linepart) 3)))
                                                (setq digs (append digs (list 6)))
                                                (if (string-equal "seven" (subseq linepart 0 (min (length linepart) 5)))
                                                    (setq digs (append digs (list 7)))
                                                    (if (string-equal "eight" (subseq linepart 0 (min (length linepart) 5)))
                                                        (setq digs (append digs (list 8)))
                                                        (if (string-equal "nine" (subseq linepart 0 (min (length linepart) 4)))
                                                            (setq digs (append digs (list 9)))
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (list digs)
        )
    )
)

(defun collect-all (inp)
    (loop for line in inp
        collect
            (let ((digits (car (section line))))
                (+ (* 10 (car digits)) (car (last digits)))
        )
    )
)

(let ((nums (collect-all(get-file "input.txt"))) (total 0))
    (progn
        (loop for n in nums
            do (setq total (+ total n)))
        (write "total ")
        (write total)
    )
)
