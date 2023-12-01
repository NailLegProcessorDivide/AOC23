(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun section (line)
    (let ((digs ()))
        (progn
            (loop for c across line
            do 
                (if (digit-char-p c 10)
                    (setq digs (append digs (list (parse-integer (string c)))))
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
