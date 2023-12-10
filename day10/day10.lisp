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
    (loop for num in (splitmi #\space inp) collect (parse-integer num)))

(defun head (l)
    (nth 0 l))
(defun north (l)
    (nth 0 l))
(defun east (l)
    (nth 1 l))
(defun south (l)
    (nth 2 l))
(defun west (l)
    (nth 3 l))
(defun visited (l)
    (nth 4 l))
(defun sum (l)
    (reduce '+ l))

(defun all-zero (inp)
    (if inp
        (if (= (head inp) 0)
            (all-zero (cdr inp))
            nil)
        t))

(defun differences (inp)
    (loop for a in inp
        for b in (cdr inp)
        collect (- a b)))

(defun find-start (inp)
    (if (all-zero inp)
        0
        (+ (head inp) (process-list (differences inp)))))

(defun getxy (mat x y)
    (nth x (nth y mat)))

;neswv

(defun step-list (matrx)
    (let ((updated nil))
        (let ((matnew (loop for line in matrx for y from 0 collect
            (loop for c in line for x from 0 collect
                (if (visited c)
                    c
                    (if (or (and (north c) (>      y              0)  (visited (getxy matrx      x (- y 1))) (south (getxy matrx      x (- y 1))) )
                            (and (east  c) (< (+ x 1) (length  line)) (visited (getxy matrx (+ x 1)     y))  (west  (getxy matrx (+ x 1)      y))  )
                            (and (south c) (< (+ y 1) (length matrx)) (visited (getxy matrx      x (+ y 1))) (north (getxy matrx      x (+ y 1))) )
                            (and (west  c) (>      x              0)  (visited (getxy matrx (- x 1)     y))  (east  (getxy matrx (- x 1)      y))  ))
                        (progn
                            (setq updated t)
                            (list (north c) (east c) (south c) (west c) t))
                        c))))))
            (list updated matnew))))

(defun addParity (c parity)
    (cond
        ((and (north c) (south c)) (+ parity 2))
        ((north c) (if (east c) (+ parity 1) (- parity 1)))
        ((south c) (if (east c) (- parity 1) (+ parity 1)))
        (t parity)))

(defun count-inner (map parity)
    (if map
        (+
            (if (and (= (mod parity 4) 2) (not (visited (head map)))) 1 0)
            (count-inner (cdr map) (if (visited (head map)) (addParity (head map) parity) parity)))
        0))

(defun map-char (c)
    (cond
        ((char-equal c #\|) (list  t ()  t () ()))
        ((char-equal c #\-) (list ()  t ()  t ()))
        ((char-equal c #\L) (list  t  t () () ()))
        ((char-equal c #\J) (list  t () ()  t ()))
        ((char-equal c #\7) (list () ()  t  t ()))
        ((char-equal c #\F) (list ()  t  t () ()))
        ((char-equal c #\.) (list () () () () ()))
        ((char-equal c #\S) (list  t  t  t  t  t))
        (t (list () () () () ()))))

(defun process-line (line)
    (loop for idex from 0 to (- (length line) 1) collect (map-char (aref line idex))))

;3835 high
;387 high
;638 high
(let ((matrx (loop for line in (get-file "input.txt") collect (process-line line))) (updat t))
    (progn
        (loop for i from 0 while updat do
            (progn
                (let ((res (step-list matrx)))
                    (setq updat (nth 0 res))
                    (setq matrx (nth 1 res))
                    (print i))))
        (print (sum (loop for line in matrx collect (count-inner line 0))))))

