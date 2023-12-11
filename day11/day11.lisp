(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun getCoords (lines)
    (let ((poss ()))
        (progn
            (loop for line in lines for y from 0 do
                (loop for i from 0 to (- (length line) 1) do
                    (if (char-equal #\# (aref line i)) (setq poss (cons (list i y) poss)))))
            poss)))

(defun isAllZeroCol (lines col)
    (if lines
        (if (char-equal (aref (nth 0 lines) col) #\.)
            (isAllZeroCol (cdr lines) col)
            nil)
        t))

(defun getXmask (lines)
    (loop for i from 0 to (- (length (nth 0 lines)) 1) collect (isAllZeroCol lines i)))

(defun isAllZero (line)
    (if (= (length line) 0)
        t
        (if (char-equal (aref line 0) #\.)
            (isAllZero (subseq line 1))
            nil)))

(defun getYmask (lines)
    (loop for line in lines
        collect (isAllZero line)))

(defun sum (xs)
    (if xs
        (+ (nth 0 xs) (sum (cdr xs)))
        0))

(defun recalcPos (x y xmap ymap exp)
    (list (sum (loop for i from 0 to (- x 1) collect (if (nth i xmap) exp 1)))
          (sum (loop for i from 0 to (- y 1) collect (if (nth i ymap) exp 1)))))

(defun remapPoss (poss xmap ymap exp)
    (loop for pos in poss collect (recalcPos (nth 0 pos) (nth 1 pos) xmap ymap exp)))

(defun calcDist (a b)
    (+ (abs (- (nth 0 a) (nth 0 b))) (abs (- (nth 1 a) (nth 1 b)))))

(defun calcdifs (poss xmap ymap exp)
    (sum (loop for poss1 on (remapPoss poss xmap ymap exp)
        collect (sum (loop for pos2 in (cdr poss1)
            collect (calcDist (nth 0 poss1) pos2))))))

;18403732 high
;9957702
(let ((lines (get-file "input.txt")))
    (print (calcdifs (getCoords lines) (getXmask lines) (getYmask lines) 2))
    (print (calcdifs (getCoords lines) (getXmask lines) (getYmask lines) 1000000)))
