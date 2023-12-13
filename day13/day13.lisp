(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun breakNewLines (lines)
    (if lines
        (if (string-equal (nth 0 lines) "")
            (cons () (breakNewLines (cdr lines)))
            (let ((rec (breakNewLines (cdr lines))))
                (cons (cons (nth 0 lines) (nth 0 rec)) (cdr rec))))
        ()))
            
(defun reencode (block)
    (let ((xmask (loop for i from 1 to (length (nth 0 block)) collect 0))
        (ymask (loop for i from 1 to (length block) collect 0)))
        (progn
            (loop for line in block for y from 0 do
                (loop for x from 0 to (- (length line) 1) do
                    (progn
                        (setf (elt ymask y) (+ (* 2 (nth y ymask)) (if (char-equal (aref line x) #\#) 1 0)))
                        (setf (elt xmask x) (+ (* 2 (nth x xmask)) (if (char-equal (aref line x) #\#) 1 0))))))
            (list xmask ymask))))

(defun checkMirror (mask x step errors)
    (if (and (>= (- x step) 0) (< (+ x 1 step) (length mask)))
        (checkMirror mask x (+ step 1) (+ errors (logcount (logxor (nth (- x step) mask) (nth (+ x 1 step) mask)))))
        errors))

(defun part1 (mask x)
    (= (checkMirror mask x 0 0) 0))

(defun part2 (mask x)
    (and (= (checkMirror mask x 0 0) 1) (not (= (checkMirror mask x 0 0) 0))))

(defun checkMask (checkfn mask x)
    (if (< x (- (length mask) 1))
        (if (funcall checkfn mask x)
            (+ x 1)
            (checkMask checkfn mask (+ x 1)))
        ()))

(defun findMirror (checkfn masks)
    (let ((xpos (checkMask checkfn (nth 0 masks) 0)))
        (if xpos xpos
            (* (checkMask checkfn (nth 1 masks) 0) 100))))

(defun doDay (checkfn chunks)
    (reduce '+ (loop for chunk in chunks collect (findMirror checkfn chunk))))

(let ((chunks (loop for chunk in (breakNewLines (get-file "input.txt")) collect (reencode chunk))))
    (progn
        (print (doDay 'part1 chunks))
        (print (doDay 'part2 chunks))))
