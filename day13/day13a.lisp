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

(defun sum (l)
    (reduce '+ l))
            
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

(defun checkMirror (mask x step)
    (if (and (>= (- x step) 0) (< (+ x 1 step) (length mask)))
        (if (= (nth (- x step) mask) (nth (+ x 1 step) mask))
            (checkMirror mask x (+ step 1))
            nil)
        t))

(defun checkMask (mask x)
    (if (< x (- (length mask) 1))
        (if (checkMirror mask x 0)
            (+ x 1)
            (checkMask mask (+ x 1)))
        ()))

(defun findMirror (xmask ymask)
    (let ((xpos (checkMask xmask 0)))
        (if xpos xpos
            (* (checkMask ymask 0) 100))))

(defun findMirrorP (masks)
    (findMirror (nth 0 masks) (nth 1 masks)))

(let ((chunks (breakNewLines (get-file "input.txt"))))
    (print (sum (map 'list 'findMirrorP (map 'list 'reencode chunks)))))