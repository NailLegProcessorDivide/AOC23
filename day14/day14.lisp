(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun reproc (lines)
    (loop for line in lines collect
        (loop for i from 0 to (- (length line) 1) collect
            (cond
                ((char-equal (aref line i) #\.) 0)
                ((char-equal (aref line i) #\O) 2)
                (t 1)))))

(defun tileOneNorth (map x y)
    (if (and (= (nth x (nth y map)) 2) (> y 0) (= (nth x (nth (- y 1) map)) 0))
        (progn
            (setf (elt (elt map y) x) 0)
            (setf (elt (elt map (- y 1)) x) 2)
            (tileOneNorth map x (- y 1)))))

(defun tiltNorth (map)
    (loop for row in map for y from 0 do
        (loop for col in row for x from 0 do
            (tileOneNorth map x y))))
;;;
(defun tileOneWest (map x y)
    (if (and (= (nth x (nth y map)) 2) (> x 0) (= (nth (- x 1) (nth y map)) 0))
        (progn
            (setf (elt (elt map y) x) 0)
            (setf (elt (elt map y) (- x 1)) 2)
            (tileOneWest map (- x 1) y))))

(defun tiltWest (map)
    (loop for row in map for y from 0 do
        (loop for col in row for x from 0 do
            (tileOneWest map x y))))
;;;
(defun tileOneSouth (map x y)
    (if (and (= (nth x (nth y map)) 2) (< y (- (length (nth 0 map)) 1)) (= (nth x (nth (+ y 1) map)) 0))
        (progn
            (setf (elt (elt map y) x) 0)
            (setf (elt (elt map (+ y 1)) x) 2)
            (tileOneSouth map x (+ y 1)))))

(defun tiltSouth (map)
    (loop for row in map for y from 0 do
        (loop for col in row for x from 0 do
            (tileOneSouth map x (- (length map) 1 y)))))
;;;
(defun tileOneEast (map x y)
    (if (and (= (nth x (nth y map)) 2) (< x (- (length (nth 0 map)) 1)) (= (nth (+ x 1) (nth y map)) 0))
        (progn
            (setf (elt (elt map y) x) 0)
            (setf (elt (elt map y) (+ x 1)) 2)
            (tileOneEast map (+ x 1) y))))

(defun tiltEast (map)
    (loop for row in map for y from 0 do
        (loop for col in row for x from 0 do
            (tileOneEast map (- (length row) 1 x) y))))

(defun scoreMap (map)
    (reduce '+ (loop for row in map for y from 0 collect
        (reduce '+ (loop for col in row for x from 0 collect
            (if (= (nth x (nth y map)) 2) (- (length map) y) 0))))))

(let ((inp (reproc (get-file "input.txt"))))
    (progn
        (loop for i from 1 to 1000000000 do
            (progn
                (tiltNorth inp)
                (tiltWest inp)
                (tiltSouth inp)
                (tiltEast inp)
                (format T "~d,~d" i (scoreMap inp))
                (terpri)))))
