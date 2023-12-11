(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

(defun within (dig set)
    (cond ((null set) nil)
        ((= (nth 0 set) dig) t)
        (t (within dig (cdr set)))))

(defun head (l)
    (nth 0 l))

(defun sortOps (l r)
    (< (head l) (head r)))

(defun readNumList (inp)
    (loop for num in (splitmi #\space inp) collect (parse-integer num)))

(defun makeRanges (inp)
    (if inp
        (cons (list (head inp) (nth 1 inp)) (cdr (cdr inp)))
        ()))

(defun numInMapping (range mapping)
    (and (>= range (nth 1 mapping)) (< range (+ (nth 2 mapping) (nth 1 mapping)))))

(defun useMap (num mapping)
    (- (+ num (nth 0 mapping)) (nth 1 mapping)))

(defun mapRange (range mapping)
    (loop for mapp in mapping
        do (progn
            )
        (if (numInMapping range (head mapping) 'sortOps)
            (useMap num (head mapping) 'sortOps)
            (mapRange range (cdr mapping)))
        (list num)))

(defun mapSeed (range maps)
    (if maps
        (mapSeed (mapRange range (sort (nth 0 maps) 'sortOps)) (cdr maps))
        range))


(defun mapSeeds (ranges maps)
    (loop for range in ranges collect (mapSeed range maps)))

(defun parseMapping (lines maps)
    (if lines
        (if (string-equal "-" (nth 0 lines))
            (cons maps (parseMapping (cdr lines) ()))
            (parseMapping (cdr lines) (cons (readNumList (nth 0 lines)) maps)))
        (list maps))
)

(defun parseSeeds (line)
    (let ((line (nth 1 (splitmi #\: line))))
        (makeRanges (readNumList line))))

(let ((file (get-file "input.txt")))
    (let ((ranges (parseSeeds (nth 0 file))) (mapping (parseMapping (cdr file) ())))
        (print (reduce 'min (mapSeeds (print ranges) (print mapping))))))
