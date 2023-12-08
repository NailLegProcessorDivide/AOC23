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

(defun readNumList (inp)
    (loop for num in (splitmi #\space inp) collect (parse-integer num)))

(defun numInMapping (num mapping)
    (and (>= num (nth 1 mapping)) (< num (+ (nth 2 mapping) (nth 1 mapping)))))

(defun useMap (num mapping)
    (- (+ num (nth 0 mapping)) (nth 1 mapping)))

(defun mapNum (num mapping)
    (if mapping
        (if (numInMapping num (nth 0 mapping))
            (useMap num (nth 0 mapping))
            (mapNum num (cdr mapping)))
        num))

(defun mapSeed (seed maps)
    (if maps
        (mapSeed (mapNum seed (nth 0 maps)) (cdr maps))
        seed))


(defun mapSeeds (seeds maps)
    (loop for seed in seeds collect (mapSeed seed maps)))

(defun parseMapping (lines maps)
    (if lines
        (if (string-equal "-" (nth 0 lines))
            (cons maps (parseMapping (cdr lines) ()))
            (parseMapping (cdr lines) (cons (readNumList (nth 0 lines)) maps)))
        (list maps))
)

(defun parseSeeds (line)
    (let ((line (nth 1 (splitmi #\: line))))
        (loop for num in (splitmi #\space line) collect (parse-integer num))))

(let ((file (get-file "input.txt")))
    (let ((seeds (parseSeeds (nth 0 file))) (mapping (parseMapping (cdr file) ())))
        (print (reduce 'min (mapSeeds seeds mapping)))))
