(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect line)))

(defun splitmi (del inp)
    (if (position del inp)
        (cons (subseq inp 0 (position del inp)) (splitmi del (subseq inp (+ (position del inp) 1))))
        (list inp)))

(defun addCounts (c counts)
    (if counts
        (if (char-equal (nth 0 (nth 0 counts)) c)
            (cons (list (nth 0 (nth 0 counts)) (+ (nth 1 (nth 0 counts)) 1)) (cdr counts))
            (cons (nth 0 counts) (addCounts c (cdr counts)))
        )
        (list (list c 1))))

(defun addCounts2 (c counts num)
    (if counts
        (if (char-equal (nth 0 (nth 0 counts)) c)
            (cons (list (nth 0 (nth 0 counts)) (+ (nth 1 (nth 0 counts)) num)) (cdr counts))
            (cons (nth 0 counts) (addCounts2 c (cdr counts) num))
        )
        (list (list c num))))

(defun countCards (hand count)
    (if (string-equal "" hand)
        count
        (countCards (subseq hand 1) (addCounts (aref hand 0) count))))

(defun countJ (cards)
    (if cards
        (if (char-equal (nth 0 (nth 0 cards)) #\J)
            (nth 1 (nth 0 cards))
            (countJ (cdr cards)))
        0))

(defun removeJ (cards)
    (if cards
        (if (char-equal (nth 0 (nth 0 cards)) #\J)
            (cdr cards)
            (cons (nth 0 cards) (removeJ (cdr cards))))
        ()))

(defun getMaxc (cards c i)
    (if cards
        (if (> (nth 1 (nth 0 cards)) i)
            (getMaxc (cdr cards) (nth 0 (nth 0 cards)) (nth 1 (nth 0 cards)))
            (getMaxc (cdr cards) c i))
        c))

(defun reprocessCards (cards)
    (let ((jokers (countJ cards)) (baseCards (removeJ cards)))
        (addCounts2 (getMaxc baseCards #\J 0) baseCards jokers)))
(defun processCards (cards)
    cards)

(defun five (counts)
    (= (nth 1 (nth 0 counts)) 5))
(defun four (counts)
    (if counts
        (if (= (nth 1 (nth 0 counts)) 4)
            t
            (four (cdr counts)))
        nil))
(defun three (counts)
    (if counts
        (if (= (nth 1 (nth 0 counts)) 3)
            t
            (three (cdr counts)))
        nil))
(defun two (counts)
    (if counts
        (if (= (nth 1 (nth 0 counts)) 2)
            t
            (two (cdr counts)))
        nil))
(defun tPair (counts)
    (if counts
        (if (= (nth 1 (nth 0 counts)) 2)
            (two (cdr counts))
            (tPair (cdr counts)))
        nil))
(defun fullHouse (counts)
    (if counts
        (if (= (nth 1 (nth 0 counts)) 3)
            (two (cdr counts))
            (if (= (nth 1 (nth 0 counts)) 2)
                (three (cdr counts))
                (fullHouse (cdr counts))))
        nil))

(defun scoreHand (hand)
    (let ((amnts (reprocessCards (countCards hand ()))))
        (cond ((five amnts) 7)
              ((four amnts) 6)
              ((fullHouse amnts) 5)
              ((three amnts) 4)
              ((tPair amnts) 3)
              ((two amnts) 2)
              (t 1))))

(defun sortCards (c1 c2)
    (cond ((char-equal c1 #\A) nil)
          ((char-equal c2 #\A) t)
          ((char-equal c1 #\K) nil)
          ((char-equal c2 #\K) t)
          ((char-equal c1 #\Q) nil)
          ((char-equal c2 #\Q) t)
          ((char-equal c1 #\T) nil)
          ((char-equal c2 #\T) t)
          ((char-equal c1 #\9) nil)
          ((char-equal c2 #\9) t)
          ((char-equal c1 #\8) nil)
          ((char-equal c2 #\8) t)
          ((char-equal c1 #\7) nil)
          ((char-equal c2 #\7) t)
          ((char-equal c1 #\6) nil)
          ((char-equal c2 #\6) t)
          ((char-equal c1 #\5) nil)
          ((char-equal c2 #\5) t)
          ((char-equal c1 #\4) nil)
          ((char-equal c2 #\4) t)
          ((char-equal c1 #\3) nil)
          ((char-equal c2 #\3) t)
          ((char-equal c1 #\2) nil)
          ((char-equal c2 #\2) t)
          ((char-equal c1 #\J) nil)
          ((char-equal c2 #\J) t)
          (t t)))

(defun sortHand (l r)
    (if (string-equal l "")
        t
        (if (char-equal (aref l 0) (aref r 0))
            (sortHand (print (subseq l 1)) (print (subseq r 1)))
            (sortCards (aref l 0) (aref r 0)))))

(defun sortHands (l r)
    (if (= (nth 0 l) (nth 0 r))
        (sortHand (nth 1 l) (nth 1 r))
        (< (nth 0 l) (nth 0 r))))

; hand cards bid
(defun prepLine (line)
    (let ((parts (splitmi #\space line)))
        (list (scoreHand (nth 0 parts)) (nth 0 parts) (parse-integer (nth 1 parts)))))

(print (scoreHand "23456"))
(print (scoreHand "32T3K"))
(print (scoreHand "T55J5"))
(print (scoreHand "KK677"))
(print (scoreHand "KTJJT"))
(print (scoreHand "QQQJA"))
;246574824 high
;246433301 low
;   ||||||
(let ((parts (loop for line in (get-file "input.txt") collect (prepLine line))))
    (print (reduce '+ (loop for hand in (sort parts 'sortHands) for i from 1 collect (* i (nth 2 hand))))))

