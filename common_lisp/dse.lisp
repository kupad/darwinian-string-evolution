;;;;;;;;;;;;;;;;;;
;;utilities
;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro n-of (n expr)
  (with-gensyms (gn gi gacc)
    `(let ((,gn ,n) ;only eval n once
           (,gacc nil)) 
      (dotimes (,gi ,gn)
        (push ,expr ,gacc))
      (nreverse ,gacc))))

(defun most (cmp-fn score-fn lst)
  (labels ((most-h (cmp-fn score-fn lst win max)
             (if (null lst)
                 (values win max)
                 (let ((score (funcall score-fn (car lst))))
                   (if (funcall cmp-fn score max)
                       (most-h cmp-fn score-fn (cdr lst) (car lst) score)
                       (most-h cmp-fn score-fn (cdr lst) win max))))))
    (most-h cmp-fn score-fn (cdr lst) (car lst) (funcall score-fn (car lst)))))

;;;;;;;;;;;;;
;;end utilities
;;;;;;;;;;;;;

(defparameter *target-string* "MORE GIDDY IN MY DESIRES THAN A MONKEY")

;;turn this into a vector?
(defparameter *alphabet* (coerce 
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ " 
                           'list))

(defun random-letter (alphabet)
  (nth (random (length alphabet)) alphabet))

(defun random-phrase (len alphabet)
  (n-of len (random-letter alphabet)))

(defun print-phrase (phrase)
  (let ((s (coerce phrase 'string)))
    (format t "~A~%" s)
    s))

(defun monkey (next-phrase-selector target-string alphabet &optional (max-attempts 10000)) 
  (let ((target-phrase (coerce target-string 'list)))
    (do ((attempt 0 (1+ attempt))
         (current-phrase 
           (random-phrase (length target-phrase) alphabet)
           (funcall next-phrase-selector current-phrase target-phrase alphabet)))
        ((or (= attempt max-attempts) (equal current-phrase target-phrase)) 
         (format t "attempts: ~A~%" attempt)
         (print-phrase current-phrase))
        (print-phrase current-phrase))))

(defun hoyle-monkey (target-string alphabet)
  (monkey
    #'(lambda (current-phrase target-phrase alphabet) 
        (random-phrase (length target-phrase) alphabet))
    target-string
    alphabet))

;;copy phrase, and randomly select a member of the phrase and replace it with a random letter
(defun mutate (phrase alphabet)
  (let ((mutated-phrase (copy-list phrase)))
    (setf (nth (random (length phrase)) mutated-phrase) 
          (random-letter alphabet))
    mutated-phrase))

(defun next-generation (phrase size alphabet)
  (n-of size (mutate phrase alphabet)))

;;hamming distance
(defun hamming-distance (a b)
  (reduce '+ (mapcar #'(lambda (x y) (if (equal x y) 0 1)) a b)))

;select the phrase from the generation that has the lowest hamming distance to the 
;target phrase
(defun select (generation target-phrase)
  (most #'< 
        #'(lambda (phrase) (hamming-distance phrase target-phrase)) 
        generation))


(defun darwin-monkey (target-string alphabet &optional (generation-size 50))
  (monkey 
    #'(lambda (current-phrase target-phrase alphabet) 
        (select (next-generation current-phrase generation-size alphabet) target-phrase))
    target-string
    alphabet))


