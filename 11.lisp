;; *******
;; INPUT
;; *******

(defparameter actual-input "1 24596 0 740994 60 803 8918 9405859")

(defparameter test-input "125 17")


;; ******
;; MACROS
;; ******

(defmacro partial (func &rest args)
  `(lambda (x) (funcall ,func x ,@args)))

(defmacro maprange (func amount)
  `(let (value)
     (dotimes (number ,amount)
       (setq value (cons (funcall ,func number) value)))
     value))

(defmacro with-print (print-statement expr)
  `(progn
    (format t ,print-statement)
    (terpri)
    ,expr))

(defparameter outside-grid-val -9000)

;; *******
;; UTILITY
;; *******

(defun count-unique (lst)
  (if (equal '() lst)
      0
      (+
       (if (member (car lst) (cdr lst) :test #'equal) 0 1)
       (count-unique (cdr lst)))))

(defun unique (lst)
  (if (equal '() lst)
      '()
      (append
        (if (member (car lst) (cdr lst) :test #'equal) '() (list (first lst)))
        (unique (cdr lst)))))

(defun count-val (val lst)
  (count val lst :test #'equal))


(defun split (input)
  (split-sequence:split-sequence #\Space input))

(assert (equal '("12" "18" "0") (split "12 18 0")))

(defun join (lst)
  (format nil "~{~a~^ ~}" lst))

(assert (equal "12 18 0" (join (split "12 18 0"))))

(defun reduce-zeroes (num-str)
  (format nil "~d" (parse-integer num-str)))

;; ********
;; SOLUTION
;; ********

(defun transform-stone (stone)
  (cond ((string= stone "0") (list "1"))
        ((= 0 (mod (length stone) 2))
          (list
           (subseq stone 0 (/ (length stone) 2))
           (reduce-zeroes (subseq stone (/ (length stone) 2)))))
        (t (list (format nil "~d" (* 2024 (parse-integer stone)))))))

(defun transform-stones (stones)
  (mapcan #'transform-stone stones))

(assert (equal '("253000" "1" "7") (transform-stones (split test-input))))

(defun blink (stones times)
  (if (= 0 times)
      stones
      (blink (transform-stones stones) (1- times))))

(assert (equal '("253000" "1" "7") (blink (split test-input) 1)))
(assert (equal '("253" "0" "2024" "14168") (blink (split test-input) 2)))
(assert (equal '("2097446912" "14168" "4048" "2" "0" "2" "4" "40" "48" "2024" "40" "48" "80" "96" "2" "8" "6" "7" "6" "0" "3" "2") (blink (split test-input) 6)))
(assert (equal '("1" "2024" "1" "0" "9" "9" "2021976") (blink (split "0 1 10 99 999") 1)))

(defun cnt-lst-vals (cnt-lst)
  (unique (mapcar #'second cnt-lst)))

(defun count-cnt-lst-val (cnt-lst val)
  (apply #'+ (mapcar #'first (remove-if-not (lambda (x) (equal (second x) val)) cnt-lst))))

(assert (equal '(2 3 1) (cnt-lst-vals (list '(1 1) '(3 1) '(1 2) '(10 3) '(12 3) '(1 3) '(1 1)))))
(assert (equal 23 (count-cnt-lst-val (list '(1 1) '(3 1) '(1 2) '(10 3) '(12 3) '(1 3) '(1 1)) 3)))

(defun simplify-cnt-lst (cnt-lst)
  (mapcar (lambda (val) (list (count-cnt-lst-val cnt-lst val) val)) (cnt-lst-vals cnt-lst)))

(defun transform-cnt-lst (cnt-lst)
  (simplify-cnt-lst (mapcan
      (lambda (x)
        (let ((new-lst (transform-stone (second x))))
          (mapcar (lambda (y) (list (first x) y)) new-lst)))
      cnt-lst)))

(defun get-cnt-lst-len (cnt-lst)
  (apply #'+ (mapcar #'first cnt-lst)))

(defun transform-lst-rec (cnt-lst times)
  (if (= 0 times) 
      cnt-lst
      (transform-lst-rec (transform-cnt-lst cnt-lst) (1- times))))

(assert (= 55312 (get-cnt-lst-len (transform-lst-rec (list (list 1 "125") (list 1 "17")) 25))))

(defun stones-to-cnt-lst (stones)
  (simplify-cnt-lst (mapcar (lambda (x) (list 1 x)) (split stones))))

(defun do-part-1/2 (stones times)
  (get-cnt-lst-len (transform-lst-rec (stones-to-cnt-lst stones) times)))

(assert (= 55312 (do-part-1/2 test-input 25)))

; (defun uniq-cnt-lst (cnt-lst)
;   )

; (defun do-part-1 (input)
;   (length (tree-blink (split input) 25)))

; (assert (= 55312 (do-part-1 test-input)))
; (assert (= 55312 (length (tree-blink (split test-input) 25))))

;;********** I need to sort the list and store the amount that things are repeated then dont save the cache, but still dont do duplicate work per level.