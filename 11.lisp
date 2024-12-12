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

(assert (equal (list "1") (transform-stone "0")))
(assert (equal (list "2024") (transform-stone "1")))
(assert (equal (list "20" "24") (transform-stone "2024")))

(defun tree-blink (stones-list times &optional (depth 0))
  (if (= depth times)
      (length stones-list)
      (apply #'+ (mapcar (lambda (stone)
                (* (count-val stone stones-list) (tree-blink (transform-stone stone) times (1+ depth))))
          (unique stones-list)))))

(assert (equal 3 (tree-blink (split test-input) 1)))
(assert (equal 13 (tree-blink (split test-input) 5)))
(assert (equal 22 (tree-blink (split test-input) 6)))
(assert (equal 55312 (tree-blink (split test-input) 25)))

; (defun do-part-1 (input)
;   (length (tree-blink (split input) 25)))

; (assert (= 55312 (do-part-1 test-input)))
; (assert (= 55312 (length (tree-blink (split test-input) 25))))

;;********** I need to sort the list and store the amount that things are repeated then dont save the cache, but still dont do duplicate work per level.