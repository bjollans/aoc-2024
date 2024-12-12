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


(defun transform-stones (stones)
  (join (mapcan #'transform-stone (split stones))))

(assert (equal "253000 1 7" (transform-stones test-input)))

(defun blink (stones times)
  (with-print (format nil "blinking stones. Times: ~d; Length: ~d" times (length (split stones)))
              (if (= 0 times)
                  stones
                  (blink (transform-stones stones) (1- times)))))


(defparameter stone-cache (make-hash-table))

(defun get-from-cache (stone)
  (let ((cache-key (parse-integer stone)))
    (gethash cache-key stone-cache)))

(defun add-to-cache (stone result)
  (let ((cache-key (parse-integer stone)))
    (setf (gethash cache-key stone-cache) result)))

(assert (not (get-from-cache "0")))
(add-to-cache "0" 34)
(assert (= 34 (get-from-cache "0")))

(setf stone-cache (make-hash-table))
(assert (not (get-from-cache "0")))


(defun tree-blink (stones-list times)
  (if (= 0 times)
      stones-list
      (mapcan (lambda (stone)
                (tree-blink (transform-stone stone) (1- times)))
          stones-list)))

(defun blink-in-5s (stones-list times)
  (if (<= times 0)
      stones-list
      (apply #'append (mapcar (lambda (stone)
                (let ((result (or (get-from-cache stone) (tree-blink (split stone) 5))))
                  (if (not (get-from-cache stone)) (add-to-cache stone result))
                  (blink-in-5s result (1- times))))
          stones-list))))

(assert (equal '("1036288" "7" "2" "20" "24" "4048" "1" "4048" "8096" "28" "67" "60" "32") (blink-in-5s (split test-input) 1)))

(assert (string= "253000 1 7" (blink test-input 1)))
(assert (equal '("253000" "1" "7") (tree-blink (split test-input) 1)))
(assert (string= "253 0 2024 14168" (blink test-input 2)))
(assert (string= "1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32" (blink test-input 5)))
(assert (equal '("1036288" "7" "2" "20" "24" "4048" "1" "4048" "8096" "28" "67" "60" "32") (tree-blink (split test-input) 5)))
(assert (string= "2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2" (blink test-input 6)))
(assert (equal '("2097446912" "14168" "4048" "2" "0" "2" "4" "40" "48" "2024" "40" "48" "80" "96" "2" "8" "6" "7" "6" "0" "3" "2") (tree-blink (split test-input) 6)))
(assert (string= "1 2024 1 0 9 9 2021976" (blink "0 1 10 99 999" 1)))

(defun do-part-1 (input)
  (length (tree-blink (split input) 25)))

(assert (= 55312 (do-part-1 test-input)))
(assert (= 55312 (length (tree-blink (split test-input) 25))))
(assert (= 55312 (length (blink-in-5s (split test-input) 5))))

(defun do-part-2 (input)
  (length (split (blink input 75))))