;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"13.txt")))

(defparameter test-input
              (list
               "Button A: X+94, Y+34"
               "Button B: X+22, Y+67"
               "Prize: X=8400, Y=5400"
               ""
               "Button A: X+26, Y+66"
               "Button B: X+67, Y+21"
               "Prize: X=12748, Y=12176"
               ""
               "Button A: X+17, Y+86"
               "Button B: X+84, Y+37"
               "Prize: X=7870, Y=6450"
               ""
               "Button A: X+69, Y+23"
               "Button B: X+27, Y+71"
               "Prize: X=18641, Y=10279"
               ""))

(defparameter test-input-2
              (list
               "Button A: X+94, Y+34"
               "Button B: X+22, Y+67"
               "Prize: X=10000000008400, Y=10000000005400"
               ""
               "Button A: X+26, Y+66"
               "Button B: X+67, Y+21"
               "Prize: X=100000000012748, Y=100000000012176"
               ""
               "Button A: X+17, Y+86"
               "Button B: X+84, Y+37"
               "Prize: X=10000000007870, Y=10000000006450"
               ""
               "Button A: X+69, Y+23"
               "Button B: X+27, Y+71"
               "Prize: X=100000000018641, Y=100000000010279"
               ""))

;; ******
;; MACROS
;; ******

(defmacro partial (func &rest args)
  `(lambda (x) (funcall ,func x ,@args)))

(defmacro print-val (expr)
  `(let ((res ,expr))
     (progn
      (format t "~a: ~a" ',expr ,'res)
      (terpri)
      res)))

(defmacro time-expr (expr)
  `(let ((time (get-internal-real-time)))
     (let ((res ,expr))
       (format t "Statement ~a Elapsed time: ~a~%" ',expr (- (get-internal-real-time) time))
       res)))


;; ********
;; Parsing*
;; ********


(defun get-machine-descrs (input &optional (descr-so-far '()))
  (if (< (length input) 4)
      descr-so-far
      (let ((descrs (append descr-so-far (list (subseq input 0 3)))))
        (get-machine-descrs (subseq input 4) descrs))))

(defun extract-x-from-line (line)
  (parse-integer (cl-ppcre:regex-replace-all ",.*" (cl-ppcre:regex-replace-all ".*X." line "") "")))

(defun extract-y-from-line (line)
  (parse-integer (cl-ppcre:regex-replace-all ".*Y." line "")))

(defun get-ab-x (machine-descr)
  (list (extract-x-from-line (first machine-descr)) (extract-x-from-line (second machine-descr))))

(defun get-ab-y (machine-descr)
  (list (extract-y-from-line (first machine-descr)) (extract-y-from-line (second machine-descr))))

(defun get-result-x (machine-descr)
  (extract-x-from-line (third machine-descr)))

(defun get-result-y (machine-descr)
  (extract-y-from-line (third machine-descr)))


;; ********
;; SOLUTION
;; ********

; FIND FIRST COMBINATION FOR BOTH Ys AND Xs
; THEN SUBTRACT BY LCM ON BOTH NUMBERS UNTIL THE COUNTS MATCH -> SHOULD BE AUTOMATICALLY FINDABLE
; I know all LCMs that fit into the result belong to the more efficient one
(defun find-combinations (ab result ab-y result-y)
  (let* ((a (first ab))
         (b (second ab))
         (a-cnt (1+ (floor result a)))
         (subtractor 1))
    (loop while (> a-cnt 0)
          do (setf a-cnt (- a-cnt subtractor))
            when (let ((remainder (- result (* a-cnt a))))
                   (if (= 0 (mod remainder b)) (let ((combination (list a-cnt (floor remainder b))))
                                                 (progn (setf subtractor (floor (lcm a b) a))
                                                        (if (is-combination-valid ab-y result-y combination) combination)))))
            return it)))

(defun find-combinations-simple (ab result)
  (let* ((a (first ab))
         (b (second ab))
         (a-cnt (1+ (floor result a))))
    (loop while (> a-cnt 0)
          do (setf a-cnt (1- a-cnt))
            when (let ((remainder (- result (* a-cnt a))))
                   (if (= 0 (mod remainder b)) (list a-cnt (floor remainder b))))
          collect it)))

(defun find-best-combination-efficient (ab-x result-x ab-y result-y)
  (if (<= (* 3 (+ (second ab-x) (second ab-y))) (+ (first ab-x) (first ab-y)))
      (find-combinations ab-x result-x ab-y result-y)
      (reverse (find-combinations (reverse ab-x) result-x (reverse ab-y) result-y))))

(defun find-best-combination-with-lcm-opti (ab-x result-x ab-y result-y)
  (if (> (apply #'lcm ab-x) (apply #'lcm ab-x))
      (find-best-combination-efficient ab-x result-x ab-y result-y)
      (find-best-combination-efficient ab-y result-y ab-x result-x)))

(assert (equal '(80 40) (find-best-combination-efficient '(94 22) 8400 '(34 67) 5400)))

(defun calc-combination-price (combination)
  (+ (* 3 (first combination)) (second combination)))

(assert (= 280 (calc-combination-price '(80 40))))

(defun is-combination-valid (ab result combination)
  (= result (+ (* (first ab) (first combination)) (* (second ab) (second combination)))))

(assert (is-combination-valid '(34 67) 5400 '(80 40)))

; (defun find-both-combinations (ab-x result-x ab-y result-y)
;   (remove-if-not (lambda (combination) (is-combination-valid ab-y result-y combination)) (time-expr (find-combinations-efficient ab-x result-x ab-y result-y))))

; (assert (member '(80 40) (find-both-combinations '(94 22) 8400 '(34 67) 5400) :test #'equal))

(defun cheapest-combination-price (combinations)
  (apply #'min (mapcar #'calc-combination-price combinations)))

(defun cheapest-possible-combination-price (ab-x result-x ab-y result-y)
  (let ((combination (find-best-combination-efficient ab-x result-x ab-y result-y)))
    (if combination (calc-combination-price combination))))

(assert (= 280 (cheapest-possible-combination-price '(94 22) 8400 '(34 67) 5400)))

(defun get-cheapest-tokens-for-machine-descr (machine-descr)
  (progn
   (format t "DOING: ~a" machine-descr)
   (cheapest-possible-combination-price
     (get-ab-x machine-descr)
     (get-result-x machine-descr)
     (get-ab-y machine-descr)
     (get-result-y machine-descr))))

(defun calc-price (machine-descr)
  (let* ((xa (first (get-ab-x machine-descr)))
         (xb (second (get-ab-x machine-descr)))
         (ya (first (get-ab-y machine-descr)))
         (yb (second (get-ab-y machine-descr)))
         (rx (+ (get-result-x machine-descr) 10000000000000))
         (ry (+ (get-result-y machine-descr) 10000000000000))
         (ma (/
               (- (/ rx xa) (/ (* xb ry) (* xa yb)))
               (- 1 (/ (* xb ya) (* xa yb)))))
         (mb (/ (- ry (* ma ya)) yb)))
    (if (and (= (floor ma) ma) (= (floor mb) mb))
        (calc-combination-price (list ma mb)))))

(assert (= 280 (get-cheapest-tokens-for-machine-descr (first (get-machine-descrs test-input)))))
(assert (= 280 (calc-price (first (get-machine-descrs test-input)))))

(defun get-cheapest-total (input)
  (apply #'+ (remove NIL (mapcar #'get-cheapest-tokens-for-machine-descr (get-machine-descrs input)))))

(assert (= 480 (get-cheapest-total test-input)))

(defun calc-total (input)
  (apply #'+ (remove NIL (mapcar #'calc-price (get-machine-descrs input)))))

(assert (= 480 (calc-total test-input)))