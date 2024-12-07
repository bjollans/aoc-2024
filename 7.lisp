(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"7.txt")))

(defparameter test-input
              (list
               "190: 10 19"
               "3267: 81 40 27"
               "83: 17 5"
               "156: 15 6"
               "7290: 6 8 6 15"
               "161011: 16 10 13"
               "192: 17 8 14"
               "21037: 9 7 18 13"
               "292: 11 6 16 20"))

(defun get-result (line)
  (parse-integer (first (cl-ppcre:split ":" line))))

(defun get-nums (line)
  (mapcar #'parse-integer(cl-ppcre:split " " (second (cl-ppcre:split ": " line)))))

(defun apply-operators-to-list (val lst operators)
  (if (null lst)
      (list val)
      (mapcan
          (lambda (operator)
            (apply-operators-to-list
              (apply operator (list val (first lst)))
              (cdr lst)
              operators))
          operators)))

(defun is-result-possible (result nums operators)
    (member result (apply-operators-to-list (first nums) (cdr nums) operators)))

(assert (is-result-possible 190 (list 10 19) (list #'+ #'*)))
(assert (is-result-possible 3267 (list 81 40 27) (list #'+ #'*)))
(assert (not (is-result-possible 83 (list 17 5) (list #'+ #'*))))

(defun is-line-possible (line operators)
    (is-result-possible (get-result line) (get-nums line) operators))

(assert (is-line-possible "190: 10 19" (list #'+ #'*)))

(defun get-possible-lines (lines operators)
    (remove-if-not 
            (lambda (line) (is-line-possible line operators))
            lines))

(defun do-part-1 (input)
    (apply #'+ (mapcar #'get-result (get-possible-lines input (list #'+ #'*)))))

(assert (= 3749 (do-part-1 test-input)))

(defun concat-nums (num1 num2)
    (parse-integer (format nil "~d~d" num1 num2)))

(assert (= 1213 (concat-nums 12 13)))
(assert (= 1213 (apply #'concat-nums (list 12 13))))

(defun do-part-2 (input)
    (apply #'+ (mapcar #'get-result (get-possible-lines input (list #'+ #'* #'concat-nums)))))


(assert (= 11387 (do-part-2 test-input)))