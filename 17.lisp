(defmacro prgfun (name register-name new-val-expr)
  `(defun ,name (prg)
     (progn
      (setf (,register-name prg) ,new-val-expr)
      (setf (ip prg) (+ 2 (ip prg))))))


(defmacro print-val (expr)
  `(let ((res ,expr))
     (progn
      (format t "~a: ~a" ',expr ,'res)
      (terpri)
      res)))


(defclass Program ()
    ((A :initarg :A :accessor A)
     (B :initarg :B :accessor B)
     (C :initarg :C :accessor C)
     (program :initarg :program :accessor program)
     (ip :initarg :ip :accessor ip)
     (output :initarg :output :accessor output)))

;jump only happens with 3 0 at end (for all progs) -> Will just repeat everything until a is 0

(defun actual-prog ()
  (make-instance 'Program
    :A 59397658
    :B 0
    :C 0
    :program '(2 4 1 1 7 5 4 6 1 4 0 3 5 5 3 0)
    :ip 0
    :output '()))

; 2 4 -> write A mod 8 to B
; 1 1 -> Increments B
; 7 5 -> shifts a to the left by B and stores in C (first time: write A<<<3 to C)
; 4 6 -> XOR of b and c (XOR of A and A+1 first time) stores in B
; 1 4 -> XOR to b and 4; stores in B
; 0 3 -> Shifts A by 3 to left
; 5 5 -> prints B
;-> cycles until A is 0

(defun fast-actual-run (a list-goal)
  (if (= 0 a) '()
      (let* ((b (logxor 1 (logand 7 a)))
             (next-print (logand 7 (logxor 4 (logxor b (ash a (* -1 b)))))))
        ; (if (not (= (first list-goal) next-print))
        ;     '()
        (append (list next-print) (fast-actual-run (ash a -3) (cdr list-goal))))))


(defun fast-actual-run-simple (a)
  (if (= 0 a) '()
      (let ((b (logxor 1 (logand 7 a))))
        (append (list (logand 7 (logxor 4 (logxor b (ash a (* -1 b))))))
          (fast-actual-run-simple (ash a -3))))))

(defun with-occasional-print (num)
  (progn
   (if (= 0 (mod num 1000000)) (format t "~d~%" num))
   num))

(defun do-part-2-actual-step (a-trial list-goal)
  (if (equal list-goal (fast-actual-run-simple a-trial))
      a-trial
      (do-part-2-actual-step (1+ (with-occasional-print a-trial)) list-goal)))

(defun do-part-2-actual (list-goal &optional (subseq-start 0))
  (if (= subseq-start (1- (length list-goal)))
      (do-part-2-actual-step 0 (subseq list-goal subseq-start))
      (do-part-2-actual-step (ash (do-part-2-actual list-goal (1+ subseq-start)) 3) (subseq list-goal subseq-start))))
;do part 2 actual for sublist on end. Then take (ash prev_num 3) as next start)

(defun test-prog ()
  (make-instance 'Program
    :A 729
    :B 0
    :C 0
    :program '(0 1 5 4 3 0)
    :ip 0
    :output '()))

;117440
(defun test-prog-2 ()
  (make-instance 'Program
    :A 2024
    :B 0
    :C 0
    :program '(0 3 5 4 3 0)
    :ip 0
    :output '()))

(defun combo (operand prg)
  (case operand
    (0 0)
    (1 1)
    (2 2)
    (3 3)
    (4 (A prg))
    (5 (B prg))
    (6 (C prg))))

(defun operand (prg)
  (nth (1+ (ip prg)) (program prg)))

(defun opc (prg)
  (nth (ip prg) (program prg)))

(defun mod8 (num)
  (logand num 7))

(prgfun adv A (ash (A prg) (* -1 (combo (operand prg) prg))))
(prgfun bdv B (ash (A prg) (* -1 (combo (operand prg) prg))))
(prgfun cdv C (ash (A prg) (* -1 (combo (operand prg) prg))))

(prgfun bxl B (logxor (B prg) (operand prg)))
(prgfun bxc B (logxor (B prg) (C prg)))

(prgfun bst B (mod8 (combo (operand prg) prg)))
(prgfun out output (append (output prg) (list (mod8 (combo (operand prg) prg)))))

(defun jnz (prg)
  (setf (ip prg)
    (if (= 0 (A prg))
        (+ 2 (ip prg))
        (operand prg))))


(defun next-op (prg)
  (let ((opc (nth (ip prg) (program prg))))
    (case opc
      (0 #'adv)
      (1 #'bxl)
      (2 #'bst)
      (3 #'jnz)
      (4 #'bxc)
      (5 #'out)
      (6 #'bdv)
      (7 #'cdv))))

(defun do-next-op (prg)
  (apply (next-op prg) (list prg)))

(defun format-int-lst (lst)
  (with-output-to-string (s)
    (loop for num in lst
          for first = t then nil
          do (unless first (princ "," s))
            (princ num s))))

(defun run (prg)
  (if (>= (ip prg) (1- (length (program prg))))
      (output prg)
      (progn
       (do-next-op prg)
       (print-val (A prg))
       (print-val (B prg))
       (print-val (C prg))
       (print-val "")
       (run prg))))

(defun do-part-1 (prg)
  (format-int-lst (run prg)))


(defun run-with-a-check (prg a-replacement)
  (progn
   (setf (A prg) a-replacement)
   (labels ((helper ()
                    (if (or
                         (>= (ip prg) (1- (length (program prg))))
                         (and (> (length (output prg)) 0) (not (equal (subseq (program prg) 0 (length (output prg))) (output prg)))))
                        (output prg)
                        (progn
                         (do-next-op prg)
                         (helper)))))
     (helper))))

(defun a-replacement-gives-itself (prg-fn a-replacement)
  (let ((prg (funcall prg-fn)))
    (equal (program prg) (run-with-a-check prg a-replacement))))


(defun find-self-creating-a (prg-fn &optional (start-val 0))
  (let ((a-replacement start-val))
    (loop while (not (a-replacement-gives-itself prg-fn a-replacement))
          do (incf a-replacement)
            (if (= 0 (mod a-replacement 1000000)) (format t "~d~%" a-replacement)))
    a-replacement))

(defun do-part-2 (prog-fn &optional (start-val 0))
  (find-self-creating-a prog-fn start-val))

;*****
;Tests
;*****

(defparameter prg (test-prog))
(adv prg)
(assert (= 364 (A prg)))

(defparameter prg (test-prog))
(bdv prg)
(assert (= 364 (B prg)))

(defparameter prg (test-prog))
(cdv prg)
(assert (= 364 (C prg)))

(defparameter prg (test-prog))
(bxl prg)
(assert (= 1 (B prg)))

(defparameter prg (test-prog))
(bst prg)
(assert (= 1 (B prg)))

(defparameter prg (test-prog))
(out prg)
(assert (equal '(1) (output prg)))

(defparameter prg (test-prog))
(jnz prg)
(assert (= 1 (ip prg)))
(setf (A prg) 0)
(jnz prg)
(assert (= 3 (ip prg)))

(assert (equal "4,6,3,5,6,3,5,2,1,0" (do-part-1 (test-prog))))
(assert (= 117440 (do-part-2 #'test-prog-2 12034)))

;Maybe combine prog into a single instruction?
; Or stop sooner if first val does not match (up to 9 times faster)

(assert (= 59397658 (do-part-2-actual '(4 6 1 4 2 1 3 1 6))))

;latest brute force 95145000000