;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"18.txt")))

(defparameter test-input
              (list
               "5,4"
               "4,2"
               "4,5"
               "3,0"
               "2,1"
               "6,3"
               "2,4"
               "1,5"
               "0,6"
               "3,3"
               "2,6"
               "5,1"
               "1,2"
               "5,5"
               "2,5"
               "6,5"
               "1,4"
               "0,4"
               "6,4"
               "1,1"
               "6,1"
               "1,0"
               "0,5"
               "1,6"
               "2,0"))


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

(defmacro xlambda (expr)
  `(lambda (x) ,expr))

(defmacro mapx (lst lambda-expr)
  `(remove nil (mapcar (xlambda ,lambda-expr) ,lst)))

(defparameter empty #\.)
(defparameter wall #\#)

;; *******
;; UTILITY
;; *******

(defun get-coords (coords grid)
  (let ((x (first coords))
        (y (second coords)))
    (nth x (nth y grid))))

(defun unique (lst)
  (if (equal '() lst)
      '()
      (append
        (if (member (car lst) (cdr lst) :test #'equal) '() (list (first lst)))
        (unique (cdr lst)))))

(defun get-position-of-char (grid c)
  (remove-if-not (lambda (coords) (equal c (get-coords coords grid))) (all-coords grid)))

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))
(defparameter ALL_DIRS (list UP DOWN LEFT RIGHT))

(defun add-coords (c1 c2)
  (mapcar #'+ c1 c2))

(defun get-next-coord (coords direction) (mapcar #'+ coords direction))

(defun get-val-in-dir (coords direction grid)
  (get-coords (get-next-coord coords direction) grid))

(defun change-coord (coord new-val grid)
  (let* ((x (first coord))
         (y (second coord))
         (line (nth y grid)))
    (append
      (subseq grid 0 y)
      (list (append (subseq line 0 x) (list new-val) (subseq line (+ x 1))))
      (subseq grid (+ y 1)))))

(defun print-grid (grid)
  (dolist (line grid)
    (format t "~a~%" line)))

(defun make-grid (width height)
  (loop for y in (make-list height)
        :collect (make-list width :initial-element #\.)))

;; ********
;; Solution
;; ********

(defun can-move (coords dir grid)
  (not (or
        (let* ((next-coords (get-next-coord coords dir))
               (x (first next-coords))
               (y (second next-coords)))
          (or (< x 0) (< y 0) (>= y (length grid)) (>= x (length (nth y grid)))))
        (equal WALL (get-val-in-dir coords dir grid)))))

(defun move (coords dir grid)
  (if (can-move coords dir grid)
      (get-next-coord coords dir)
      coords))


(defun get-score-for-coord (coords score-memory)
  (gethash coords score-memory))

(defun set-score-for-coord (coords score score-memory)
  (setf (gethash coords score-memory) score))

(defun parse-input-line-to-coord (input-line)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, input-line)))

(defun fill-grid (inputs grid &optional (input-limit 1000))
  (if (or (= 0 input-limit) (= 0 (length inputs))) grid
      (fill-grid
        (cdr inputs)
        (change-coord (parse-input-line-to-coord (first inputs)) #\# grid)
        (1- input-limit))))


(defun get-and-improve-improvable-neighbours (coords grid score-memory)
  (let ((movable-dirs (remove-if-not
                          (xlambda (can-move coords x grid))
                          ALL_DIRS))
        (coords-score (get-score-for-coord coords score-memory)))
    (remove nil
        (mapx movable-dirs
              (let* ((new-coord (get-next-coord coords x))
                     (old-score (get-score-for-coord new-coord score-memory))
                     (new-score (1+ coords-score)))
                (if (or (not old-score) (< new-score old-score))
                    (progn
                     (set-score-for-coord new-coord new-score score-memory)
                     new-coord)
                    nil))))))

; (defparameter score-mem (make-hash-table :test #'equal))
; (set-score-for-coord '(0 0) 0 score-mem)
(defparameter test-score-mem (make-hash-table :test #'equal))
(set-score-for-coord '(1 1) 0 test-score-mem)
(set-score-for-coord '(0 0) 0 test-score-mem)
(assert (equal (list '(0 1) '(1 0)) (get-and-improve-improvable-neighbours '(0 0) (make-grid 7 7) test-score-mem)))
(assert (equal (list '(1 2) '(2 1)) (get-and-improve-improvable-neighbours '(1 1) (make-grid 7 7) test-score-mem)))
(assert (equal (list) (get-and-improve-improvable-neighbours '(1 1) (make-grid 7 7) test-score-mem)))

(defun search-paths (coords-q grid score-memory)
  (if (and coords-q (not (equal '() coords-q)))
      (let* ((curr_coord (first coords-q))
             (improved-coords (get-and-improve-improvable-neighbours curr_coord grid score-memory)))
        (search-paths
          (if improved-coords
              (append (cdr coords-q) improved-coords)
              (cdr coords-q))
          grid score-memory))))


(defun do-part-1 (input grid-size input-limit end-pos)
  (let ((score-mem (make-hash-table :test #'equal)))
    (set-score-for-coord '(0 0) 0 score-mem)
    (search-paths (list '(0 0)) (fill-grid input (make-grid (first grid-size) (second grid-size)) input-limit) score-mem)
    (get-score-for-coord end-pos score-mem)))

(assert (= 22 (do-part-1 test-input '(7 7) 12 '(6 6))))

; part 1
; * (defparameter score-mem (make-hash-table :test #'equal))
; SCORE-MEM
; * (set-score-for-coord '(0 0) 0 score-mem)
; 0
; * (search-paths (list '(0 0)) (fill-grid test-input (make-grid 7 7) 12) score-mem)
; NIL
; * (get-score-for-coord '(7 7) score-mem)