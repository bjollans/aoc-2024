;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"16.txt")))

(defparameter test-input
              (list
               "###############";0
               "#.......#....E#";1
               "#.#.###.#.###.#";2
               "#.....#.#...#.#";3
               "#.###.#####.#.#";4
               "#.#.#.......#.#";5
               "#.#.#####.###.#";6
               "#...........#.#";7
               "###.#.#####.#.#";8
               "#...#.....#.#.#";9
               "#.#.#.###.#.#.#";10
               "#.....#...#.#.#";11
               "#.###.#.#.#.#.#";12
               "#S..#.....#...#";13
               "###############"))


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


(defparameter outside-grid-val ".")

;; *******
;; UTILITY
;; *******

(defun get-coords (coords grid)
  (let ((x (first coords))
        (y (second coords)))
    (if (or
         (< y 0)
         (< x 0)
         (>= y (length grid))
         (>= x (length (nth y grid))))
        outside-grid-val
        (char (nth y grid) x))))

(defun all-coords (grid &optional (coords '(0 0)))
  (let* ((given-x (first coords))
         (given-y (second coords))
         (out-of-bounds (>= given-x (length (nth given-y grid))))
         (x (if out-of-bounds 0 given-x))
         (y (if out-of-bounds (+ 1 given-y) given-y)))
    (if (>= y (length grid))
        '()
        (cons (list x y) (all-coords grid (list (+ 1 x) y))))))

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

(defun get-position-of-char (grid c)
  (remove-if-not (lambda (coords) (equal c (get-coords coords grid))) (all-coords grid)))

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))
(defparameter ALL_DIRS (list UP DOWN LEFT RIGHT))

(defparameter WALL #\#)
(defparameter PATH #\.)
(defparameter END #\E)
(defparameter START #\S)

(defun add-coords (c1 c2)
  (mapcar #'+ c1 c2))

(defun get-next-coord (coords direction) (mapcar #'+ coords direction))

(defun get-val-in-dir (coords direction grid)
  (get-coords (get-next-coord coords direction) grid))

;; ********
;; Solution
;; ********

(defun can-move (coords dir grid)
  (not (equal WALL (get-val-in-dir coords dir grid))))

(defun move (coords dir grid)
  (if (can-move coords dir grid)
      (get-next-coord coords dir)
      coords))

; keep track of all coords their lowest score (and maybe with the best path to them, or just the coord that came before)
; I add each coordinate to that score counter. If the new score is lower, I add it to the re-evaluation queue
; when evaluating, I calculate all scores for all neighbours
; when the queue is empty I check the score for the End value


(defun get-score-for-coord (coords dir score-memory)
  (gethash (list coords dir) score-memory))

(defun set-score-for-coord (coords dir score score-memory)
  (setf (gethash (list coords dir) score-memory) score))


;mapcar all directions if can move coords' score plus 1001, direction only plus 1)
(defun get-improvable-neighbours (coords curr_dir grid score-memory)
  (let ((dirs-without-walls (remove-if-not
                                (lambda (dir) (can-move coords dir grid))
                                ALL_DIRS))
        (coords-score (get-score-for-coord coords curr_dir score-memory)))
    (remove nil
        (mapcar
            (lambda (dir)
              (let* ((new-coord (get-next-coord coords dir))
                     (old-score (get-score-for-coord new-coord dir score-memory))
                     (new-score (+ coords-score
                                   (if (equal dir curr_dir)
                                       1
                                       1001))))
                (if (or (not old-score) (< new-score old-score))
                    (progn
                     (set-score-for-coord new-coord dir new-score score-memory)
                     (list new-coord dir))
                    nil)))
            dirs-without-walls))))

(defparameter fake-score-mem (make-hash-table :test #'equal))
(set-score-for-coord '(1 13) RIGHT 0 fake-score-mem)
(assert (equal (list (list (list 1 12) UP) (list (list 2 13) RIGHT)) (get-improvable-neighbours '(1 13) RIGHT test-input fake-score-mem)))
(assert (= 1 (get-score-for-coord '(2 13) RIGHT fake-score-mem)))
(assert (= 1001 (get-score-for-coord '(1 12) UP fake-score-mem)))
(assert (not (get-score-for-coord '(3 13) RIGHT fake-score-mem)))
(assert (equal (list (list (list 1 13) LEFT) (list (list 3 13) RIGHT)) (get-improvable-neighbours '(2 13) RIGHT test-input fake-score-mem)))
(assert (= 2 (get-score-for-coord '(3 13) RIGHT fake-score-mem)))

(defun search-paths (coords-q grid score-memory)
  (if (and coords-q (not (equal '() coords-q)))
      (let* ((curr_coord (first (first coords-q)))
             (curr_dir (second (first coords-q)))
             (improved-coords (get-improvable-neighbours curr_coord curr_dir grid score-memory)))
        (search-paths
          (if improved-coords
              (append (cdr coords-q) improved-coords)
              (cdr coords-q))
          grid score-memory))))

(defparameter test-score-mem (make-hash-table :test #'equal))
(set-score-for-coord '(1 13) RIGHT 0 test-score-mem)

(defun get-min-for-coords (coords score-memory)
  (apply #'min (remove nil (mapcar (lambda (dir) (get-score-for-coord coords dir score-memory)) ALL_DIRS))))

(defun do-part-1 (grid)
  (let ((start-pos (first (get-position-of-char grid #\S)))
        (end-pos (first (get-position-of-char grid #\E)))
        (score-memory (make-hash-table :test #'equal)))
    (set-score-for-coord start-pos RIGHT 0 score-memory)
    (search-paths (list (list start-pos RIGHT)) grid score-memory)
    (get-min-for-coords end-pos score-memory)))

(assert (= 7036 (do-part-1 test-input)))