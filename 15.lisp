;; *******
;; INPUT
;; *******

(defparameter actual-grid
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"15_grid.txt")))

(defparameter actual-moves
              (uiop:read-file-string
                #p"15_moves.txt"))

(defparameter test-grid
              (list
               "##########"
               "#..O..O.O#"
               "#......O.#"
               "#.OO..O.O#"
               "#..O@..O.#"
               "#O#..O...#"
               "#O..O..O.#"
               "#.OO.O.OO#"
               "#....O...#"
               "##########"))

(defparameter test-moves "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")


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


(defparameter outside-grid-val "#")

;; *******
;; Parsing
;; *******


;; *******
;; UTILITY
;; *******

(defun get-coord (coords grid)
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

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))

(defun add-coords (c1 c2)
  (mapcar #'+ c1 c2))

(defun direction-from-char (c)
  (case c
    (#\v DOWN)
    (#\^ UP)
    (#\> RIGHT)
    (#\< LEFT)))

(defun get-next-coord (coords c) (mapcar #'+ coords (direction-from-char c)))

(defun get-val-in-dir (coords c grid)
  (get-coord (get-next-coord coords (direction-from-char c)) grid))

(defun change-coord (coord new-val grid)
  (let* ((x (first coord))
         (y (second coord))
         (line (nth y grid)))
    (append
      (subseq grid 0 y)
      (list (concatenate 'string (subseq line 0 x) (list new-val) (subseq line (+ x 1))))
      (subseq grid (+ y 1)))))

;; ********
;; SOLUTION
;; ********

(defun incr-coords (coords grid)
  (let ((x (first coords))
        (y (second coords)))
    (cond
     ((>= y (length grid))
       (error "Incr Coords Out of Bounds ~A ~A" x y))
     ((< x (length (nth y grid)))
       (list (+ x 1) y))
     (t
       (list 0 (+ y 1))))))

(defun get-start-position (grid &optional (start-coords '(0 0)))
  (let ((x (first start-coords))
        (y (second start-coords)))
    (if (string= "@" (get-coord (list x y) grid))
        (list x y)
        (let ((next-coords (incr-coords (list x y) grid)))
          (get-start-position grid next-coords)))))

; (defun get-coord-range-vertical (x y-start y-end)
;   (maprange (lambda (num) (list x (+ num (min y-end y-start)))) (1+ (abs (- y-end y-start)))))

; (defun get-coord-range-horizontal (y x-start x-end)
;   (maprange (lambda (num) (list (+ num (min x-end x-start)) y)) (1+ (abs (- x-end x-start)))))

; (defun get-coord-range (coord dir-c distance)
;   (if (= 0 distance) (list coord)
;       (cons coord (get-coord-range (get-next-coord coord dir-c) dir-c (1- distance)))))

(defun is-touching-wall (coord dir-c grid)
  (let ((val (get-coord coord grid)))
    (cond ((equal (char "#" 0) val) t)
          ((equal (char "." 0) val) nil)
          (t (is-touching-wall (get-next-coord coord dir-c) dir-c grid)))))

(defun move-boxes (coord dir-c grid &optional (prev-val (char "." 0)))
  (let* ((new-coord (get-next-coord coord dir-c))
         (this-val (get-coord coord grid))
         (next-val (get-coord new-coord grid))
         (new-grid (change-coord coord prev-val grid)))
    (cond ((equal (char "." 0) next-val) (change-coord new-coord this-val new-grid))
          ((equal (char "#" 0) next-val) grid)
          (t (move-boxes new-coord dir-c new-grid this-val)))))


(defun move (coord dir-c-list grid)
  (if (= 0 (length dir-c-list)) (list coord grid)
      (let ((dir-c (char dir-c-list 0)))
        (if (is-touching-wall coord dir-c grid)
            (move coord (subseq dir-c-list 1) grid)
            (move (get-next-coord coord dir-c) (subseq dir-c-list 1) (move-boxes coord dir-c grid))))))


(defun calc-gps (coord)
  (+ (first coord) (* 100 (second coord))))

(assert (= 104 (calc-gps '(4 1))))

(defun calc-gps-sum (grid)
  (apply #'+ (mapcar #'calc-gps
                 (remove-if-not
                     (lambda (coord) (equal (char "O" 0) (get-coord coord grid)))
                     (all-coords grid)))))

(defun do-part-1 (grid moves)
  (calc-gps-sum (second (move (get-start-position grid) moves grid))))

(assert (= 10092 (do-part-1 test-grid test-moves)))