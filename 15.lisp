;; *******
;; INPUT
;; *******

(defparameter actual-grid
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"15_grid.txt")))

(defparameter actual-grid-2
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"15_grid_part2.txt")))

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

(defparameter test-grid-2
              (list
               "####################"
               "##....[]....[]..[]##"
               "##............[]..##"
               "##..[][]....[]..[]##"
               "##....[]@.....[]..##"
               "##[]##....[]......##"
               "##[]....[]....[]..##"
               "##..[][]..[]..[][]##"
               "##........[]......##"
               "####################"))

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

(defparameter RIGHT_C (char ">" 0))
(defparameter LEFT_C (char "<" 0))
(defparameter UP_C (char "^" 0))
(defparameter DOWN_C (char "v" 0))

(defparameter EMPTY_C (char "." 0))
(defparameter WALL_C (char "#" 0))


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

(defparameter box-values (list (char "O" 0) (char "]" 0) (char "[" 0)))

(defun is-box (coord grid)
  (member (get-coord coord grid) box-values))

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


(defun calc-gps (coord)
  (+ (first coord) (* 100 (second coord))))

(assert (= 104 (calc-gps '(4 1))))

(defun calc-gps-sum (grid)
  (apply #'+ (mapcar #'calc-gps
                 (remove-if-not
                     (lambda (coord) (equal #\[ (get-coord coord grid)))
                     (all-coords grid)))))

(defun get-other-box-coord (coord grid)
  (let ((val (get-coord coord grid)))
    (cond
     ((equal val (char "O" 0)) nil)
     ((equal val (char "]" 0)) (get-next-coord coord LEFT_C))
     ((equal val (char "[" 0)) (get-next-coord coord RIGHT_C)))))


(defun is-box-touching-wall-in-dir (coord dir-c grid)
  (cond
   ((member dir-c (list RIGHT_C LEFT_C)) (is-touching-wall coord dir-c grid))
   ((equal (get-coord coord grid) EMPTY_C) nil)
   ((equal (get-coord coord grid) WALL_C) t)
   (t (let ((other-box-coord (get-other-box-coord coord grid)))
        (or
         (is-box-touching-wall-in-dir (get-next-coord coord dir-c) dir-c grid)
         (if other-box-coord (is-box-touching-wall-in-dir (get-next-coord other-box-coord dir-c) dir-c grid)))))))

(assert (is-box-touching-wall-in-dir '(6 4) UP_C (list
                                                  "####################"
                                                  "##[]........[]..[]##"
                                                  "##.[].........[]..##"
                                                  "##..[][]....[]..[]##"
                                                  "##...[]@......[]..##"
                                                  "##[]##....[]......##"
                                                  "##[]....[]....[]..##"
                                                  "##..[][]..[]..[][]##"
                                                  "##........[]......##"
                                                  "####################")))

(assert (not (is-box-touching-wall-in-dir '(6 4) UP_C (list
                                                       "####################"
                                                       "##[]........[]..[]##"
                                                       "##............[]..##"
                                                       "##..[][]....[]..[]##"
                                                       "##...[]@......[]..##"
                                                       "##[]##....[]......##"
                                                       "##[]....[]....[]..##"
                                                       "##..[][]..[]..[][]##"
                                                       "##........[]......##"
                                                       "####################"))))


(defun move-boxes (coord dir-c grid &optional (prev-val (char "." 0)))
  (let* ((new-coord (get-next-coord coord dir-c))
         (this-val (get-coord coord grid))
         (next-val (get-coord new-coord grid))
         (new-grid (change-coord coord prev-val grid)))
    (cond ((equal (char "." 0) next-val) (change-coord new-coord this-val new-grid))
          ((equal (char "#" 0) next-val) grid)
          (t (move-boxes new-coord dir-c new-grid this-val)))))

; get all coords of boxes
; Replace all with dots
; redraw all one higher

; (defun move-double-boxes (coord dir-c grid &optional (prev-val (char "." 0)))
;   (let* ((new-coord (get-next-coord coord dir-c))
;          (this-val (get-coord coord grid))
;          (next-val (get-coord new-coord grid))
;          (new-grid (change-coord coord prev-val grid))
;          (other-box-coord (get-other-box-coord coord grid)))
;     (cond ((and (equal (char "." 0) next-val) (equal (char "." 0) next-val)) (change-coord new-coord this-val new-grid))
;           ((equal (char "#" 0) next-val) grid)
;           (t (move-boxes new-coord dir-c new-grid this-val)))))

(defun get-connected-box-coords (coord dir-c grid &optional (vals-so-far '()))
  (if (member coord vals-so-far :test #'equal)
      '()
      (let* ((new-coord (get-next-coord coord dir-c))
             (next-val (get-coord new-coord grid))
             (this-val (get-coord coord grid))
             (other-box-coord (get-other-box-coord coord grid)))
        (setf vals-so-far (append vals-so-far (list coord)))
        (unique (append (list (list coord this-val))
                  (if (member next-val (list (char "[" 0) (char "]" 0)) :test #'equal) (get-connected-box-coords new-coord dir-c grid vals-so-far))
                  (if other-box-coord (get-connected-box-coords other-box-coord dir-c grid vals-so-far)))))))

(defun move-box-coords (box-coords dir-c)
  (mapcar
      (lambda (box-coord)
        (list (get-next-coord (first box-coord) dir-c) (second box-coord)))
      box-coords))

(defun put-dots-at-box-coords (box-coords grid)
  (if (= 0 (length box-coords)) grid
      (put-dots-at-box-coords (cdr box-coords) (change-coord (first (first box-coords)) EMPTY_C grid))))

(defun put-box-coords (box-coords grid)
  (if (= 0 (length box-coords)) grid
      (put-box-coords (cdr box-coords) (change-coord (first (first box-coords)) (second (first box-coords)) grid))))

(defun move-double-boxes (coord dir-c grid)
  (put-box-coords (move-box-coords (get-connected-box-coords coord dir-c grid) dir-c) (put-dots-at-box-coords (get-connected-box-coords coord dir-c grid) grid)))


(defun move (coord dir-c-list grid)
  (if (= 0 (length dir-c-list)) (list coord grid)
      (let ((dir-c (char dir-c-list 0)))
        (progn
         (print-grid grid)
         (if (is-box-touching-wall-in-dir (get-next-coord coord dir-c) dir-c grid)
             (move coord (subseq dir-c-list 1) grid)
             (move (get-next-coord coord dir-c) (subseq dir-c-list 1) (move-double-boxes coord dir-c grid)))))))

(defun print-grid (grid)
  (dolist (line grid)
    (format t "~a~%" line)))


(defun do-part-1 (grid moves)
  (calc-gps-sum (second (move (get-start-position grid) moves grid))))


; (assert (= 10092 (do-part-1 test-grid test-moves)))