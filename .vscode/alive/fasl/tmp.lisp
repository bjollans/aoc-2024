(+ 5 5)
(defun test-fun (x)
  (+ x 5))

(defun asrt (condition descr)
  (if condition
      (format t "AssertionSuccess:~A~%" descr)
      (error "AssertionFailure:~A~%" descr)))

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"6.txt")))

(defparameter test-input
              (list
               "....#....."
               ".........#"
               ".........."
               "..#......."
               ".......#.."
               ".........."
               ".#..^....."
               "........#."
               "#........."
               "......#..."))

(defparameter test-loop-input
              (list
               "....#....."
               ".........#"
               ".........."
               "..#......."
               ".......#.."
               ".........."
               ".#.#^....."
               "........#."
               "#........."
               "......#..."))

(defparameter outside-grid-val "-")

(defun coord (coords grid)
  (let ((x (first coords))
        (y (second coords)))
    (if (or
         (< y 0)
         (< x 0)
         (>= y (length grid))
         (>= x (length (nth y grid))))
        outside-grid-val
        (char (nth y grid) x))))

(assert (string= (coord (list 4 0) test-input) "#"))
(assert (string= (coord (list 4 1) test-input) "."))
(assert (string= (coord (list 5 1) test-input) "."))
(assert (string= (coord (list 4 6) test-input) "^"))
(assert (string= (coord (list -4 0) test-input) outside-grid-val))
(assert (string= (coord (list 40 0) test-input) outside-grid-val))
(assert (string= (coord (list 4 -6) test-input) outside-grid-val))
(assert (string= (coord (list 4 60) test-input) outside-grid-val))

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

(assert (equal (list 0 1) (incr-coords (list 400 0) test-input)))
(assert (equal (list 0 1) (incr-coords (list 10 0) test-input)))
(assert (equal (list 0 1) (incr-coords (list 10 0) test-input)))
(assert (equal (list 1 1) (incr-coords (list 0 1) test-input)))

(defun get-start-position (grid &optional (start-coords '(0 0)))
  (let ((x (first start-coords))
        (y (second start-coords)))
    (if (string= "^" (coord (list x y) grid))
        (list x y)
        (let ((next-coords (incr-coords (list x y) grid)))
          (get-start-position grid next-coords)))))

(assert (equal (list 4 6) (get-start-position test-input)))

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))

(defun get-next-coord (coords direction) (mapcar #'+ coords direction))

(defun turn-right (direction)
  (cond ((equal direction RIGHT) DOWN)
        ((equal direction DOWN) LEFT)
        ((equal direction LEFT) UP)
        ((equal direction UP) RIGHT)
        (t (error "Invalid Direction"))))

(defun get-next-direction (coords direction grid)
  (let ((next-coord (get-next-coord coords direction)))
    (if (string= (coord next-coord grid) "#")
        (get-next-direction coords (turn-right direction) grid)
        direction)))

(assert (equal RIGHT (get-next-direction '(4 1) UP test-input)))
(assert (equal RIGHT (get-next-direction '(4 1) RIGHT test-input)))
(assert (equal RIGHT (get-next-direction '(5 1) RIGHT test-input)))

(defun move (coords direction grid)
  (get-next-coord coords (get-next-direction coords direction grid)))

(assert (equal '(0 1) (move '(0 0) DOWN test-input)))
(assert (equal (move '(4 1) RIGHT test-input) (move '(4 1) UP test-input)))


(defun get-path (coords direction grid &optional (depth 0))
  (if (> depth 10000)
      (list -1)
      (if (string= "-" (coord coords grid))
          '()
          (let* ((next-direction (get-next-direction coords direction grid))
                 (next-coords (get-next-coord coords next-direction)))
            (cons
              next-coords
              (get-path next-coords next-direction grid (+ depth 1)))))))

(defun count-unique (lst)
  (if (equal '() lst)
      0
      (+
       (if (member (car lst) (cdr lst) :test #'equal) 0 1)
       (count-unique (cdr lst)))))

(assert (= 1 (count-unique '('(1 1) '(1 1)))))
(assert (= 2 (count-unique '('(1 1) '(1 1) '(1 2)))))

; (defparameter pth (get-path (get-start-position test-input) UP test-input))

(defun do-part-1 (input)
  (count-unique (get-path (get-start-position input) UP input)))

(defun is-loop (input)
  (if (member -1 (get-path (get-start-position input) UP input)) 1 0))

(assert (= 0 (is-loop test-input)))
(assert (= 1 (is-loop test-loop-input)))

(defun modify-at (str pos new-item type)
  (concatenate type
    (subseq str 0 pos)
    new-item
    (subseq str (+ 1 pos))))

(defun modify-char (str pos new-char)
  (modify-at str pos new-char 'string))

(assert (string= (modify-char "..#.#.." 3 "#") "..###.."))

(defun modify-line (input line-num new-line)
  (modify-at input line-num new-line 'list))

(defun add-obstacle (coords input)
  (let ((x (first coords))
        (y (second coords)))
    (modify-line
      input
      y
      (list (modify-char
              (nth y input)
              x
              "#")))))

(defun is-loop-with-obstace (obstacle-coords input)
  (if (equal obstacle-coords (get-start-position input)) 
      0
      (is-loop (add-obstacle obstacle-coords input))))

(assert (= 0 (is-loop-with-obstace '(0 0) test-input)))
(assert (= 1 (is-loop-with-obstace '(3 6) test-input)))

(defun all-coords (input &optional (coords '(0 0)))
  (let* ((given-x (first coords))
        (given-y (second coords))
         (out-of-bounds (>= given-x (length (nth given-y input))))
         (x (if out-of-bounds 0 given-x))
         (y (if out-of-bounds (+ 1 given-y) given-y)))
    (if (>= y (length input)) 
        '() 
        (cons (list x y) (all-coords input (list (+ 1 x) y))))))

(defun do-part-2 (input)
  (apply #'+ (mapcar (lambda (x) (is-loop-with-obstace x input)) (all-coords input))))

(assert (= 6 (do-part-2 test-input)))