(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"8.txt")))

(defparameter test-input
              (list
               "............"
               "........0..."
               ".....0......"
               ".......0...."
               "....0......."
               "......A....."
               "............"
               "............"
               "........A..."
               ".........A.."
               "............"
               "............"))

(defmacro partial (func &rest args)
  `(lambda (x) (funcall ,func x ,@args)))

(defmacro maprange (func amount)
  `(let (value)
     (dotimes (number ,amount)
       (setq value (cons (funcall ,func (+ number 1)) value)))
     value))

(defparameter outside-grid-val "-")
(defparameter default-grid-val ".")

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

(defun count-unique (lst)
  (if (equal '() lst)
      0
      (+
       (if (member (car lst) (cdr lst) :test #'equal) 0 1)
       (count-unique (cdr lst)))))

(defun all-coords (input &optional (coords '(0 0)))
  (let* ((given-x (first coords))
         (given-y (second coords))
         (out-of-bounds (>= given-x (length (nth given-y input))))
         (x (if out-of-bounds 0 given-x))
         (y (if out-of-bounds (+ 1 given-y) given-y)))
    (if (>= y (length input))
        '()
        (cons (list x y) (all-coords input (list (+ 1 x) y))))))

(defun get-antenna-coords (input)
  (remove-if
      (lambda (x) (string= default-grid-val (coord x input)))
      (all-coords input)))

(defun coord-diff (c1 c2)
  (mapcar #'- c1 c2))

(defun coord-sum (c1 c2)
  (mapcar #'+ c1 c2))

(defun coord-mul (c1 scalar)
  (mapcar (lambda (x) (* x scalar)) c1))

(defun antinode-positions (c1 c2 &optional (nodes-per-side 1))
  (let ((diff (coord-diff c1 c2)))
    (append
      (maprange (lambda (x) (coord-sum c1 (coord-mul diff x))) nodes-per-side)
      (maprange (lambda (x) (coord-diff c2 (coord-mul diff x))) nodes-per-side))))

(assert (equal (antinode-positions (list 1 2) (list 2 2)) (list '(0 2) '(3 2))))

(defun get-antinodes-between-antennas (antenna-coord-1 antenna-coord-2 grid nodes-per-side)
  (if (or
       (not (string= (coord antenna-coord-1 grid) (coord antenna-coord-2 grid)))
       (equal antenna-coord-1 antenna-coord-2))
      '()
      (antinode-positions antenna-coord-1 antenna-coord-2 nodes-per-side)))

(defun get-antinodes-for-antenna (antenna-coord grid nodes-per-side)
  (let ((all-antennas (get-antenna-coords grid)))
    (mapcan
        (partial #'get-antinodes-between-antennas antenna-coord grid nodes-per-side)
        all-antennas)))

(defun get-all-antinodes (grid &optional (nodes-per-side 1))
  (let ((all-antennas (get-antenna-coords grid)))
    (mapcan
        (partial #'get-antinodes-for-antenna grid nodes-per-side)
        all-antennas)))

(defun is-in-grid (c grid)
  (not (string= outside-grid-val (coord c grid))))

(assert (not (is-in-grid '(100 0) test-input)))
(assert (is-in-grid '(11 11) test-input))

(defun do-part-1 (grid)
  (count-unique
    (remove-if-not
        (partial #'is-in-grid grid)
        (get-all-antinodes grid))))

(assert (= 14 (do-part-1 test-input)))


(defun do-part-2 (grid)
  (count-unique
    (remove-if-not
        (partial #'is-in-grid grid)
        (append
          (get-antenna-coords grid)
          (get-all-antinodes grid (* (length grid) 2))))))


(assert (= 34 (do-part-2 test-input)))