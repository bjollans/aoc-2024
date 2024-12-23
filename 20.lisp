(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"20.txt")))

(defparameter test-input
              (list
               "###############"
               "#FG.#...#.....#"
               "#.#.#.#.#.###.#"
               "#S#...#.#.#...#"
               "#######.#.#.###"
               "#######.#.#...#"
               "#######.#.###.#"
               "###..E#...#...#"
               "###.#######.###"
               "#...###...#...#"
               "#.#####.#.###.#"
               "#.#...#.#.#...#"
               "#.#.#.#.#G#.###"
               "#...#...#F..###"
               "###############"))

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))
(defparameter ALL_DIRS (list UP DOWN LEFT RIGHT))

(defparameter WALL #\#)
(defparameter PATH #\.)
(defparameter END #\E)
(defparameter START #\S)

(defmacro print-val (expr)
  `(let ((res ,expr))
     (progn
      (format t "~a: ~a" ',expr ,'res)
      (terpri)
      res)))

(defmacro xlambda (expr)
  `(lambda (x) ,expr))

(defmacro mapx (lst lambda-expr)
  `(remove nil (mapcar (xlambda ,lambda-expr) ,lst)))

(defun all-coords (grid &optional (coords '(0 0)))
  (let* ((given-x (first coords))
         (given-y (second coords))
         (out-of-bounds (>= given-x (length (nth given-y grid))))
         (x (if out-of-bounds 0 given-x))
         (y (if out-of-bounds (+ 1 given-y) given-y)))
    (if (>= y (length grid))
        '()
        (cons (list x y) (all-coords grid (list (+ 1 x) y))))))

(defun get-coords (coords grid)
  (let ((x (first coords))
        (y (second coords)))
    (if (or
         (< y 0)
         (< x 0)
         (>= y (length grid))
         (>= x (length (nth y grid))))
        WALL
        (char (nth y grid) x))))


(defun get-position-of-char (grid c)
  (remove-if-not (lambda (coords) (equal c (get-coords coords grid))) (all-coords grid)))


(defun get-next-coord (coords direction &optional (distance 1)) (mapcar #'+ coords (mapx direction (* distance x))))

(defun get-val-in-dir (coords direction grid &optional (distance 1))
  (get-coords (get-next-coord coords direction distance) grid))

(defun can-move (coords dir grid)
  (not (equal WALL (get-val-in-dir coords dir grid))))

(defun move (coords dir grid)
  (if (can-move coords dir grid)
      (get-next-coord coords dir)
      coords))

(defun get-score-for-coord (coords score-memory)
  (gethash coords score-memory))

(defun set-score-for-coord (coords score score-memory)
  (setf (gethash coords score-memory) score))

(defmacro memfun (name args expr)
  `(defun ,name ,args
     (let ((memory (make-hash-table :test #'equal)))
       (labels ((call-self ,args ,expr)) (call-self ,@args)))))

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

(defun search-paths (coords-q grid score-memory)
  (if (and coords-q (not (equal '() coords-q)))
      (let* ((curr_coord (first coords-q))
             (improved-coords (get-and-improve-improvable-neighbours curr_coord grid score-memory)))
        (search-paths
          (if improved-coords
              (append (cdr coords-q) improved-coords)
              (cdr coords-q))
          grid score-memory))))


(defun calculate-scores (grid)
  (let ((score-mem (make-hash-table :test #'equal))
        (start-pos-list (get-position-of-char grid START)))
    (set-score-for-coord (first start-pos-list) 0 score-mem)
    (search-paths start-pos-list grid score-mem)
    score-mem))

(assert (= 84 (get-score-for-coord (first (get-position-of-char test-input END)) (calculate-scores test-input))))

(defun count-cheats-with-gain (grid picosecond-gain &optional (test-fn #'>=))
  (let ((score-mem (calculate-scores grid)))
    (apply #'+
        (mapcar
            (lambda (coords)
              (let ((coord-score (gethash coords score-mem)))
                (if (not coord-score) 0
                    (apply #'+
                        (mapx ALL_DIRS
                              (let* ((neighbour-score (gethash (get-next-coord coords x 2) score-mem)))
                                (if (and
                                     neighbour-score
                                     (apply test-fn (list (- neighbour-score coord-score 2) picosecond-gain)))
                                    1
                                    0)))))))
            (all-coords grid)))))

(assert (= 14 (count-cheats-with-gain test-input 2 #'=)))
(assert (= 14 (count-cheats-with-gain test-input 4 #'=)))
(assert (= 2 (count-cheats-with-gain test-input 6 #'=)))
(assert (= 4 (count-cheats-with-gain test-input 8 #'=)))
(assert (= 2 (count-cheats-with-gain test-input 10 #'=)))
(assert (= 3 (count-cheats-with-gain test-input 12 #'=)))
(assert (= 1 (count-cheats-with-gain test-input 20 #'=)))
(assert (= 1 (count-cheats-with-gain test-input 36 #'=)))
(assert (= 1 (count-cheats-with-gain test-input 38 #'=)))
(assert (= 1 (count-cheats-with-gain test-input 40 #'=)))
(assert (= 1 (count-cheats-with-gain test-input 64 #'=)))


(defun remove-out-of-bounds (coords-list grid)
  (remove-if (lambda (coords)
               (let ((x (first coords))
                     (y (second coords)))
                 (or
                  (< y 0)
                  (< x 0)
                  (>= y (length grid))
                  (>= x (length (nth y grid))))))
      coords-list))


(defun coord-diff (c1 c2)
  (+ (abs (- (first c1) (first c2))) (abs (- (second c1) (second c2)))))

(defun are-in-radius (c1 c2 r)
  (<= (coord-diff c1 c2) r))

(defun get-radius-coords (coords grid)
  (remove-out-of-bounds
    (let ((coords-list (list))
          (coords-x (first coords))
          (coords-y (second coords)))
      (dotimes (x 45)
        (dotimes (y 45)
          (let ((new-coords (list (+ (- coords-x 24) x) (+ (- coords-y 24) y))))
            (if (are-in-radius coords new-coords 20)
                (setf coords-list
                  (append
                    coords-list
                    (list new-coords)))))))
      coords-list)
    grid))

(defun score-diff (c1 c2 score-mem)
  (- (gethash c2 score-mem) (gethash c1 score-mem)))

;Distance between starts is just dots and distance between ends is also just dots
; -> score difference is same as coord difference => distance is only dots
(defun is-the-same-cheat-according-to-2-i-think (c1-start c1-end c2-start c2-end score-mem)
  (and
   (= (score-diff c1-start c2-start score-mem) (coord-diff c1-start c2-start))
   (= (score-diff (print-val c1-end) (print-val c2-end) score-mem) (coord-diff c1-end c2-end))))


(defun special-unique-for-cheat-2 (lst score-mem)
  (if (equal '() lst)
      '()
      (append
        (if (some
                (xlambda (is-the-same-cheat-according-to-2-i-think (first (first lst)) (second (first lst)) (first x) (second x) score-mem))
                (cdr lst)) '() (list (first lst)))
        (special-unique-for-cheat-2 (cdr lst) score-mem))))


(defun count-cheats-with-gain-2-list (grid picosecond-gain &optional (test-fn #'>=))
  (let ((score-mem (calculate-scores grid)))
    (mapcan
        (lambda (coords)
          (let ((coord-score (gethash coords score-mem)))
            (if coord-score
                (mapx (get-radius-coords coords grid)
                      (let* ((neighbour-score (gethash x score-mem)))
                        (if (and
                             neighbour-score
                             (apply test-fn (list (- neighbour-score coord-score (coord-diff x coords)) picosecond-gain)))
                            (list coords x)))))))
        (all-coords grid))))

(defun count-cheats-with-gain-2 (grid picosecond-gain &optional (test-fn #'>=))
  (length (count-cheats-with-gain-2-list grid picosecond-gain test-fn)))


(special-unique-for-cheat-2 (count-cheats-with-gain-2-list test-input 76 #'=) (calculate-scores test-input))

(assert (= 32 (count-cheats-with-gain-2 test-input 50 #'=)))
(assert (= 31 (count-cheats-with-gain-2 test-input 52 #'=)))
(assert (= 29 (count-cheats-with-gain-2 test-input 54 #'=)))
(assert (= 39 (count-cheats-with-gain-2 test-input 56 #'=)))
(assert (= 25 (count-cheats-with-gain-2 test-input 58 #'=)))
(assert (= 23 (count-cheats-with-gain-2 test-input 60 #'=)))
(assert (= 20 (count-cheats-with-gain-2 test-input 62 #'=)))
(assert (= 19 (count-cheats-with-gain-2 test-input 64 #'=)))
(assert (= 12 (count-cheats-with-gain-2 test-input 66 #'=)))
(assert (= 14 (count-cheats-with-gain-2 test-input 68 #'=)))
(assert (= 12 (count-cheats-with-gain-2 test-input 70 #'=)))
(assert (= 22 (count-cheats-with-gain-2 test-input 72 #'=)))
(assert (= 4 (count-cheats-with-gain-2 test-input 74 #'=)))
(assert (= 3 (count-cheats-with-gain-2 test-input 76 #'=)))