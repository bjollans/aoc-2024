(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"20.txt")))

(defparameter test-input
              (list
               "###############"
               "#...#...#.....#"
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
               "#.#.#.#.#.#.###"
               "#...#...#...###"
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