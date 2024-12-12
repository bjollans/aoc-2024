;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"12.txt")))

(defparameter test-input
              (list
               "RRRRIICCFF"
               "RRRRIICCCF"
               "VVRRRCCFFF"
               "VVRCCCJFFF"
               "VVVVCJJCFE"
               "VVIVCCJJEE"
               "VVIIICJJEE"
               "MIIIIIJJEE"
               "MIIISIJEEE"
               "MMMISSJEEE"))


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

(defun get-next-coord (coords direction) (mapcar #'+ coords direction))

(defun get-val-in-dir (coords direction grid)
  (coord (get-next-coord coords direction) grid))

(assert (equal (char "R" 0) (get-val-in-dir '(0 0) DOWN test-input)))

;; ********
;; SOLUTION
;; ********

(defun get-unique-values (grid)
  (unique (mapcar (partial #'coord grid) (all-coords grid))))

(defun get-plant-coords (plant grid)
  (remove-if-not
      (lambda (x) (equal (coord x grid) plant))
      (all-coords grid)))

(defun get-plot-area (plant grid)
  (length (get-plant-coords plant grid)))

(assert (= 12 (get-plot-area (char "R" 0) test-input)))

(defun get-surrounding-vals (coords grid)
  (mapcar (lambda (dir) (get-val-in-dir coords dir grid)) (list UP DOWN LEFT RIGHT)))

(defun get-borders-for-coord (plant coords grid)
  (apply #'+ (mapcar
                 (lambda (val) (if (equal plant val) 0 1))
                 (get-surrounding-vals coords grid))))

(assert (= 2 (get-borders-for-coord (char "R" 0) '(0 0) test-input)))
(assert (= 3 (get-borders-for-coord (char "R" 0) '(4 2) test-input)))

(defun circumferance (plant grid)
  (apply #'+ (mapcar
                 (lambda (coords) (get-borders-for-coord plant coords grid))
                 (get-plant-coords plant grid))))

(assert (= 18 (circumferance (char "R" 0) test-input)))

(defun get-fence-price (plant grid)
  (* (circumferance plant grid) (get-plot-area plant grid)))

(defun circ-from-plot (plot grid)
  (let ((plant (coord (first plot) grid)))
    (apply #'+ (mapcar
                   (lambda (coords) (get-borders-for-coord plant coords grid))
                   plot))))

(defun area-from-lot (plot)
  (length plot))

(defun price-from-plot (plot grid)
  (* (circ-from-plot plot grid) (area-from-lot plot)))

(assert (= 216 (get-fence-price (char "R" 0) test-input)))

(defun surrounding-coords (coords)
  (mapcar (lambda (dir) (get-next-coord coords dir)) (list UP DOWN LEFT RIGHT)))

(defun surrounding-same-coords (coords grid)
  (let ((plant (coord coords grid)))
    (remove-if-not (lambda (new-coords) (equal plant (coord new-coords grid))) (surrounding-coords coords))))

(defun filter-existing (existing-lst to-be-filtered-list)
  (remove-if (partial #'member existing-lst :test #'equal) to-be-filtered-list))

(defun sort-coord-lst (coord-lst)
  (sort coord-lst (lambda (a b)
                    (or (< (first a) (first b))
                        (and (= (first a) (first b))
                             (< (second a) (second b)))))))

(defun get-plot-coords-lst (coords-queue grid &optional (coords-so-far '()))
  (cond
   ((null coords-queue)
     coords-so-far)
   ((member (first coords-queue) coords-so-far :test #'equal)
     (get-plot-coords-lst (cdr coords-queue) grid coords-so-far))
   (t
     (get-plot-coords-lst
      (append (cdr coords-queue) (surrounding-same-coords (first coords-queue) grid))
      grid
      (append coords-so-far (list (first coords-queue)))))))

(defun all-plots (grid coords-to-check &optional (plots-so-far '()))
  (unique (let ((new-coords-to-check (time-expr (filter-existing (reduce #'append plots-so-far) coords-to-check))))
            (if new-coords-to-check
                (let* ((next-plot (time-expr (get-plot-coords-lst (list (print-val (first new-coords-to-check))) grid)))
                       (new-plots (append plots-so-far (list next-plot))))
                  (append new-plots (all-plots grid new-coords-to-check new-plots)))))))

(defun get-all-plots (grid)
  (time-expr (all-plots grid (all-coords grid))))

(defun get-all-fence-prices (grid)
  (apply #'+ (mapcar (partial #'price-from-plot grid) (get-all-plots grid))))

(assert (= 1930 (get-all-fence-prices test-input)))