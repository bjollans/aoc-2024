;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"10.txt")))

(defparameter test-input
              (list
               "89010123"
               "78121874"
               "87430965"
               "96549874"
               "45678903"
               "32019012"
               "01329801"
               "10456732"))


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

(defparameter outside-grid-val -9000)

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
        (digit-char-p (char (nth y grid) x)))))

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

(defparameter RIGHT '(1 0))
(defparameter LEFT '(-1 0))
(defparameter UP '(0 -1))
(defparameter DOWN '(0 1))

(defun get-next-coord (coords direction) (mapcar #'+ coords direction))


;; ********
;; SOLUTION
;; ********

(defun get-start-coords (input)
  (remove-if-not
      (lambda (x) (= 0 (coord x input)))
      (all-coords input)))

(defun can-move-there (coords direction grid)
  (let ((current-val (coord coords grid))
        (next-val (coord (get-next-coord coords direction) grid)))
    (= 1 (- next-val current-val))))

(defun get-trailhead-coords (grid)
  (remove-if-not
      (lambda (x) (= 0 (coord x grid)))
      (all-coords grid)))


(defun has-visited (coords)
  (member coords visited-so-far :test #'equal))

(defun visit (coords)
  (setf visited-so-far (cons coords visited-so-far)))

(defparameter visited-so-far '())

(defun get-next-possible-coords (coords grid)
  (remove nil (mapcar
                  (lambda (direction)
                    (if (can-move-there coords direction grid)
                        (get-next-coord coords direction)))
                  (list UP DOWN LEFT RIGHT))))

(defun get-next-coords (coords grid)
  (remove-if #'has-visited (get-next-possible-coords coords grid)))

(defun get-possible-trail-ends-rec (from-coord grid)
  (progn
   (visit from-coord)
   (let ((val (coord from-coord grid)))
     (if (= 9 val)
         (list from-coord)
         (mapcan
             (partial #'get-possible-trail-ends-rec grid)
             (get-next-coords from-coord grid))))))

(defun get-possible-trail-ends (from-coord grid)
  (progn
   (setf visited-so-far '())
   (get-possible-trail-ends-rec from-coord grid)))

(defun count-trail-ends (from-coord grid)
  (length (get-possible-trail-ends from-coord grid)))

(defun do-part-1 (grid)
  (apply #'+ (mapcar (partial #'count-trail-ends grid) (get-start-coords grid))))

(assert (= 36 (do-part-1 test-input)))