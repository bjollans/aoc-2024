(defmacro print-val (expr)
  `(let ((res ,expr))
     (progn
      (format t "~a: ~a" ',expr ,'res)
      (terpri)
      res)))

(defmacro xlambda (expr)
  `(lambda (x) ,expr))

;optimization to just store the pad as a hashmap of chars to coords

(defparameter numpad
              (list '(#\7 #\8 #\9)
                    '(#\4 #\5 #\6)
                    '(#\1 #\2 #\3)
                    '(#\# #\0 #\A)))

(defparameter arrowpad
              (list '(#\# #\^ #\A)
                    '(#\< #\v #\>)))

(defparameter numpad-reverse-coords
              (list '(1 3) '(0 2)))

(defparameter arrowpad-reverse-coords
              (list '(1 0) '(0 1)))

(defun get-btn-coord (btn pad &optional (coords '(0 0)))
  (let* ((given-x (first coords))
         (given-y (second coords))
         (out-of-bounds (>= given-x (length (nth given-y pad))))
         (x (if out-of-bounds 0 given-x))
         (y (if out-of-bounds (+ 1 given-y) given-y)))
    (if (>= y (length pad))
        nil
        (if (char= btn (nth x (nth y pad)))
            (list x y)
            (get-btn-coord btn pad (list (+ 1 x) y))))))

(assert (equal '(0 0) (get-btn-coord #\7 numpad)))
(assert (equal '(2 3) (get-btn-coord #\A numpad)))
(assert (equal '(2 0) (get-btn-coord #\A arrowpad)))

(defun coord-diff (c1 c2)
  (mapcar #'- c1 c2))


(defun dir->num/reversed (dir)
  (case dir
    (#\> 0)
    (#\^ 1)
    (#\v 2)
    (#\< 3)))

(defun dir->num/normal (dir)
  (case dir
    (#\< 0)
    (#\v 1)
    (#\^ 2)
    (#\> 3)))

(defun sort-dirs/normal (d1 d2)
  (< (dir->num/normal d1) (dir->num/normal d2)))

(defun sort-dirs/reversed (d1 d2)
  (< (dir->num/reversed d1) (dir->num/reversed d2)))


(defun sort-bothpads-path/normal (path)
  (sort path #'sort-dirs/normal))

(defun sort-bothpads-path/reversed (path)
  (sort path #'sort-dirs/reversed))

(defun sort-arrowpad-path (path)
  (reverse (sort-numpad-path path)))

(defun get-shortest-path-between-coords (coord1 coord2)
  (let* ((diff (coord-diff coord1 coord2))
         (x-diff (first diff))
         (y-diff (second diff)))
    (append
      (make-list (abs x-diff) :initial-element (if (> x-diff 0) #\< #\>))
      (make-list (abs y-diff) :initial-element (if (> y-diff 0) #\^ #\v)))))

(defun is-in-same-row (coord1 coord2)
  (= (first coord1) (first coord2)))

(defun is-in-same-col (coord1 coord2)
  (= (second coord1) (second coord2)))


; if x coord is 0 for only one of them in numpad
; AND the other one is at the bottom for numpad and at the top for arrowpad`
(defun would-cross-bad-coord (coord1 coord2 bad-coord)
  (and
   (or (is-in-same-row coord1 bad-coord) (is-in-same-row coord2 bad-coord))
   (or (is-in-same-col coord1 bad-coord) (is-in-same-col coord2 bad-coord))))


(defun coord-list-to-paths-list (coord-list bad-coord)
  (if (> (length coord-list) 1)
      (append
        (if (would-cross-bad-coord (first coord-list) (second coord-list) bad-coord)
            (sort-bothpads-path/reversed (get-shortest-path-between-coords (first coord-list) (second coord-list)))
            (sort-bothpads-path/normal (get-shortest-path-between-coords (first coord-list) (second coord-list))))
        '(#\A)
        (coord-list-to-paths-list (cdr coord-list) bad-coord))))


(defun get-shortest-path (char-list pad bad-coord)
  (let* ((coord-list (mapx (cons #\A char-list) (get-btn-coord x pad))))
    (coord-list-to-paths-list coord-list bad-coord)))

; (assert (equal
;          (get-shortest-path '(#\0 #\2 #\9 #\A) numpad numpad-reverse-coords)
;          '(#\< #\A #\^ #\A #\> #\^ #\^ #\A #\v #\v #\v #\A)))

(assert (=
            (length (get-shortest-path '(#\< #\A #\^ #\A #\> #\^ #\^ #\A #\v #\v #\v #\A) arrowpad '(0 0)))
          (length '(#\v #\< #\< #\A #\> #\> #\^ #\A #\< #\A #\> #\A #\v #\A #\< #\^ #\A #\A #\> #\A #\< #\v #\A #\A #\A #\> #\^ #\A))))

(defun do-part-1-for-1-line (char-list)
  (get-shortest-path
    (get-shortest-path
      (get-shortest-path char-list numpad '(0 3))
      arrowpad '(0 0))
    arrowpad '(0 0)))

(defun do-part-1 (lines)
  (if (= 0 (length lines)) 0
      (append
        (do-part-1-for-1-line (first lines))
        (do-part-1 (cdr lines)))))

(* 29 (length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A))))

(assert (= 68 (length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A)))))
(assert (= 60 (length (do-part-1-for-1-line '(#\9 #\8 #\0 #\A)))))
(assert (= 68 (length (do-part-1-for-1-line '(#\1 #\7 #\9 #\A)))))
(assert (= 64 (length (do-part-1-for-1-line '(#\4 #\5 #\6 #\A)))))
(assert (= 64 (length (do-part-1-for-1-line '(#\3 #\7 #\9 #\A)))))
; -  ^  A
; <  v  >

(assert (= 126384 (+
                   (* 29 (length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A))))
                   (* 980 (length (do-part-1-for-1-line '(#\9 #\8 #\0 #\A))))
                   (* 179 (length (do-part-1-for-1-line '(#\1 #\7 #\9 #\A))))
                   (* 456 (length (do-part-1-for-1-line '(#\4 #\5 #\6 #\A))))
                   (* 379 (length (do-part-1-for-1-line '(#\3 #\7 #\9 #\A)))))))

(+ (* 140 (length (do-part-1-for-1-line '(#\1 #\4 #\0 #\A))))
(* 169 (length (do-part-1-for-1-line '(#\1 #\6 #\9 #\A))))
(* 170 (length (do-part-1-for-1-line '(#\1 #\7 #\0 #\A))))
(* 528 (length (do-part-1-for-1-line '(#\5 #\2 #\8 #\A))))
(* 340 (length (do-part-1-for-1-line '(#\3 #\4 #\0 #\A)))))
