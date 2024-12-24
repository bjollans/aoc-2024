(defmacro print-val (expr)
  `(let ((res ,expr))
     (progn
      (format t "~a: ~a" ',expr ,'res)
      (terpri)
      res)))

(defmacro xlambda (expr)
  `(lambda (x) ,expr))


(defmacro memfun (name args expr code)
  `(defun ,name ,args
     (let ((memory (make-hash-table :test #'equal)))
       (labels ((call-self ,args ,expr)) ,code))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

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

(defparameter memory (make-hash-table :test #'equal))

(defun get-path-from-mem (coord1 coord2 bad-coord)
  (gethash (list coord1 coord2 bad-coord) memory))

(defun set-path-to-mem (coord1 coord2 bad-coord path)
  (setf (gethash (list coord1 coord2 bad-coord) memory) path))


(defun coord-pair-to-path (coord1 coord2 bad-coord)
  (or (get-path-from-mem coord1 coord2 bad-coord)
      (progn
       (set-path-to-mem
         coord1 coord2 bad-coord
         (append
           (if (would-cross-bad-coord coord1 coord2 bad-coord)
               (sort-bothpads-path/reversed (get-shortest-path-between-coords coord1 coord2))
               (sort-bothpads-path/normal (get-shortest-path-between-coords coord1 coord2)))
           '(#\A)))
       (get-path-from-mem coord1 coord2 bad-coord))))


(defun coord-list-to-paths-list (coord-list bad-coord)
  (if (> (length coord-list) 1)
      (append
        (coord-pair-to-path (first coord-list) (second coord-list) bad-coord)
        (coord-list-to-paths-list (cdr coord-list) bad-coord))))

(defmacro mapx (lst lambda-expr)
  `(remove nil (mapcar (xlambda ,lambda-expr) ,lst)))

(defun get-coord-pair-list (coord-list)
  (if (> (length coord-list) 2)
      (append
        (list (list 1 (list (first coord-list) (second coord-list))))
        (list (get-coord-pair-list (cdr coord-list))))
      (list (list 1 (list (first coord-list) (second coord-list))))))

(memfun group-cnt-items (cnt-item-list)
        (if (> (length cnt-item-list) 0)
            (progn
             (let* ((cnt-item-w-cnt (first cnt-item-list))
                    (cnt-item-cnt (first cnt-item-w-cnt))
                    (cnt-item (second cnt-item-w-cnt))
                    (latest-count (or (gethash cnt-item memory) 0)))
               (setf (gethash cnt-item memory) (+ cnt-item-cnt latest-count)))
             (call-self (rest cnt-item-list)))
            (mapx (hash-keys memory) (list (gethash x memory) x)))
        (call-self cnt-item-list))

(defun get-shortest-path (char-list pad bad-coord)
  (let* ((coord-list (mapx (cons #\A char-list) (get-btn-coord x pad))))
    (coord-list-to-paths-list coord-list bad-coord)))

(defun pathlist-length (pathlist)
  (apply #'+ (mapx pathlist (* (first x) (length (second x))))))

(defun do-part-1-for-1-line (char-list)
  (expand-char-cnt-list
    (expand-char-cnt-list
      (expand-char-cnt-list (list (list 1 char-list)) numpad '(0 3))
      arrowpad '(0 0))
    arrowpad '(0 0)))

(defun char-list-to-btn-groups (char-list)
  (mapx
    (reverse (cdr (reverse (split-sequence:split-sequence #\A (coerce char-list 'string)))))
    (append (coerce x 'list) '(#\A))))

(defun expand-char-cnt-list (char-cnt-list pad bad-coord)
  (group-cnt-items (mapcan
                       (xlambda
                         (mapcar (lambda (sub-path) (list (first x) sub-path))
                             (char-list-to-btn-groups (get-shortest-path (second x) pad bad-coord))))
                       char-cnt-list)))

(defun do-part-1 (lines)
  (if (= 0 (length lines)) 0
      (append
        (do-part-1-for-1-line (first lines))
        (do-part-1 (cdr lines)))))

(* 29 (pathlist-length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A))))

(assert (= 68 (pathlist-length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A)))))
(assert (= 60 (pathlist-length (do-part-1-for-1-line '(#\9 #\8 #\0 #\A)))))
(assert (= 68 (pathlist-length (do-part-1-for-1-line '(#\1 #\7 #\9 #\A)))))
(assert (= 64 (pathlist-length (do-part-1-for-1-line '(#\4 #\5 #\6 #\A)))))
(assert (= 64 (pathlist-length (do-part-1-for-1-line '(#\3 #\7 #\9 #\A)))))
; -  ^  A
; <  v  >

(assert (= 126384 (+
                   (* 29 (pathlist-length (do-part-1-for-1-line '(#\0 #\2 #\9 #\A))))
                   (* 980 (pathlist-length (do-part-1-for-1-line '(#\9 #\8 #\0 #\A))))
                   (* 179 (pathlist-length (do-part-1-for-1-line '(#\1 #\7 #\9 #\A))))
                   (* 456 (pathlist-length (do-part-1-for-1-line '(#\4 #\5 #\6 #\A))))
                   (* 379 (pathlist-length (do-part-1-for-1-line '(#\3 #\7 #\9 #\A)))))))

(+ (* 140 (pathlist-length (do-part-1-for-1-line '(#\1 #\4 #\0 #\A))))
   (* 169 (pathlist-length (do-part-1-for-1-line '(#\1 #\6 #\9 #\A))))
   (* 170 (pathlist-length (do-part-1-for-1-line '(#\1 #\7 #\0 #\A))))
   (* 528 (pathlist-length (do-part-1-for-1-line '(#\5 #\2 #\8 #\A))))
   (* 340 (pathlist-length (do-part-1-for-1-line '(#\3 #\4 #\0 #\A)))))

(defun do-part-2-for-1-line (char-list)
  (expand-char-cnt-list
    (expand-char-cnt-list
      (expand-char-cnt-list
        (expand-char-cnt-list
          (expand-char-cnt-list
            (expand-char-cnt-list
              (expand-char-cnt-list
                (expand-char-cnt-list
                  (expand-char-cnt-list
                    (expand-char-cnt-list
                      (expand-char-cnt-list
                        (expand-char-cnt-list
                          (expand-char-cnt-list
                            (expand-char-cnt-list
                              (expand-char-cnt-list
                                (expand-char-cnt-list
                                  (expand-char-cnt-list
                                    (expand-char-cnt-list
                                      (expand-char-cnt-list
                                        (expand-char-cnt-list
                                          (expand-char-cnt-list
                                            (expand-char-cnt-list
                                              (expand-char-cnt-list
                                                (expand-char-cnt-list
                                                  (expand-char-cnt-list
                                                    (expand-char-cnt-list (list (list 1 char-list)) numpad '(0 3))
                                                    arrowpad '(0 0))
                                                  arrowpad '(0 0))
                                                arrowpad '(0 0))
                                              arrowpad '(0 0))
                                            arrowpad '(0 0))
                                          arrowpad '(0 0))
                                        arrowpad '(0 0))
                                      arrowpad '(0 0))
                                    arrowpad '(0 0))
                                  arrowpad '(0 0))
                                arrowpad '(0 0))
                              arrowpad '(0 0))
                            arrowpad '(0 0))
                          arrowpad '(0 0))
                        arrowpad '(0 0))
                      arrowpad '(0 0))
                    arrowpad '(0 0))
                  arrowpad '(0 0))
                arrowpad '(0 0))
              arrowpad '(0 0))
            arrowpad '(0 0))
          arrowpad '(0 0))
        arrowpad '(0 0))
      arrowpad '(0 0))
    arrowpad '(0 0)))


(+ (* 140 (pathlist-length (do-part-2-for-1-line '(#\1 #\4 #\0 #\A))))
   (* 169 (pathlist-length (do-part-2-for-1-line '(#\1 #\6 #\9 #\A))))
   (* 170 (pathlist-length (do-part-2-for-1-line '(#\1 #\7 #\0 #\A))))
   (* 528 (pathlist-length (do-part-2-for-1-line '(#\5 #\2 #\8 #\A))))
   (* 340 (pathlist-length (do-part-2-for-1-line '(#\3 #\4 #\0 #\A)))))