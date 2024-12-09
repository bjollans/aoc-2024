; (ql:quickload "split-sequence")
; (use-package :split-sequence)

(defparameter actual-input
              (first (split-sequence:split-sequence
                       #\newline
                       (uiop:read-file-string
                         #p"9.txt"))))

(defparameter test-input "2333133121414131402")

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

(defun str->num-lst (str)
  (if (string= "" str)
      '()
      (append
        (list (digit-char-p (char str 0)))
        (str->num-lst (subseq str 1)))))

(assert (= 5 (+ (first (str->num-lst test-input)) (second (str->num-lst test-input)))))
(assert (equal '(2 3 3 3 1 3 3 1 2 1 4 1 4 1 3 1 4 0 2) (str->num-lst test-input)))

(defun num-pair->file-str (file-len space-len file-id)
  (append
    (make-list file-len :initial-element file-id)
    (if (null space-len) '() (make-list space-len :initial-element -1))))

(defun disk-map->interm-lst (disk-map &optional (file-id 0))
  (if (null disk-map)
      '()
      (append
        (num-pair->file-str (first disk-map) (second disk-map) file-id)
        (disk-map->interm-lst (cdr (cdr disk-map)) (+ 1 file-id)))))

; (defparameter interim-lst (disk-map->interm-lst (str->num-lst test-input)))

(defun remove-dots (lst)
  (remove-if (lambda (x) (= -1 x)) (disk-map->interm-lst (str->num-lst lst))))


(defun repl-first (lst item repl)
  (if (null lst)
      '()
      (if (equal (first lst) item)
          (append (list repl) (cdr lst))
          (cons (first lst) (repl-first (cdr lst) item repl)))))

; (defun next-index (lst item index)
;   (if (null lst)
;       -1
;       (if (equal item (first lst))
;           index
;           (next-dot lst (+ 1 index)))))


(defun fold-list (lst)
  (if (null (member -1 lst :test 'equal))
      lst
      (fold-list (repl-first (butlast lst) -1 (first (last lst))))))

; (repl-first (butlast interim-lst) -1 (first (last interim-lst)))

(defun calc-checksum (lst)
  (apply
      #'+
      (mapcar
          (lambda (x y) (* x (max 0 y)))
          (reverse (maprange
                     (partial #'* 1)
                     (length lst)))
        lst)))


(defun do-part-1 (input)
  (let* ((interim-lst (disk-map->interm-lst (str->num-lst input)))
         (folded-interim-lst (fold-list interim-lst)))
    (calc-checksum folded-interim-lst)))

(assert (= 1928 (do-part-1 test-input)))

(defun max-for-nums (lst)
  (apply #'max (remove-if-not #'numberp lst)))

(assert (= 3 (max-for-nums '(1 2 3 -1 0))))

(defun get-num-length (num disk-map &optional (length-so-far 0))
  (if
   (null disk-map)
   length-so-far
   (get-num-length num (cdr disk-map) (+
                                       (if (equal (first disk-map) num) 1 0)
                                       length-so-far))))

(assert (= 3 (get-num-length 9 '(1 1 -1 -1 -1 3 4 -1 5 5 6 7 8 8 8 9 9 9 10 10))))

(defun get-beginning-of-dot-len (len disk-map no-pass-num &optional (length-seen 0) (index 0))
  (cond
   ((>= length-seen len) (- index len))
   ((null disk-map) NIL)
   ((null (first disk-map)) NIL)
   ((= (first disk-map) no-pass-num) NIL)
   (t (get-beginning-of-dot-len
        len
        (cdr disk-map)
        no-pass-num
        (if (equal (first disk-map) -1) (1+ length-seen) 0)
        (1+ index)))))

(assert (= 2 (get-beginning-of-dot-len 3 '(1 1 -1 -1 -1 3 4 -1 -1 -1 -1 5 5 6 7 8 8 8 9 9 9 10 10) -2)))
(assert (= 7 (get-beginning-of-dot-len 4 '(1 1 -1 -1 -1 3 4 -1 -1 -1 -1 5 5 6 7 8 8 8 9 9 9 10 10) -2)))
(assert (not (get-beginning-of-dot-len 40 '(1 1 -1 -1 -1 3 4 -1 -1 -1 -1 5 5 6 7 8 8 8 9 9 9 10 10) -2)))

(defun replace-num-w-dots (num disk-map)
  (if (not (null disk-map))
      (progn
       (if (= num (first disk-map)) (setf (first disk-map) -1))
       (replace-num-w-dots num (cdr disk-map)))))

(defun insert-num (num disk-map index amount)
  (progn
   (if (<= index 0) (setf (first disk-map) num))
   (if (> index (- 1 amount)) (insert-num num (cdr disk-map) (1- index) amount))))


(defun fold-p2 (disk-map)
  (let ((num (apply #'max disk-map)))
    (loop while (>= num 0)
          do (let*
                 ((len (get-num-length num disk-map))
                  (dot-index (get-beginning-of-dot-len len disk-map num)))
               (progn
                (if (not (null dot-index))
                    (progn
                     (replace-num-w-dots num disk-map)
                     (insert-num num disk-map dot-index len)))
                (setf num (1- num)))))))

(defun do-part-2 (input)
  (let* ((interim-lst (disk-map->interm-lst (str->num-lst input))))
    (progn
     (format t "~A" interim-lst)
     (fold-p2 interim-lst)
     (format t "~A" interim-lst)
     (calc-checksum interim-lst))))

(assert (= 2858 (do-part-2 test-input)))