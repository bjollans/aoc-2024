(ql:quickload "split-sequence")
(use-package :split-sequence)

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
    (make-list file-len :initial-element (format nil "~d" file-id))
    (if (null space-len) '() (make-list space-len :initial-element "."))))

(defun disk-map->interm-lst (disk-map &optional (file-id 0))
  (with-print (format nil "length in disk-map ~d" (length disk-map))
              (if (null disk-map)
                  '()
                  (append
                    (num-pair->file-str (first disk-map) (second disk-map) file-id)
                    (disk-map->interm-lst (cdr (cdr disk-map)) (+ 1 file-id))))))

; (defparameter interim-lst (disk-map->interm-lst (str->num-lst test-input)))

(defun remove-dots (lst)
  (remove-if (lambda (x) (string= "." (format nil "~A" x))) (disk-map->interm-lst (str->num-lst lst))))


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
  (if (null (member "." lst :test 'equal))
      lst
      (fold-list (repl-first (butlast lst) "." (first (last lst))))))

; (repl-first (butlast interim-lst) "." (first (last interim-lst)))

(defun do-part-1 (input)
  (let* ((interim-lst (disk-map->interm-lst (str->num-lst input)))
         (folded-interim-lst (with-print (format nil "length of interim list ~d" (length interim-lst)) (fold-list interim-lst))))
    (apply
        #'+
        (mapcar
            (lambda (x y) (* x (parse-integer y)))
            (reverse (maprange
                       (partial #'* 1)
                       (length folded-interim-lst)))
          folded-interim-lst))))

(assert (= 1928 (do-part-1 test-input)))