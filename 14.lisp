;; *******
;; INPUT
;; *******

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"14.txt")))

(defparameter test-input
              (list
               "p=0,4 v=3,-3"
               "p=6,3 v=-1,-3"
               "p=10,3 v=-1,2"
               "p=2,0 v=2,-1"
               "p=0,0 v=1,3"
               "p=3,0 v=-2,-2"
               "p=7,6 v=-1,-3"
               "p=3,0 v=-1,-2"
               "p=9,3 v=2,3"
               "p=7,3 v=-1,2"
               "p=2,4 v=2,-3"
               "p=9,5 v=-3,-3"))

;; ********
;; Parsing*
;; ********

(defun extract-p (line)
  (list
   (parse-integer (cl-ppcre:regex-replace-all ",.*" (cl-ppcre:regex-replace-all "p=" line "") ""))
   (parse-integer (cl-ppcre:regex-replace-all ".*," (cl-ppcre:regex-replace-all " .*" line "") ""))))

(defun extract-v (line)
  (list
   (parse-integer (cl-ppcre:regex-replace-all ",.*" (cl-ppcre:regex-replace-all ".*v=" line "") ""))
   (parse-integer (cl-ppcre:regex-replace-all ".*," line ""))))

(assert (equal '(0 4) (extract-p (first test-input))))
(assert (equal '(3 -3) (extract-v (first test-input))))

(defun get-bots (input)
  (mapcar
      (lambda (line) (list (extract-p line) (extract-v line)))
      input))


;; ********
;; SOLUTION
;; ********

(defun robot-position-after-seconds (start-pos velocity steps grid-dimensions)
  (list
   (mod (+ (first start-pos) (* (first velocity) steps)) (first grid-dimensions))
   (mod (+ (second start-pos) (* (second velocity) steps)) (second grid-dimensions))))

(assert (equal '(1 3) (robot-position-after-seconds '(2 4) '(2 -3) 5 '(11 7))))


(defun get-bot-positions (bots)
  (mapcar #'first bots))

(defun count-bots-in-tl-quadrant (bot-positions grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot-position) (and
                                             (< (first bot-position) grid-mid-x)
                                             (< (second bot-position) grid-mid-y)))
          bot-positions))))

(assert (= 4 (count-bots-in-tl-quadrant (get-bot-positions (get-bots test-input)) '(11 7))))

(defun count-bots-in-tr-quadrant (bot-positions grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot-position) (and
                                             (> (first bot-position) grid-mid-x)
                                             (< (second bot-position) grid-mid-y)))
          bot-positions))))

(assert (= 0 (count-bots-in-tr-quadrant (get-bot-positions (get-bots test-input)) '(11 7))))

(defun count-bots-in-bl-quadrant (bot-positions grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot-position) (and
                                             (< (first bot-position) grid-mid-x)
                                             (> (second bot-position) grid-mid-y)))
          bot-positions))))

(assert (= 2 (count-bots-in-bl-quadrant (get-bot-positions (get-bots test-input)) '(11 7))))

(defun count-bots-in-br-quadrant (bot-positions grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot-position) (and
                                             (> (first bot-position) grid-mid-x)
                                             (> (second bot-position) grid-mid-y)))
          bot-positions))))

(assert (= 2 (count-bots-in-br-quadrant (get-bot-positions (get-bots test-input)) '(11 7))))

(defun move-bots-by-steps (bots steps grid-dimensions)
  (mapcar
      (lambda (bot)
        (robot-position-after-seconds (first bot) (second bot) steps grid-dimensions))
      bots))

(defun get-bot-positions-after-steps (input steps grid-dimensions)
  (move-bots-by-steps (get-bots input) steps grid-dimensions))

(defun do-part-1 (input steps grid-dimensions)
  (let ((bots-positions-after-steps (move-bots-by-steps (get-bots input) steps grid-dimensions)))
    (*
     (count-bots-in-tl-quadrant bots-positions-after-steps grid-dimensions)
     (count-bots-in-tr-quadrant bots-positions-after-steps grid-dimensions)
     (count-bots-in-bl-quadrant bots-positions-after-steps grid-dimensions)
     (count-bots-in-br-quadrant bots-positions-after-steps grid-dimensions))))

(assert (= 12 (do-part-1 test-input 100 '(11 7))))

(defun print-bots (bot-positions grid-dimensions &optional (current-coord '(0 0)))
  (let ((x (first current-coord))
        (y (second current-coord))
        (max-x (first grid-dimensions))
        (max-y (second grid-dimensions)))
    (if (member current-coord bot-positions :test #'equal)
        (format t "#")
        (format t "."))
    (if (= x max-x) (progn (terpri) (setf y (1+ y))))
    (if (not (and (= x max-x) (= y max-y)))
        (print-bots bot-positions grid-dimensions (list (mod (1+ x) (1+ max-x)) (mod y (1+ max-y)))))))

(defun print-bots-after-steps (input steps grid-dimensions)
  (print-bots (move-bots-by-steps (get-bots input) steps grid-dimensions) grid-dimensions))

(defun get-middle-percentage-occupied (bot-positions grid-dimensions)
  (let ((middle-x (floor (first grid-dimensions) 2))
        (size-y (second grid-dimensions)))
    (floor (* 100 (length
                    (remove-if-not (lambda (pos) (= (first pos) middle-x)) bot-positions)))
           size-y)))

(defun get-positions-variance (bot-positions)
  (let ((x-positions (mapcar #'first bot-positions))
        (y-positions (mapcar #'second bot-positions)))
    (+
     (get-variance x-positions)
     (get-variance y-positions))))

(defun get-variance (nums)
  (let ((avg (/ (apply #'+ nums) (length nums))))
    (apply #'+
        (mapcar (lambda (x) (abs (- avg x))) nums))))

(defun get-positions-with-most-of-middle-occupied (input percentage grid-dimensions values-to-try-start values-to-try-end)
  (remove nil (loop for steps from values-to-try-start below values-to-try-end
                    collect (if (>= (get-middle-percentage-occupied (get-bot-positions-after-steps input steps grid-dimensions) grid-dimensions) percentage)
                                steps))))

(defun get-position-with-low-variance (input variance-threshold grid-dimensions values-to-try-start values-to-try-end)
  (remove nil (loop for steps from values-to-try-start below values-to-try-end
                    collect (if (<= (get-positions-variance (get-bot-positions-after-steps input steps grid-dimensions)) variance-threshold)
                                steps))))

; final variance was 13298