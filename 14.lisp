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


(defun count-bots-in-tl-quadrant (bots grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot) (and
                                    (< (first (first bot)) grid-mid-x)
                                    (< (second (first bot)) grid-mid-y)))
          bots))))

(assert (= 4 (count-bots-in-tl-quadrant (get-bots test-input) '(11 7))))

(defun count-bots-in-tr-quadrant (bots grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot) (and
                                    (> (first (first bot)) grid-mid-x)
                                    (< (second (first bot)) grid-mid-y)))
          bots))))

(assert (= 0 (count-bots-in-tr-quadrant (get-bots test-input) '(11 7))))

(defun count-bots-in-bl-quadrant (bots grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot) (and
                                    (< (first (first bot)) grid-mid-x)
                                    (> (second (first bot)) grid-mid-y)))
          bots))))

(assert (= 2 (count-bots-in-bl-quadrant (get-bots test-input) '(11 7))))

(defun count-bots-in-br-quadrant (bots grid-dimensions)
  (let ((grid-mid-x (floor (first grid-dimensions) 2))
        (grid-mid-y (floor (second grid-dimensions) 2)))
    (length
      (remove-if-not (lambda (bot) (and
                                    (> (first (first bot)) grid-mid-x)
                                    (> (second (first bot)) grid-mid-y)))
          bots))))

(assert (= 2 (count-bots-in-br-quadrant (get-bots test-input) '(11 7))))

(defun move-bots-by-steps (bots steps grid-dimensions)
  (mapcar
      (lambda (bot)
        (list (robot-position-after-seconds (first bot) (second bot) steps grid-dimensions) (second bot)))
      bots))

(defun do-part-1 (input steps grid-dimensions)
  (let ((bots-after-steps (move-bots-by-steps (get-bots input) steps grid-dimensions)))
    (*
     (count-bots-in-tl-quadrant bots-after-steps grid-dimensions)
     (count-bots-in-tr-quadrant bots-after-steps grid-dimensions)
     (count-bots-in-bl-quadrant bots-after-steps grid-dimensions)
     (count-bots-in-br-quadrant bots-after-steps grid-dimensions))))

(assert (= 12 (do-part-1 test-input 100 '(11 7))))