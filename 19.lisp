; check all possible sub combinations
; check all possible sub sub combinations
; if any is possible, return true (do with a queue)

(defparameter actual-input
              (split-sequence:split-sequence
                #\newline
                (uiop:read-file-string
                  #p"19.txt")))

(defparameter actual-towels
              (mapx (split-sequence:split-sequence #\, "gur, uub, wgwu, rggu, urguur, uugbrw, rwu, ruw, wubgrb, wugu, rrg, uguurg, gwrwb, www, gggwb, bbw, gbrwgwg, rub, grwwur, rwrb, ubb, gbgbg, gwugw, bgw, guwr, guw, rrgbrw, uru, wurwg, wuw, rgbu, rguw, grwur, gwwg, grg, gwbw, wuu, brubbrw, wwuwr, rg, b, buwwug, wruwgrg, wwwwwrrg, wwww, rwbubr, uggr, gbrb, bbwbu, bwrrguw, bwu, wu, wru, gbbw, wwbbw, wwwuu, uwg, gub, ru, ugurwu, burgwbu, uwrg, wgubwrru, bwwgww, uwgugww, rrggu, uugwb, wrru, rrwr, bu, ugb, ugbg, bbgbr, ubr, rwruwur, uwuuguw, rgb, bbur, rbrwr, gwb, ggggbbg, ruugbr, ubbbgr, wrrrgg, bbwww, rgg, urggrrgb, gbwr, brgb, rgbwr, wbuw, rur, rwr, ggrgw, buruu, gug, wuwg, gg, uwrbuug, ubbwbrg, w, wuguuw, wrww, bbb, rw, rgr, brggrgw, gwrrub, rwgbu, gwrrgu, rgug, buu, bbbguggg, wwwugub, grrbuw, rrugu, wgw, rgugrbwu, uuwbwwr, gwuuwuu, wrrwwr, buw, rbr, uurw, wbug, rggwr, bbubgu, rgrwwu, rbggr, gbrbuu, ggwuguw, ruu, gwr, bru, wbg, wuwgbrr, wrrg, ub, ugu, urg, wwbw, gbw, ugg, wrubr, gubu, bwwg, rubbwu, ggurr, rwww, uurg, wwb, rbrgu, rburu, rrw, bwggwr, rwgggr, rwubg, wuug, gr, grwwbw, wub, uurr, wrrr, wugg, ug, brrr, wwbur, bwbgg, bruw, uguw, bbuurgw, rwugu, wuwrgr, uuw, grr, rrrg, gru, rbwb, ggbbw, urubrbb, rrgggr, bbg, wurwrw, bwubb, urr, rbg, brbrbb, rgu, bwgrbgb, rrbbg, wbgw, wuggub, bg, uwbr, rbb, wruw, bbwwwb, brbg, ggrgubwg, ubg, rrurb, bwbub, wrugrw, gbrr, ururg, rrwu, uburwggw, gwrg, bubu, bbugg, brbwwubb, wrgb, rgur, ururb, u, bgu, guu, rbu, urrbwb, rrbu, grurwwgg, wgr, wbbg, bgr, gruw, bbr, rbbbwr, uu, ggg, rru, wwuwu, wbww, rbwu, wruuu, urug, rrbgbuw, wrrbg, gww, wg, wgrg, wur, wwrwg, bb, uugg, uwur, wwugr, bbgwbww, uugw, wubwr, gbg, brwb, rgw, wrrrw, rbgu, ugr, gbbbbw, wgu, wbr, ubbu, wwrw, bugur, rrrur, wuubr, rgwb, bwbbbuu, wurwbrbg, ugw, uug, wrub, ggubb, uwuww, bubbb, gbr, brw, bwgu, gw, grwg, wwr, bwrrw, wwg, gwub, wgbwgrg, ggr, rwburubu, bgg, gbb, ggw, brwrrbw, gbuwb, wug, gwg, wbgwg, br, ruwuw, gwgrrw, bwgg, wbbb, gurwg, wwwbguur, ggrgbb, rgwuuu, rgugb, uugubu, bgrrg, gwbr, ugbw, uburwb, rwbwgbb, gwu, gbbu, bwb, rb, brgr, rbbur, gwurrb, wwu, bbu, gurg, bug, wrw, bbuwu, bbwu, gwuwbw, ggrrr, wruugg, grwrr, bggwgr, gbrbr, uur, gbgr, brbwwubu, bwggrbb, rwbr, rr, ubbwr, rwrrwgwr, uwgrwgr, grwugbrg, ggu, bwwub, rggbb, rwb, ggubru, wwrwuww, wrb, urb, grwbuw, bw, bbgbb, rgbrrr, wbu, rbw, wrubg, bugr, rbbrr, wgww, rug, burwuwb, wbubrw, brwrbwb, rbwr, bgb, gwbuwwbu, rwggr, wrr, urw, uurb, wrgrrrr, wubg, gwrb, wb, burgg, rgbugrw, rgrgru, urrw, bugbg, wbwb, gurwruuw, rubrrb, wbw, brbrgb, r, wrwbru, grw, urgw, wrg, wugug, rruwbbg, rwggurw, bbwr, uggggw, wgb, ugbbr, rwrgr, uwr, buwuwwrb, ubggrg, bur, uwrgg, uubrw, brrgwwg, ugbgb, burburb, ggwwbbgr, uwb, wbb, rwuw, brg, ugru, ur, rwggrr, wggw, gwwuu, ggb, bgwrbwu, grww, ruwu, uuu, wwgrug, urub, rubwbwbr, rrrbrbb, uww, ggwr, uruuru, bww, rwg, uw, rww, ubw, gu, wugrw, gbru, uwu, ruwbrb, ww, gggugw, uuwbbr, bwr, brb, rrr, grb, bubb, bub, gbug, bgrgubw, ubu") (remove #\SPACE x)))


(defparameter test-towels
              (mapx (split-sequence:split-sequence #\, "r, wr, b, g, bwu, rb, gb, br") (remove #\SPACE x)))


(defparameter test-input
              (list
               "brwrr"
               "bggr"
               "gbbr"
               "rrbgbr"
               "ubwu"
               "bwurrg"
               "brgr"
               "bbrgwb"))

(defmacro xlambda (expr)
  `(lambda (x) ,expr))

(defmacro mapx (lst lambda-expr)
  `(remove nil (mapcar (xlambda ,lambda-expr) ,lst)))

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

(defun get-next-matching-towels (pattern towels)
  (remove-if-not (xlambda (let ((search-match (search x pattern :test 'char=)))
                            (and search-match (= 0 search-match))))
      towels))

(assert (equal '("b" "br") (get-next-matching-towels "brwrr" test-towels)))

(defun remove-towel-from-pattern (pattern towel)
  (subseq pattern (length towel)))

(assert (equal "wrr" (remove-towel-from-pattern "brwrr" "br")))

(defun any-towel-matches-exactly (pattern towels)
  (remove-if-not (xlambda (equal x pattern)) towels))

(assert (any-towel-matches-exactly "b" (list "br" "b" "k")))
(assert (not (any-towel-matches-exactly "b" (list "br" "bk" "k"))))

(defun is-combination-valid (pattern-q towels)
  (if (not pattern-q) nil
      (let ((next-pattern (first pattern-q)))
        (if (any-towel-matches-exactly (print-val next-pattern) towels) t
            (let* ((matching-towels (get-next-matching-towels next-pattern towels))
                   (next-patterns (if matching-towels
                                      (mapx matching-towels (remove-towel-from-pattern next-pattern x))
                                      '())))
              (is-combination-valid (append next-patterns (cdr pattern-q)) towels))))))

(assert (is-combination-valid (list "brwrr") test-towels))
(assert (not (is-combination-valid (list "ubwu") test-towels)))

(defun do-part-1 (patterns towels)
  (length (remove-if-not (xlambda (time-expr (is-combination-valid (list x) towels))) patterns)))

(assert (= 6 (do-part-1 test-input test-towels)))