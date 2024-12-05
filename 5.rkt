#lang racket

(define (asrt cond descr) 
    (if cond (string-append "AssertionSuccess: " descr) (raise (string-append "AssertionError: " descr))))

(define (default-ref hashmap refname default-value) 
    (if (hash-has-key? hashmap refname)
        (hash-ref hashmap refname)
        default-value))

(define (read-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ([lines '()])
        (let ([line (read-line)])
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define actual-rules
  (read-lines "5_rules.txt"))

(define actual-updates
  (read-lines "5_updates.txt"))

(define test-rules '(
    "47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
))

(define test-updates '(
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"
))

(define (str->num-list rgx str) 
    (map 
        (lambda 
            (x) 
            (string->number x)) 
        (regexp-split 
            rgx 
            str)))

(define (rule->kv rule)
    (str->num-list #rx"\\|" rule))

(define (update->num-list update)
    (str->num-list #rx"," update))

(displayln (update->num-list "75,47,61,53,29"))

(define test-rule-kvs (map rule->kv test-rules))
(define actual-rule-kvs (map rule->kv actual-rules))

(define (k-must-be-after-vs-rule-hash rule-kvs)
    (for/fold ([h (hash)]) ([kv rule-kvs])
        (let (  [key (second kv)]
                [value (list (first kv))])
            (if (hash-has-key? h key)
                (hash-set h key (append (hash-ref h key) value))
                (hash-set h key value))
            )))

(define (is-update-valid num-list rule-kvs)
    (if (null? num-list)
        #t
        (let* ( [num (first num-list)]
                [remaining-list (cdr num-list)]
                [must-be-before-num-list (if (hash-has-key? (k-must-be-after-vs-rule-hash rule-kvs) num)
                                                (hash-ref (k-must-be-after-vs-rule-hash rule-kvs) num)
                                                '())])
                (and 
                    (not (ormap 
                        (lambda (x) (member x must-be-before-num-list))
                        remaining-list))
                    (is-update-valid remaining-list rule-kvs)))))

(asrt (is-update-valid (update->num-list "75,47,61,53,29") test-rule-kvs) "valid update")
(asrt (not (is-update-valid (update->num-list "97,13,75,29,47") test-rule-kvs)) "invalid update")

(define (get-middle-number lst)
    (let* ( [len (length lst)]
            [middle-index (quotient len 2)])
        (list-ref lst middle-index)))

(asrt (= (get-middle-number '(30 2 50 5 6)) 50) "middle number works")

(define (get-valid-updates update-list rule-kvs)
    (filter (lambda (x) (is-update-valid x rule-kvs)) (map update->num-list update-list)))

(define (get-invalid-updates update-list rule-kvs)
    (filter (lambda (x) (not (is-update-valid x rule-kvs))) (map update->num-list update-list)))

(map update->num-list test-updates)

(apply + (map get-middle-number (get-valid-updates test-updates test-rule-kvs)))

(asrt (= (apply + (map get-middle-number (get-valid-updates test-updates test-rule-kvs))) 143) "Test values work for part 1")

; (apply + (map get-middle-number (get-valid-updates actual-updates actual-rule-kvs)))

(k-must-be-after-vs-rule-hash test-rule-kvs)

(define (should-be-after-any num lst rule-kvs)
    (let ([after-nums (default-ref (k-must-be-after-vs-rule-hash rule-kvs) num '())]) 
        (ormap 
            (lambda (x) (member x after-nums))
            lst)))

(asrt (not (should-be-after-any 97 '(97 13 75 29 47) test-rule-kvs)) "97 should not be after any in test 97,13,75,29,47")
(asrt (should-be-after-any 13 '(97 13 75 29 47) test-rule-kvs) "13 should be after some in test 97,13,75,29,47")

(define (next-val-in-order lst rule-kvs)
    (first (filter 
        (lambda (x) 
            (not (should-be-after-any x lst rule-kvs))) 
        lst)))

(asrt (= (next-val-in-order '(97 13 75 29 47) test-rule-kvs) 97) "97 should be next value for 97,13,75,29,47")
(asrt (= (next-val-in-order '(13 75 29 47) test-rule-kvs) 75) "75 should be next value for 13,75,29,47")
(asrt (= (next-val-in-order '(13 29 47) test-rule-kvs) 47) "47 should be next value for 13,29,47")
(asrt (= (next-val-in-order '(13 29) test-rule-kvs) 29) "29 should be next value for 13,29")
(asrt (= (next-val-in-order '(13) test-rule-kvs) 13) "13 should be next value for 13")

(define (re-order-lst lst rule-kvs)
    (if (null? lst)
        '()
        (let ([next-val (next-val-in-order lst rule-kvs)])
            (cons 
                next-val 
                (re-order-lst 
                    (remove next-val lst) 
                    rule-kvs)))))

(re-order-lst '(97 13 75 29 47) test-rule-kvs)
(asrt (equal? (re-order-lst '(97 13 75 29 47) test-rule-kvs) '(97 75 47 29 13)) "'(97 13 75 29 47) should reorder to '(97 75 47 29 13)")
(asrt (equal? (re-order-lst '(75 97 47 61 53) test-rule-kvs) '(97 75 47 61 53)) "'(75 97 47 61 53) should reorder to '(97 75 47 61 53)")
(asrt (equal? (re-order-lst '(61 13 29) test-rule-kvs) '(61 29 13)) "'(61 13 29) should reorder to '(61 29 13)")



(define (re-order-invalid-lists updates rule-kvs)
    (map 
        (lambda (x) (re-order-lst x rule-kvs))
        (get-invalid-updates updates rule-kvs)))

(define (do-part-2 updates rule-kvs)
    (apply + (map 
        (lambda (x) (get-middle-number x))
        (re-order-invalid-lists updates rule-kvs))))

(asrt (= (do-part-2 test-updates test-rule-kvs) 123) "part 2 works for test vals")

(do-part-2 actual-updates actual-rule-kvs)