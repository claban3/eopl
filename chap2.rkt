; ---------- Exercise 2.1 ----------
(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successor 
    (lambda (powers)
        (if (null? powers)
            `()
            (if (eqv? 15 (car powers))
                (cons (car powers) (successor (cdr powers)))
                (cons (+ (car powers) 1) (cdr powers))))))

(define predecessor 
    (lambda (powers)
        (if (null? powers)
            `()
            (if (eqv? 0 (car powers))
                (cons (+ (car powers) 15) (predecessor (cdr powers)))
                (cons (- (car powers) 1) (cdr powers))))))

; (display (predecessor `(0 0 15))) (newline)

; ---------- Exercise 2.2 ----------
(define zero (lambda () `(diff (one) (one))))
(define is-zero? (lambda (lst) (equal? lst (zero))))
; (define successor 
;     (lambda (lst)
;         ))

(define predecessor
    (lambda (lst)
        (list `diff lst `one)))

; (display (equal? `(diff (one) (one)) (zero)))
; (display `(diff (one) (one)))
; (display (is-zero? `(diff (one) (one)))) (newline)
(display (predecessor `(diff (one) (one)))) (newline)
