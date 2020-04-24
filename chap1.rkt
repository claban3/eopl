;; ---------- Exercise 1.7 ----------
(define nth-element-helper
    (lambda (lst n)
        (if (null? lst)
            lst
        (if (zero? n)
            (car lst)
            (nth-element-helper (cdr lst) (- n 1))))))

(define nth-element
    (lambda (lst n)
        (define x (nth-element-helper lst n))
        (if (null? x)
            (report-list-too-short lst n)
            x)))
            
(define report-list-too-short
    (lambda (lst n)
        (error `nth-element
            "List ~s does not have ~s elements.~%" lst (+ n 1))))

; (display (nth-element `(1 2 3) 2)) (newline)

;; ---------- Exercise 1.9 ----------
(define remove-my
    (lambda (s los)
        (if (null? los)
            `()
            (if (eqv? s (car los))
                (remove-my s (cdr los))
                (cons (car los) (remove-my s (cdr los)))))))

; (display (remove-my 3 `(1 1 3 3 3 2))) (newline)

;; ---------- Exercise 1.13 ----------
(define curry (lambda (f) (lambda (a) (lambda (b) (lambda (c) (f a b c))))))

(define subst-in-s-exp
    (lambda (new old sexp)
        (if (symbol? sexp)
            (if (eqv? sexp old) new sexp)
                (subst new old sexp))))

(define my-subst (curry subst-in-s-exp))

(define subst
    (lambda (new old slist)
    (if (null? slist)
        `()
        (map ((my-subst new) old) slist))))

; (display (subst `x `y `((x y) () (y b a (y)) a y))) (newline)

;; ---------- Exercise 1.15 ----------
(define duple 
    (lambda (n x)
        (if (zero? n)
            `()
            (cons x (duple (- n 1) x)))))

; (display (duple 1 0)) (newline)

;; ---------- Exercise 1.16 ----------
(define inv-two
    (lambda (two-lst)
        (cons (car (cdr two-lst)) (list (car two-lst)))))

(define inverse
    (lambda (lst)
        (map inv-two lst)))

; (display (inverse `((a b) (x y) (z w)))) (newline)

;; ---------- Exercise 1.21 ----------
(define scalar-cross
    (lambda (x lst)
        (if (null? lst)
            `()
            (cons (list x (car lst)) (scalar-cross x (cdr lst))))))

(define product
    (lambda (sos1 sos2)
        (if (null? sos1)
            `()
            (append (scalar-cross (car sos1) sos2) (product (cdr sos1) sos2)))))


(display (scalar-cross `x `(a b c d))) (newline)
(display (product `(x y z x) `(a b c))) (newline)
