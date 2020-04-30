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
(define successor 
    (lambda (lst)
        (list `diff lst `(diff (diff (one) (one)) (one)))))

(define predecessor
    (lambda (lst)
        (list `diff lst `one)))

(define diff-tree-plus
    (lambda (n-1 n-2)
        (diff n-1 (diff zero n-2))))

; (display (equal? `(diff (one) (one)) (zero)))
; (display `(diff (one) (one)))
; (display (is-zero? `(diff (one) (one)))) (newline)
; (display (predecessor `(diff (one) (one)))) (newline)
; (display (successor `(diff (one) (one)))) (newline)

; ---------- Exercise 2.5 ----------
; (define empty-env (lambda () `()))
; (define extend-env 
;     (lambda (var val env)
;         (list var val env)))

; (define apply-env
;     (lambda (env search-var)
;         (display env)(newline)
;         (if (eqv? (car env) `())
;             (report-no-binding-found search-var)
;             (let ((saved-var (car env))
;                 (saved-val (cadr env))
;                 (saved-env (caddr env)))
;             (if (eqv? search-var saved-var)
;                 saved-val
;                 (apply-env saved-env search-var))))))

; (define report-no-binding-found
;     (lambda (search-var)
;         (error `apply-env "No binding for ~s" search-var)))

; (define e 
;     (extend-env `d 6
;         (extend-env `c 5
;             (extend-env `b 4
;                 (empty-env)))))

; (display (apply-env e `c))(newline)

; ---------- Exercise 2.11 ----------
(define empty-env (lambda () `(())))
(define extend-env* 
    (lambda (vars vals env)
        (list vars vals env)))

(define find-in-ribs
    (lambda (search-var vars vals)
        (if (eqv? vars `())
            `()
            (if (eqv? (car vars) search-var)
                (car vals)
                (find-in-ribs search-var (cdr vars) (cdr vals))))))

(define apply-env
    (lambda (env search-var)
        (display env)(newline)
        (if (eqv? (car env) `())
            (report-no-binding-found search-var)
            (let ((saved-vars (car env))
                (saved-vals (cadr env))
                (saved-env (caddr env)))
            (let ((found-in-ribs (find-in-ribs search-var saved-vars saved-vals)))
            (if (eqv? found-in-ribs `())
                (apply-env saved-env search-var)
                found-in-ribs))))))

(define report-no-binding-found
    (lambda (search-var)
        (error `apply-env "No binding for ~s" search-var)))

(define vars (list `a `b `c))
(define vals (list 1 2 3))
; (display (find-in-ribs `b vars vals)) (newline)

(define e 
    (extend-env* `(a b c) `(1 2 3)
        (extend-env* `(d e f) `(4 5 6)
            (empty-env))))
; (display (apply-env e `w)) (newline)
