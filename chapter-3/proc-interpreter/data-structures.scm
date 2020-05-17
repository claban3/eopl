(module data-structures (lib "eopl.ss" "eopl")

    (provide (all-defined-out))

    (define list-of
    (lambda (pred?)
        (lambda (lst)
            (if (eqv? lst `())
                #t
                (if (pred? (car lst))
                    (list-of (cdr lst))
                    #f)))))
    
    (define-datatype expval expval?
        (emptylist)
        (num-val
            (value number?))
        (bool-val
            (boolean boolean?))
        (pair-val
            (thecar expval?)
            (thecdr expval?))
        (proc-val
            (proc proc?))
        )

    (define proc? procedure?)

    ;; expval->num : ExpVal -> Int
    (define expval->num
        (lambda (v)
            (cases expval v
                (num-val (num) num)
                (else (expval-extractor-error 'num v)))))

    ;; expval->bool : ExpVal -> Bool
    (define expval->bool
        (lambda (v)
            (cases expval v
                (bool-val (bool) bool)
                (else (expval-extractor-error 'bool v)))))

    ;; expval->pair : ExpVal -> Pair
    (define expval->pair
        (lambda (v)
            (cases expval v
                (pair-val (thecar thecdr) (cons thecar thecdr))
                (else (expval-extractor-error 'pair v)))))

    ;; expval->proc : ExpVal -> Procedure
    (define expval->proc
        (lambda (v)
            (cases expval v
                (proc-val (proc) proc)
                (else (expval-extractor-error 'proc v)))))

    (define expval-extractor-error
        (lambda (variant value)
            (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
        	variant value)))

    (define empty-env-record
        (lambda () 
            '()))

    (define extended-env-record
        (lambda (sym val old-env)
            (cons (list sym val) old-env)))

    (define empty-env-record? null?)

    (define environment?
        (lambda (x)
            (or (empty-env-record? x)
                (and (pair? x)
                    (symbol? (car (car x)))
                    (expval? (cadr (car x)))
                    (environment? (cdr x))))))

    (define extended-env-record->sym
        (lambda (r)
            (car (car r))))

    (define extended-env-record->val
        (lambda (r)
            (cadr (car r))))

    (define extended-env-record->old-env
        (lambda (r)
            (cdr r)))
)
