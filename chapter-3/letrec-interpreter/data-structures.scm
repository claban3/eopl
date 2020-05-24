(module data-structures (lib "eopl.ss" "eopl")

    (require "lang.scm")
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

    (define-datatype proc proc?
        (procedure
            (bvar symbol?)
            (body expression?)
            (env environment?)))

    (define-datatype environment environment?
        (empty-env)
        (extend-env 
            (bvar symbol?)
            (bval expval?)
            (saved-env environment?))
        (extend-env-rec
            (id symbol?)
            (bvar symbol?)
            (body expression?)
            (saved-env environment?)))

)
