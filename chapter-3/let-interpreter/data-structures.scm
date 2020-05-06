(module data-structures

    (require (lib "eopl.ss" "eopl"))
    (provide (all-defined-out))
    
    (define-datatype expval expval?
        (num-val
            (value number?))
        (bool-val
            (boolean boolean?)))

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

    (define expval-extractor-error
        (lambda (variant value)
            (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
        	variant value)))
)
