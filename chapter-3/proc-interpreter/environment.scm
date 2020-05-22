(module environments (lib "eopl.ss" "eopl") 

    ;; builds environment interface, using data structures defined in
    ;; data-structures.scm. 

    (require "data-structures.scm")
    (provide init-env empty-env extend-env apply-env extend-env-list)

    ;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
    (define init-env 
    (lambda ()
        (extend-env 
            'i (num-val 1)
        (extend-env
            'v (num-val 5)
        (extend-env
            'x (num-val 10)
            (empty-env))))))

    ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

    (define empty-env
        (lambda ()
            (empty-env-record)))

    (define empty-env? 
        (lambda (x)
            (empty-env-record? x)))

    (define extend-env
        (lambda (sym val old-env)
            (extended-env-record sym val old-env)))
    
    (define extend-env-list
        (lambda (syms vals old-env)
            (cond 
                ((and (null? syms) (null? vals))
                    old-env)
                ((and (not (null? syms)) (not (null? syms)))
                    (extend-env 
                        (car syms) 
                        (car vals) 
                        (extend-env-list (cdr syms) (cdr vals) old-env)))
                (else eopl:error 'extend-env-list 
                    "Unequal length of syms and vals: ~s ~s" syms vals))))

    (define apply-env
        (lambda (env search-sym)
            (if (empty-env? env)
                (eopl:error 'apply-env "No binding for ~s" search-sym)
                (let ((sym (extended-env-record->sym env))
                    (val (extended-env-record->val env))
                    (old-env (extended-env-record->old-env env)))
                    (if (eqv? search-sym sym)
                        val
                        (apply-env old-env search-sym))))))

)