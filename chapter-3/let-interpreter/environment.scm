(module environments (lib "eopl.ss" "eopl") 

    ;; builds environment interface, using data structures defined in
    ;; data-structures.scm. 

    (require "data-structures.scm")
    (provide init-env empty-env extend-env apply-env)

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