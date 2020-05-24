(module environments (lib "eopl.ss" "eopl") 

    ;; builds environment interface, using data structures defined in
    ;; data-structures.scm. 

    (require "data-structures.scm")
    (provide apply-env init-env extend-env extend-env extend-env-rec)

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
    (define apply-env
        (lambda (env search-sym)
            (cases environment env
                (empty-env ()
                    (eopl:error 'apply-env "No binding for ~s" search-sym))
                (extend-env (var val saved-env)
                    (if (eqv? search-sym var)
                        val
                        (apply-env saved-env search-sym)))
                (extend-env-rec (p-name b-var p-body saved-env)
                    (if (eqv? search-sym p-name)
                        (proc-val (procedure b-var p-body env))          
                        (apply-env saved-env search-sym))))))

)