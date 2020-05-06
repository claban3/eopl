(module environment

    (require (lib "eopl.ss" "eopl"))
    (require "data-structures.scm")
    (provide (all-defined-out))

    (define empty-env (lambda () `()))

    (define extend-env 
        (lambda (var val env)
            (list var val env)))

    (define apply-env
        (lambda (env search-var)
            (if (eqv? (car env) `())
                (report-no-binding-found search-var)
                (let ((saved-var (car env))
                    (saved-val (cadr env))
                    (saved-env (caddr env)))
                (if (eqv? search-var saved-var)
                    saved-val
                    (apply-env saved-env search-var))))))

    (define report-no-binding-found
        (lambda (search-var)
            (error `apply-env "No binding for ~s" search-var)))

    (define init-env
        (extend-env `i (num-val 1)
            (extend-env `v (num-val 5)
                (extend-env `x (num-val 10)
                    (empty-env)))))

)