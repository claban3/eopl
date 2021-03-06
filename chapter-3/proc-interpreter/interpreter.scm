(module interpreter (lib "eopl.ss" "eopl")

    (require "drscheme-init.scm")

    (require "lang.scm")
    (require "data-structures.scm")
    (require "environment.scm")

    (provide value-of-program value-of)

    ;; value-of-program : Program -> ExpVal
    (define value-of-program 
        (lambda (pgm)
            (cases program pgm
            (a-program (exp1)
                (value-of exp1 (init-env))))))

    (define eval-cast
        (lambda (exp1 exp2 env)
            (lambda (operator)
                (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                    (num-val
                    (operator num1 num2)))))))

    (define cons-list   
        (lambda (exp-list env)
            (if (null? exp-list)
                (emptylist)
                (let ((val-car (value-of (car exp-list) env)))
                    (pair-val 
                        val-car 
                        (cons-list (cdr exp-list) env))))))

    ;; value-of : Exp * Env -> ExpVal
    (define value-of
        (lambda (exp env)
            (cases expression exp

            (const-exp (num) (num-val num))
            
            (var-exp (var) (apply-env env var))

            (diff-exp (exp1 exp2)
                ((eval-cast exp1 exp2 env) - ))

            (minus (exp1)
                (let* ((val (value-of exp1 env))
                    (num (expval->num val)))
                    (num-val (- 0 num))))

            (plus-exp (exp1 exp2)
                ((eval-cast exp1 exp2 env) + ))
            
            (mult-exp (exp1 exp2)
                ((eval-cast exp1 exp2 env) * ))
            
            (div-exp (exp1 exp2)
                ((eval-cast exp1 exp2 env) / ))
            
            (zero?-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                (let ((num1 (expval->num val1)))
                    (if (zero? num1)
                    (bool-val #t)
                    (bool-val #f)))))
                    
            (null?-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                    (cases expval val1
                        (emptylist () (bool-val #t))
                        (else (bool-val #f)))))
                
            (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))

            (let-exp (var exp1 body)       
                (let ((val1 (value-of exp1 env)))
                (value-of body
                    (extend-env var val1 env))))

            (emptylist-exp () (emptylist))

            (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))                
                    (pair-val val1 val2)))

            (car-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                (let ((pair1 (expval->pair val1)))
                    (car pair1))))

            (cdr-exp (exp1)
                (let ((val1 (value-of exp1 env)))
                (let ((pair1 (expval->pair val1)))
                    (cdr pair1))))

            (list-exp (exp-list)
                (cons-list exp-list env))

            (proc-exp (var-list body)
                (proc-val (procedure var-list body)))

            (call-exp (rator rand-list)
                (let ((proc (expval->proc (value-of rator env)))
                    (args (map ((curry2 value-of) env) rand-list)))
                        (apply-procedure proc args env)))

        )))

    (define curry2
        (lambda (proc)
            (lambda (arg1)
                (lambda (arg2)
                    (proc arg2 arg1)))))

    ;; Instead of using recursive data types, we are using 
    ;; scheme's procedural datatype. 
    (define procedure 
        (lambda (var-list body)
            (lambda (val-list env)
                (value-of body (extend-env-list var-list val-list env)))))

    (define apply-procedure
        (lambda (proc args env)
            (proc args env)))

)
