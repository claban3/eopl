
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

    ;; value-of : Exp * Env -> ExpVal
    (define value-of
    (lambda (exp env)
        (cases expression exp

        (const-exp (num) (num-val num))
        
        (var-exp (var) (apply-env env var))
        
        (diff-exp (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                    (num2 (expval->num val2)))
                (num-val
                (- num1 num2)))))
        
        ; ---------- Exercise 3.6 ----------
        (minus (expr)
            (let* ((val (value-of expr env))
                (num (expval->num val)))
                (- 0 num)))

        ; ---------- Exercise 3.7 ---------- 
        
        (zero?-exp (exp1)
            (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
                (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
                
        (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
                (value-of exp2 env)
                (value-of exp3 env))))

        (let-exp (var exp1 body)       
            (let ((val1 (value-of exp1 env)))
            (value-of body
                (extend-env var val1 env))))

        )))
)
