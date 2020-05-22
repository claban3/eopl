(module tests mzscheme

    (provide test-list)

    ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

    (define test-list
        '(
        ;; simple arithmetic
        (positive-const "11" 11)
        (negative-const "-33" -33)
        (simple-arith-1 "-(44,33)" 11)
        (minus "minus(-9)" 9)

        ;; nested arithmetich
        (nested-arith-left "-(-(44,33),22)" -11)
        (nested-arith-right "-(55, -(22,11))" 44)
        (nested-arith-left-plus "+(+(44,33),22)" 99)
        (nested-arith-right-plus "+(55,+(22,11))" 88)

        ;; simple variables
        (test-var-1 "x" 10)
        (test-var-2 "-(x,1)" 9)
        (test-var-3 "-(1,x)" -9)

        ;; simple unbound variables
        (test-unbound-var-1 "foo" error)
        (test-unbound-var-2 "-(x,foo)" error)

        ;; simple conditionals
        (if-true "if zero?(0) then 3 else 4" 3)
        (if-false "if zero?(1) then 3 else 4" 4)

        ;; test dynamic typechecking
        (no-bool-to-diff-1 "-(zero?(0),1)" error)
        (no-bool-to-diff-2 "-(1,zero?(0))" error)
        (no-int-to-if "if 1 then 2 else 3" error)

        ;; make sure that the test and both arms get evaluated
        ;; properly. 
        (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
        (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)

        ;; and make sure the other arm doesn't get evaluated.
        (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
        (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

        ;; simple let
        (simple-let-1 "let x = 3 in x" 3)

        ;; make sure the body and rhs get evaluated
        (eval-let-body "let x = 3 in -(x,1)" 2)
        (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

        ;; check nested let and shadowing
        (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
        (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
        (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

        ;; list processing
        (const-cons "cons(11,12)" (11 . 12))
        (empty-list "emptylist" ())
        (cons-emptylist "cons(1,emptylist)" (1))
        (eval-let-cons "let x = 4 in cons(x,cons(cons(-(x,1),emptylist),emptylist))" (4 (3)))
        (car-list-1 "car(cons(11,12))" 11)
        (car-list-2 "let x = 4 in car(cons(x,cons(cons(-(x,1),emptylist),emptylist)))" 4)
        (cdr-list-1 "cdr(cons(11,12))" 12)
        (cdr-list-1 "cdr(cons(11,cons(12,13)))" (12 . 13))
        (null-pred-true "if null?(emptylist) then 3 else 4" 3)
        (null-pred-false "if null?(cons(3,4)) then 3 else 4" 4)
        (list-consts "list(1,2,3)" (1 2 3))
        (list-exprs "let x = 4 in list(x, -(x,1), -(x,3))" (4 3 1))

        ;; writting procedures
        (simple-lambda "(proc (x) -(x,1) 2)" 1)
        (arg-list-lambda "(proc (x,y) -(x,y) 3 2)" 1)
        (u-combinator "let makemult = proc (maker)
            proc (x)
            if zero?(x)
            then 0
            else -(((maker maker) -(x,1)), -4)
            in let times4 = proc (x) ((makemult makemult) x)
            in (times4 3)" 12)
        (5-factorial "let makefact = proc (maker)
            proc (x)
            if zero?(x)
            then 1
            else *(((maker maker) -(x,1)), x)
            in let factorial = proc (x) ((makefact makefact) x)
            in (factorial 5)" 120)   
        
        (test-old-closure-scope "let func = proc (q) -(q,x) in (func 2)" -8)
        (test-static-binding "let a = 3
            in let p = proc (x) -(x,a)
            in let a = 5
            in -(a,(p 2))" 6)
        (test-dynamic-binding "let a = 3
            in let p = proc (x) -(x,a)
            in let a = 5
            in -(a,(p 2))" 8)
    ))
)