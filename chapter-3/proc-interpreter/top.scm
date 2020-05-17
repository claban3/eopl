(require "drscheme-init.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interpreter.scm")      ; for value-of-program
(require "tests.scm")            ; for test-list
(require (lib "eopl.ss" "eopl"))

(define (test-all) (run-all))

(define run
(lambda (string)
    (value-of-program (scan&parse string))))

(define run-all
(lambda ()
    (run-tests! run equal-answer? test-list)))

(define equal-answer?
(lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
(lambda (sloppy-val)
    (cond
    ((number? sloppy-val) (num-val sloppy-val))
    ((boolean? sloppy-val) (bool-val sloppy-val))
    ((pair? sloppy-val) (pair-val 
        (sloppy->expval (car sloppy-val)) 
        (sloppy->expval (cdr sloppy-val))))
    ((null? sloppy-val) (emptylist))
    (else
        (eopl:error 'sloppy->expval 
                    "Can't convert sloppy value to expval: ~s"
                    sloppy-val)))))

(define run-one
(lambda (test-name)
    (let ((the-test (assoc test-name test-list)))
    (cond
        ((assoc test-name test-list)
        => (lambda (test)
            (run (cadr test))))
        (else (eopl:error 'run-one "no such test: ~s" test-name))))))

(run-all)