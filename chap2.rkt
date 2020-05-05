(require (lib "eopl.ss" "eopl"))
; ---------- Exercise 2.1 ----------
(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successor 
    (lambda (powers)
        (if (null? powers)
            `()
            (if (eqv? 15 (car powers))
                (cons (car powers) (successor (cdr powers)))
                (cons (+ (car powers) 1) (cdr powers))))))

(define predecessor 
    (lambda (powers)
        (if (null? powers)
            `()
            (if (eqv? 0 (car powers))
                (cons (+ (car powers) 15) (predecessor (cdr powers)))
                (cons (- (car powers) 1) (cdr powers))))))

; (display (predecessor `(0 0 15))) (newline)

; ---------- Exercise 2.2 ----------
(define zero (lambda () `(diff (one) (one))))
(define is-zero? (lambda (lst) (equal? lst (zero))))
(define successor 
    (lambda (lst)
        (list `diff lst `(diff (diff (one) (one)) (one)))))

(define predecessor
    (lambda (lst)
        (list `diff lst `one)))

(define diff-tree-plus
    (lambda (n-1 n-2)
        (diff n-1 (diff zero n-2))))

; (display (equal? `(diff (one) (one)) (zero)))
; (display `(diff (one) (one)))
; (display (is-zero? `(diff (one) (one)))) (newline)
; (display (predecessor `(diff (one) (one)))) (newline)
; (display (successor `(diff (one) (one)))) (newline)

; ---------- Exercise 2.5 ----------
; (define empty-env (lambda () `()))
; (define extend-env 
;     (lambda (var val env)
;         (list var val env)))

; (define apply-env
;     (lambda (env search-var)
;         (display env)(newline)
;         (if (eqv? (car env) `())
;             (report-no-binding-found search-var)
;             (let ((saved-var (car env))
;                 (saved-val (cadr env))
;                 (saved-env (caddr env)))
;             (if (eqv? search-var saved-var)
;                 saved-val
;                 (apply-env saved-env search-var))))))

; (define report-no-binding-found
;     (lambda (search-var)
;         (error `apply-env "No binding for ~s" search-var)))

; (define e 
;     (extend-env `d 6
;         (extend-env `c 5
;             (extend-env `b 4
;                 (empty-env)))))

; (display (apply-env e `c))(newline)

; ---------- Exercise 2.11 ----------
(define empty-env (lambda () `(())))
(define extend-env* 
    (lambda (vars vals env)
        (list vars vals env)))

(define find-in-ribs
    (lambda (search-var vars vals)
        (if (eqv? vars `())
            `()
            (if (eqv? (car vars) search-var)
                (car vals)
                (find-in-ribs search-var (cdr vars) (cdr vals))))))

(define apply-env
    (lambda (env search-var)
        (display env)(newline)
        (if (eqv? (car env) `())
            (report-no-binding-found search-var)
            (let ((saved-vars (car env))
                (saved-vals (cadr env))
                (saved-env (caddr env)))
            (let ((found-in-ribs (find-in-ribs search-var saved-vars saved-vals)))
            (if (eqv? found-in-ribs `())
                (apply-env saved-env search-var)
                found-in-ribs))))))

(define report-no-binding-found
    (lambda (search-var)
        (error `apply-env "No binding for ~s" search-var)))

(define vars (list `a `b `c))
(define vals (list 1 2 3))
; (display (find-in-ribs `b vars vals)) (newline)

(define e 
    (extend-env* `(a b c) `(1 2 3)
        (extend-env* `(d e f) `(4 5 6)
            (empty-env))))
; (display (apply-env e `w)) (newline)

; ---------- Exercise 2.19 ----------
(define number->bintree
    (lambda (n)
        (list n `() `())))

(define current-element
    (lambda (tree)
        (car tree)))

(define insert-to-right
    (lambda (n tree)
        (let ((root (car tree))
            (left-tree (cadr tree))
            (right-tree (list n (caddr tree) `())))
        (list root left-tree right-tree))))

(define insert-to-left
    (lambda (n tree)
        (let ((root (car tree))
            (left-tree (list n (cadr tree) `()))
            (right-tree (caddr tree)))
        (list root left-tree right-tree))))

(define move-to-left
    (lambda (tree)
        (cadr tree)))

(define move-to-right
    (lambda (tree)
        (caddr tree)))

(define at-leaf? 
    (lambda (tree)
        (and
            (eqv? `() (move-to-left tree))
            (eqv? `() (move-to-right tree)))))

(define t1 (insert-to-right 14 (insert-to-left 15 (number->bintree 13))))

; (display (number->bintree 13))(newline)
; (display t1)(newline)
; (display (move-to-right t1))(newline)
; (display (at-leaf? t1))(newline)
; (display (at-leaf? (move-to-left t1)))(newline)

; ---------- Exercise 2.22 ----------
(define-datatype stack-type is-stack?
    (empty-stack)
    (push (datum always?)
          (saved-stack is-stack?)))

(define top
    (lambda (stack)
        (cases stack-type stack
            (push (datum saved-stack) datum)
            (empty-stack () (error "Cannot take top of empty stack")))))

(define pop
    (lambda (stack)
        (cases stack-type stack
            (push (datum saved-stack) saved-stack)
            (empty-stack () (error "Cannot pop empty stack")))))

(define stack (push 2 (push 1 (empty-stack))))
; (display (top stack)) (newline)
; (display (top (empty-stack))) (newline)
; (display (top (pop stack))) (newline)
; (display (pop (empty-stack))) (newline)

; ---------- Exercise 2.24 ----------
(define-datatype bintree bintree?
    (leaf-node
        (num integer?))
    (interior-node
        (key symbol?)
        (left bintree?)
        (right bintree?)))

(define bintree-to-list
    (lambda (tree)
        (cases bintree tree
            (leaf-node (num) (list `leaf-node num))
            (interior-node (key left right) 
                (list `interior-node key 
                    (bintree-to-list left) 
                    (bintree-to-list right))))))


(define t1 
    (interior-node `a 
        (interior-node `b 
            (leaf-node 3) (leaf-node 4))
        (leaf-node 5)))

; (display (bintree-to-list (interior-node `a (leaf-node 3) (leaf-node 4))))(newline)
; (display (bintree-to-list t1))(newline)

; ---------- Exercise 2.26 ----------

(define list-type?
    (lambda (pred?)
        (lambda (lst)
            (if (eqv? lst `())
                #t
                (if (pred? (car lst))
                    (list-type? (cdr lst))
                    #f)))))
        
(define-datatype red-blue-tree red-blue-tree?
    (red-node
        (left red-blue-tree?)
        (right red-blue-tree?))
    (blue-node
        (subtree-list (list-type? red-blue-tree?)))
    (leaf-node 
        (num integer?)))

(define replace-leaf-red-count-list
    (lambda (subtree-list n)
        (if (eqv? subtree-list `())
            `()
            (list 
                (replace-leaf-red-count (car subtree-list) n)
                (replace-leaf-red-count-list (cdr subtree-list) n)))))

(define replace-leaf-red-count
    (lambda (tree n)
        (cases red-blue-tree tree
            (leaf-node (num) (leaf-node n))
            (red-node (left right) 
                (red-node
                    (replace-leaf-red-count left (+ 1 n))
                    (replace-leaf-red-count right (+ 1 n))))
            (blue-node (subtree-list) (blue-node (replace-leaf-red-count-list subtree-list n))))))

(define t2 (red-node (leaf-node 5) 
    (blue-node 
        (list (red-node (leaf-node 100) (leaf-node 100)) (leaf-node 10)))))
; (display (replace-leaf-red-count t2 0))(newline)

; ---------- Exercise 2.31 ----------
(define-datatype prefix-exp prefix-exp?
    (const-exp
        (num integer?))
    (diff-exp
        (operand1 prefix-exp?)
        (operand2 prefix-exp?)))

(define parse-prefix-exp
    (lambda (prefix-list)
        (let 
            ((head (car prefix-list))
            (tail (cdr prefix-list)))
            (cond
                ((integer? head) 
                    (cons (const-exp head) tail))
                ((eqv? head `-)
                    (let* 
                        ((operand-1-rest (parse-prefix-exp tail))
                        (operand-1 (car operand-1-rest))
                        (operand-2-rest (parse-prefix-exp (cdr operand-1-rest)))
                        (operand-2 (car operand-2-rest))
                        (rest-2 (cdr operand-2-rest)))
                        (cons (diff-exp operand-1 operand-2) rest-2)))))))

(define parse-prefix-list
    (lambda (prefix-list)
        (let* 
            ((expr-rest (parse-prefix-exp prefix-list))
            (expr (car expr-rest))
            (rest (cdr expr-rest)))
            (if (null? rest)
                expr
                (error `parse-prefix-list "Bad return ~s" rest)))))

; (display (parse-prefix-list `(- - 3 2 - 4 - 12 7)))(newline)
