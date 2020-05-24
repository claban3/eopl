(module lang (lib "eopl.ss" "eopl")

    (require "drscheme-init.scm")
    (provide (all-defined-out))

    (define the-lexical-spec
        '((whitespace (whitespace) skip)
            (comment ("%" (arbno (not #\newline))) skip)
            (identifier
            (letter (arbno (or letter digit "_" "-" "?")))
            symbol)
            (number (digit (arbno digit)) number)
            (number ("-" digit (arbno digit)) number)))

    (define the-grammar
        '((program (expression) a-program)

            (expression (number) const-exp)
            
            ; (expression 
            ; ("(" (separated-list expression ",") ")")
            ; s-expression)

            (expression
            ("-" "(" expression "," expression ")")
            diff-exp)

            (expression
            ("+" "(" expression "," expression ")")
            plus-exp)

            (expression
            ("*" "(" expression "," expression ")")
            mult-exp)

            (expression
            ("/" "(" expression "," expression ")")
            div-exp)

            (expression
            ("minus" "(" expression ")")
            minus)

            (expression
            ("zero?" "(" expression ")")
            zero?-exp)

            (expression
            ("null?" "(" expression ")")
            null?-exp)

            (expression
            ("if" expression "then" expression "else" expression)
            if-exp)

            (expression (identifier) var-exp)

            (expression
            ("let" identifier "=" expression "in" expression)
            let-exp)

            (expression
            ("emptylist")
            emptylist-exp)

            (expression
            ("cons" "(" expression "," expression ")")
            cons-exp)

            (expression
            ("car" "(" expression ")")
            car-exp)

            (expression
            ("cdr" "(" expression ")")
            cdr-exp)

            (expression
            ("list" "("(separated-list expression ",") ")")
            list-exp)
        ))


    (sllgen:make-define-datatypes the-lexical-spec the-grammar)

    (define show-the-datatypes
        (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

    (define scan&parse
        (sllgen:make-string-parser the-lexical-spec the-grammar))

    (define just-scan
        (sllgen:make-string-scanner the-lexical-spec the-grammar))

)