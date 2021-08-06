(define (split-at lst n) 
    (let (a (split-at (cdr lst) (- n 1)))
        (cons(cons (car lst) (car a)) (cdr a))))

(define (compose-all funcs)
    (lambda (x)
        (if (null? funcs) x
        (compose-all (cdr funcs)) 
        ((car funcs) x))))