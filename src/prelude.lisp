(define foldl
  (lambda (func accum lst)
  (cond ((nil? lst) accum)
        (true (foldl func (func accum (car lst)) (cdr lst))))))
(define + (lambda (x . xs ) (foldl int-plus x xs)))
(define - (lambda (x . xs ) (foldl int-minus x xs)))
