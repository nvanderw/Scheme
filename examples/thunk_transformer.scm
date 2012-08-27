(let ((map ; map higher-order function
  (let ((mapR
    (lambda (this f xs)
      (if (null? xs)
          '()
          (cons (f (car xs))
                (this this f (cdr xs)))))))
    (mapR mapR))))

  (let ((~thunk ; Define our syntax transformer
    (lambda (form) ; Takes a single argument: its form
      (let ((body (car (cdr form)))) ; Second element of form -> body
           (map ; Use a map to replace ? in following form with body
             (lambda (id)
               (if (eq? id '?)
                   body
                   id))
             '(lambda () ?))))))

    ((thunk (+ 1 2))))) ; Create a thunk and evaluate it
