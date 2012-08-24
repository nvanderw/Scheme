(let ((append
 (let ((appendR
  (lambda (this x y)
    (if (null? x)
        y
        (cons
          (car x)
          (this this (cdr x) y))))))
   (appendR appendR))))
  (append '(1 2 3) '(4 5 6)))
