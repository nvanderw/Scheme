(let
  ; Function which takes n and produces the nth Fibonacci number 0,1,1,2,3,...
  ((fibs
    ; Inner function which takes itself as its first argument to perform
    ; a recursive call
    (let ((fibsR
      (lambda (this n)
        (if (< n 2)
            n
            (+
              (this this (- n 1))
              (this this (- n 2)))))))
      ; Partially apply fibR to itself to get our outer function
      (fibsR fibsR))))
  (fibs 20)) ; Get the 20th (zero-indexed) Fibonacci number
