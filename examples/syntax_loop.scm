; Repeatedly prints a string using the macro transformer functionality
(let ((~hello
  (lambda (form)
    '(begin
       (display "Hello, world!\n")
       (hello)))))
  (hello))
