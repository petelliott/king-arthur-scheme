(load "ast.scm")
(load "frontend.scm")

(write (object->ast (read)))
(newline)
