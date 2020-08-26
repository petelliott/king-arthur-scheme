(load "util.scm")
(load "ast.scm")
(load "frontend.scm")

(write (ast->sexp (frontend (read))))
(newline)
