
; não gostei dessa função
(define (member? s l)
  (if (member s l) #t #f))

(define (init lst)
  (if (null? lst)
      '()
      (reverse (cdr (reverse lst)))))

(define (true-symbol? s)
  (member? s '(1 true ~0 ¬0)))

(define (false-symbol? s)
  (member? s '(0 false ~1 ¬1)))

(define (not-symbol? s)
  (member? s '(~ ¬ not)))

(define (and-symbol? s)
  (member? s '(∧ and ^)))

(define (or-symbol? s)
  (member? s '(∨ or v)))

(define (directional-symbol? s)
  (member? s '(-> →)))

(define (bidirectional-symbol? s)
  (member? s '(= <-> ↔)))


;criar uma syntax tree
;
;de     0 ∨ 1  ∧ 1
;para ((0 ∨ 1) ∧ 1)
;
;de     0 ∨  1 -> 0   ∧ 1
;para ((0 ∨ (1 -> 0)) ∧ 1)


(define (make-tree tokens precedence-list)
  (define (aux left right operator?)
    (cond
      ((null? right) left)
      ((not-symbol? (car right))
       (aux (append left (list (list (car right) (cadr right))))
            (cddr right)
            operator?))
      ((operator? (car right))
       (if (not-symbol? (cadr right))
           (aux
             (append
               (init left)
               (list (list
                       (last left)
                       (car right)
                       (list (cadr right) (caddr right)))))
             (cdddr right)
             operator?)
           (aux
             (append
               (init left)
               (list (list (last left) (car right) (cadr right))))
             (cddr right)
             operator?)))
      (else
        (aux (append left (list (car right)))
             (cdr right)
             operator?))))
  (fold
    (lambda (predicate parsed-tokens)
      (aux '() parsed-tokens predicate))
    tokens
    precedence-list))

(define precedence-list
  (list
    directional-symbol?
    bidirectional-symbol?
    or-symbol?
    and-symbol?))

(define (eval-tree tree env)
  (cond
     ; se for um atomo
    ((assoc tree env) (cdr (assoc tree env)))
    ((true-symbol? tree)          #t)
    ((false-symbol? tree)         #f)
    ((not-symbol? tree)           (lambda (r) (not r)))
    ((and-symbol? tree)           (lambda (l r) (and l r)))
    ((or-symbol? tree)            (lambda (l r) (or l r)))
    ((directional-symbol? tree)   (lambda (l r) (if l r #t)))
    ((bidirectional-symbol? tree) (lambda (l r) (eq? l r)))
    ((list? tree)
           ; se for uma lista contendo uma lista
     (cond ((= 1 (length tree)) (eval-tree (car tree) env))
           ; se for uma lista contendo um ~
           ((= 2 (length tree)) ((eval-tree (car tree) env)
                                   (eval-tree (cadr tree) env)))
           ; se for uma lista de 3 argumentos:
           ; 2 expressões e 1 operador
           (else ((eval-tree (cadr tree) env)
                    (eval-tree (car tree) env)
                    (eval-tree (caddr tree) env)))))
    (else (error "erro!"))))
;(trace eval-tree)

(define (eval-logic tokens)
  (eval-tree (make-tree tokens precedence-list) consts))

(define (eval-with-vars tokens truth-table)
  (map (lambda (enviroment)
         (let ((result (eval-tree
                         (make-tree tokens precedence-list)
                         (append consts enviroment))))
           (if truth-table 
               (list enviroment result)
               result)))
       (make-envs tokens)))
;(trace eval-with-vars)

(define (make-envs tokens)
  (let ((vars (find-variables tokens)))
    (if (= 0 (length vars))
        '(())
        (let* ((vars (find-variables tokens))
               (combination-size (-1+ (expt 2 (length vars))))
               (combinations 
                 (map (lambda (i)
                        (map int->bool
                             (to-fixed-length (-1+ combination-size)
                                              (to-binary-list i))))
                      (0-to combination-size)))
               (envs (map (lambda (l) (map cons vars l)) combinations)))
          envs))))

; (to-binary-list 8) => (1 0 0 0)
(define (to-binary-list n)
  (cond ((= n 0) '(0))
        ((= n 1) '(1))
        (else (append
                (to-binary-list (quotient n 2))
                (list (if (odd? n) 1 0))))))

(define (int->bool i)
  (not (= i 0)))


(define (to-fixed-length n lst)
  (cond ((= n (length lst)) lst)
        ((> n (length lst)) (to-fixed-length n (cons 0 lst)))
        ((< n (length lst)) (to-fixed-length n (cdr lst)))))


(define (find-variables tree )
  (cond ((null? tree) '())
        ((list? (car tree))
         (lset-union eq?
                     (find-variables (car tree) )
                     (find-variables (cdr tree) )))
        ((not (or (true-symbol? (car tree))
                  (false-symbol? (car tree))
                  (not-symbol? (car tree))
                  (and-symbol? (car tree))
                  (or-symbol? (car tree))
                  (directional-symbol? (car tree))
                  (bidirectional-symbol? (car tree))))
         (lset-union eq?
                     (list (car tree))
                     (find-variables (cdr tree) )))
        (else (find-variables (cdr tree) ))))

; (0-to 3) => (0 1 2 3)
(define (0-to n)
  (if (= 0 n)
      '(0)
      (append (0-to (-1+ n)) (list n))))

; eval-with-vars
; (((~ P) ∧ (~ Q)) = (~ (P ∨ Q)))

; vira:
; (( 0 0 )
;  ( 0 1 )
;  ( 1 0 )
;  ( 1 1 ))

; no caso P recebe o primeiro valor e Q o segundo resultando em:
; ( #t #t #t #t )

; Teste de implementação
; extraindo as variaveis:
; de (((~ P) ∧ (~ Q)) = (~ (P ∨ Q))) para (P Q) => find-variables
; associando com um array de combinações
; (( 0 0 )
;  ( 0 1 )
;  ( 1 0 )
;  ( 1 1 ))
; ficando:
; (( (P . 0) (Q . 0) )
;  ( (P . 0) (Q . 1) )
;  ( (P . 1) (Q . 0) )
;  ( (P . 1) (Q . 1) ))
; no caso seria usando (map cons '(P Q) |a lista|)
; ideia 1:
; pra cada iteração da função mapear os simbolos para os valores

; acredito que seria interessante extender a função
; eval-logic pra receber um array associativo que seria o "ambiente"

; quase deu bom, só que deve fazer a recursão dentro das outras listas
;(define (sla)
;  (map (lambda (env)
;         (map (lambda (i) (if (assoc i env) (cdr (assoc i env)) i)) |teste 3|)) teste))


; Coisas a fazer
; ter um modo de rodar o teste 3: FEITO
;
; ter um modo de dizer se a operação é binária ou não
;   preferencialmente de antemão
;
; no lugar de comparar por simbolos usar um predicado,
; assim da pra adicionar simbolos facilmente: FEITO

(define teste-1 '((0 ∨ 1) ∧ 1)) ; #t
(define teste-2 '(0 ∨ 1 ∧ 1)) ; #t
(define teste-3 '(((~ P) ∧ (~ Q)) = (~ (P ∨ Q)))) ; (#t #t #t #t)
(define teste-4 '(~ (1 ∨ 0)))
(define teste-5 '(~ (p ∨ q)))
(define teste-6 '(not (1 or 0)))
(define teste-7 '(((not P) and (not Q)) = (not (P or Q))))


