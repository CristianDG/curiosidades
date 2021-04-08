# Interpretador de expressões lógicas

## pre-requisitos

Ter o [interpretador MIT/GNU scheme](https://www.gnu.org/software/mit-scheme/)
## modo de uso

dentro do interpretador carregue o arquivo escrevendo `(load "logic.scm")`

e depois, é só utilizar a função `eval-logic` dessa forma:

`(eval-logic <expressão> <tabela verdade>)` onde `<expressão>` é uma lista de simbolos como `'(p or not p)` ou `'((1 ou 0) ou sla)` e `<tabela verdade>` pode ter somente dois valores: `#f` para não mostrar a tabela verdade e `#t` para mostrar

