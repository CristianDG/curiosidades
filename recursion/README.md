
# Recursividade

## O que é:

É um modo de resolver problemas dividindo eles em problemas menores

## Como funciona?

Simplesmente chamando a função na sua propria definição. *(existem outros modos mas esse é o mais simples)*

## Casos de uso:

Recursão pode ser usada principalmente para resolver problemas de um tamanho não especificado *(mesma ideia de um while)*.

Alguns exemplos são:
- Fatorial
- Sequencia de Fibonacci
- Pesquisa binária


## Tipos:

Existem vários tipos de recursão, os mais famosos são **Simples** e **Em cauda**

### Recursão simples

A recursão simples é quando a chamada recursiva é uma parte do retorno

Pros:
- É simples de escrever
- Você controla a ordem de execução

Contras:
- A memoria acumula a cada chamada (não é boa para entradas grandes)


#### Exemplos:

Fatorial:

```py
def fatorial(x):
    if x == 0:
        return 1
    else:
        return x * fatorial(x-1)
```
```hs
-- Em haskell é mais simples ainda
fatorial 0 = 1
fatorial x = x * fatorial (x-1)
```

Bora analizar a memoria ao chamar `fatorial(5)`:
```
fatorial(5)
-> 5 * fatorial(4)
-> 5 * 4 * fatorial(3)
-> 5 * 4 * 3 * fatorial(2)
-> 5 * 4 * 3 * 2 * fatorial(1)
-> 5 * 4 * 3 * 2 * 1 * fatorial(0)
-> 5 * 4 * 3 * 2 * 1 * 1
-> 20 * 3 * 2 * 1 * 1
-> 60 * 2 * 1 * 1
-> 120 * 1 * 1
-> 120 * 1
-> 120
```

Esse estilo de recursão não é muito performante mas isso não significa que é essencialmente ruim,
em alguns casos é necessário manter na memória a fila de execução.

### Recursão em cauda

Pros:
- A memoria não acumula

Contras:
- Só existe uma ordem de execução
- É um pouco mais complicado de escrever

## Outros assuntos (Wip)
- Recrusão é um assunto muito importante principalmente no paradígma funcional.
- É equivalente aos iteradores (for, while)

