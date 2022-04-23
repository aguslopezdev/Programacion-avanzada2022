--Ejercicio 1
f = [a] -> Bool
Predicado

1. Todos los elementos de la lista son iguales

f.xs = <V i: 0<= i <#xs : xs.i = xs.0>
f'.xs = <V i: 0<= i < #xs-1 :xs.i = xs.i+1>

si la lista es vacia el rango "#xs" es Falso, devuelve el neutro, en este caso es True

(#xs es la cantidad de elementos de la lista)

2. Todos los elementos de la lista son diferentes

f.xs = <V i,j: 0<=i<#xs ^ 0<=j<#xs ^ i!=j : xs.i != xs.j>
f'.xs = <V i: 0<=i<#xs : <V j: i<j<#xs : xs.j != xs.i>>

3. Los elementos de la lista estan ordenados

f.xs <V i: 0<=i<#xs-1 : xs.i <= xs.i+1>

4.
P:[num] -> Bool

<E i: 0<=i<#xs ^ xs.i=1 : <E j : 0<=j<#xs : xs.j=0>>

5.
productoria *

p=<*i: 0<=i<#xs ^ esPrimo xs.i : xs.i>

--Ejercicio 2
n=<Min i : 0<=i<#xs ^ xs.i=True: i>