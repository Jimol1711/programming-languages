#lang play

#|
       SUPER PAUTA - Auxiliar #3

================================================
P1
================================================

a) Tenemos:
{with {x 5}
    {+  {with {y 6} 
              {with {x {+ 1 y}}
                    {+ x y}}}
        {with {y {+ x 5}}
               y}}}
Veamos primero el proceso utilizando sustitución
directa.
-- 1. SUBST (x 5)
{+  {with {y 6} 
          {with {x {+ 1 y}}   ;; este es otro x!
                {+ x y}}}     ;; otro x
    {with {y {+ 5 5}}
            y}}

-- 2. SUBST (y 6)
{+  {with {x {+ 1 6}}
          {+ x 6}}}
    {with {y {+ 5 5}}         ;; este es otro y
           y}}                ;; same

-- 3. SUBST (x 7)
{+  {+ 7 6}
    {with {y {+ 5 5}}       
           y}}

-- 4. SUBST (y 10)
{+  13
    10}

-- 5. Solve
23


Si ahora utilizamos ambientes, el proceso sería
similar a lo siguiente:
-- 1. Ambiente Vacío
EXPR                              ENVIRONMENT
{with {x 5}                       []
    {+  {with {y 6} 
              {with {x {+ 1 y}}
                    {+ x y}}}
        {with {y {+ x 5}}
               y}}}

-- 2. Extend-Env (x 5)            ENVIRONMENT
{+  {with {y 6}                   [x->5]
          {with {x {+ 1 y}}
                {+ x y}}}
    {with {y {+ x 5}}             [x->5]
           y}}}
Se añade la asociación de x con 5 en el ambiente.

-- 3. Extend-Env (y 6)            ENVIRONMENT
{+  {with {x {+ 1 y}}             [y->6;x->5]
          {+ x y}}}
    {with {y {+ x 5}}             [x->5]
           y}}}
Se añade la asociación de y con 6 en el ambiente, sin
embargo esto solo se propaga en el primer sumando.
Al necesitar el valor de y, se busca en el ambiente,
devolviendo así el valor 6.

-- 4. Extend-Env (x 7)            ENVIRONMENT
{+  {+ x y}                       [x->7;y->6;x->5]
    {with {y {+ x 5}}             [x->5]
           y}}}
Finalmente, para resolver la suma se buscan tanto
x e y en el ambiente, encontrando que x vale 7
e y tiene el valor 6. Notar que la búsqueda es desde
los más recientemente agregados hacia atrás y no al
revés.

-- 5. Extend-Env (y 10)           ENVIRONMENT
{+  13                            [x->7;y->6;x->5]
    {with {y {+ x 5}}             [y->10;x->5]
           y}}}
Se añade la asociación y=10, de modo que al evaluar
y, se busca en el ambiente y se devuelve un 10

-- 6. Solve
{+ 13 10}
23

b) En base a lo anterior, podemos decir que evaluar
mediante sustitución directa o mediante sustitución
diferida produce el mismo resultado. La principal
diferencia entre utilizar sustitución directa y un ambiente
(o repositorio) recae en un tema de eficiencia.
|#


