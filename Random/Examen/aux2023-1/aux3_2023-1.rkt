#lang play

; P1. Ya se hizo

; P2.
; Si es un num o id, devuelve el mismo num y id. Si no, se aplica obfuscate a las expresiones y cuando es un with, se define un new con gensym y se devuelve la misma expresion with
; con ese new y se aplica recursivamente obfuscate al cuerpo

; P3.
; a) Si es el mismo simbolo que en la expresion, se devuelve el with haciendo la substitucion, si no, se devuelve el with haciendo la substitucion y el body se hace la substitucion
; pero aumentando el val en 1.

; P4.
; Un interp pero se hace brujin de las expr que hayan (en el add, en el body del with, en el value del id del with, etc). En el body del with se hace substs con 0
