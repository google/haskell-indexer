module FunCall where

f = id
fCaller = f 1

g = id
gReferrer = g

h = id
hMixed = h h
