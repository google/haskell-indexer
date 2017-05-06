module DataDecl where

data AB = A | B Int

f :: AB -> Int
f A = 0
f (B x) = x

data Record = Record { recInt :: Int }

g :: Record -> Int
g (Record x) = x

h = recInt
