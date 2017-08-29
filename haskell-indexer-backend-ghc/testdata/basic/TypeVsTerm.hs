module TypeVsTerm where

data X = X Int -- same name

toTerm :: X
toTerm = X 42
