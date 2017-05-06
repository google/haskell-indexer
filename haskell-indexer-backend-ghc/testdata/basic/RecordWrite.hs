{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module RecordWrite where

data Rec = Rec
    { foo :: Int
    , bar :: Int
    }

create = Rec 1 2

createRec = let x = 1 in Rec { bar = 2, foo = x }

wildcard =
    let foo = 1
        bar = 3
    in Rec{..}

pun =
    let foo = 1
        bar = 2
    in Rec{foo,..}

update r = r { bar = 1 }

unpackCreate Rec{..} = Rec
    { foo = 1
    , bar = bar
    }

data Multi = A { m :: Int } | B { m :: Int }

multiUpdate mu = mu { m = 5 }
