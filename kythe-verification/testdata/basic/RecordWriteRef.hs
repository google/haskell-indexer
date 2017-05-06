{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module RecordWrite where

data Rec = Rec
    -- - @foo defines/binding FieldFoo
    { foo :: Int
    -- - @bar defines/binding FieldBar
    , bar :: Int
    }

create = Rec 1 2

-- - @foo ref FieldFoo
-- - @bar ref FieldBar
createRec = let x = 1 in Rec { bar = 2, foo = x }

wildcard =
    -- - @foo ref FieldFoo
    let foo = 1
        -- - @bar ref FieldBar
        bar = 3
    -- - @"Rec{..}" ref FieldFoo
    -- - @"Rec{..}" ref FieldBar
    in Rec{..}

pun =
    let foo = 1
        bar = 2
    -- - @foo ref FieldFoo
    -- - @"Rec{foo,..}" ref FieldBar
    in Rec{foo,..}

-- - @bar ref FieldBar
update r = r { bar = 1 }

unpackCreate Rec{..} = Rec
    -- - @foo ref FieldFoo
    { foo = 1
    -- - @bar ref FieldBar
    , bar =
          bar
    }

data Multi
    -- - @m defines/binding FieldM
    = A { m :: Int }
    -- Note: no bindings / refs at @m.
    | B { m :: Int }

multiUpdate mu = mu
    -- - @m ref FieldM
    { m = 5 }
