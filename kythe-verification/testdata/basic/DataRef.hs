module DataRef where

-- TODO(robinpalotai): - CtorA childof TypeD
-- TODO(robinpalotai): - CtorB childof TypeD
-- TODO(robinpalotai): maybe have `FieldInt childof CtorB` with an implicit
--   anchor (or an overloaded anchor @Int) defines/binding it.
-- - @D defines/binding TypeD
-- - @A defines/binding CtorA
-- - @B defines/binding CtorB
-- - @Int ref _
data D = A | B Int

-- - @D ref TypeD
f :: D -> Int
-- - @A ref CtorA
f A = 0
-- No reference from 'x' to the field, since Kythe only adds references for
-- explicitly referred nodes.
-- - @B ref CtorB
-- - @x defines/binding PatternX
f (B x) =
    -- - @x ref PatternX
    x

-- @Record defines/binding TypeR
data Record =
    -- TODO(robinpalotai): - CtorR childof TypeR
    -- TODO(robinpalotai): - FieldR childof CtorR
    -- TODO(robinpalotai): - FieldR childof TypeR, at least for fields that are
    --   shared by all constructors? Tools would like it probably.
    -- - @Record defines/binding CtorR
    -- - @recInt defines/binding FieldR
    Record { recInt :: Int }

-- - @Record ref TypeR
g :: Record -> Int
-- - @Record ref CtorR
g (Record x) = x

-- - @recInt ref FieldR
h = recInt

-- Banging fields (and some other things) makes the Wrapper Id to be used
-- instead of the Worker / DataCon Id. If we don't handle this, we wouldn't
-- be able to find reference to the data constructor.
-- - @MX defines/binding CtorMX
data X a = MX { unX :: !a }

-- - @MX ref CtorMX
makeX = MX 5
