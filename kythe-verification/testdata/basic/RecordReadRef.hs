{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module RecordReadRef where
{-# ANN module "HLint: ignore Eta reduce" #-}

-- TODO(robinpalotai): define and refer separate bindings for accessors. Maybe
--   add an option to choose if the bindings will overlap, or if one should go
--   on the ::s. So can be switched depending on overlap support from tools.
--   Or this can just be a hint to the frontend (assuming backend provides the
--   alternative span).
-- - @foo defines/binding FieldFoo
-- - @bar defines/binding FieldBar
data Rec = Rec { foo :: Int, bar :: Int }

-- No field references here, only to the introduced local binding (see
-- DataRef.hs for more comments).
unpack (Rec f b) = f

-- - @foo ref FieldFoo
access r = foo r

-- TODO(robinpalotai): revamp record references, and omit field refs originating
--   from wildcard locations (since they are not explicit references?) Also
--   elaborate this section here (AST vs interpretation). What does this being
--   a syntactic sugar imply?
-- TODO(robinpalotai): maybe fix the spans of the wildcard matches to only the
-- double-dots.
-- - @Rec ref CtorR
-- - @"Rec{..}" ref FieldFoo
-- - @"Rec{..}" ref FieldBar
-- - @foo ref FieldFoo
wildcard Rec{..} = foo

-- - @foo ref FieldFoo
punned Rec{foo} =
    -- - @foo ref FieldFoo
    foo

-- - @foo ref FieldFoo
-- - @baz defines/binding VarBaz
reassigned Rec{foo=baz} =
    -- @baz ref VarBaz
    baz

-- - @bar ref FieldBar
-- - @"Rec{bar,..}" ref FieldFoo
rightWild Rec{bar,..} =
    -- - @foo ref FieldFoo
    -- - @bar ref FieldBar
    foo + bar
