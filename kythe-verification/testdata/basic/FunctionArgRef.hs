module FunctionArgRef where

-- Test simple argument reference.
-- - @a defines/binding ParamFA
f a =
    -- - @a ref ParamFA
    a

-- Test pattern-matched argument reference.
-- - @a defines/binding ParamGA
-- - @b defines/binding ParamGB
g (a,b) =
    -- - @a ref ParamGA
    -- - @b ref ParamGB
    a + b
