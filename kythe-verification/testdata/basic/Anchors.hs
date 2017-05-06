module Anchors where

-- Anchors are childof the files they are defined in.
-- - @f defines/binding FunF
-- - @f childof File
-- - File.node/kind file
f =
    -- Kythe usually assign source anchors of ref/calls to their parent context.
    -- - @callMeMaybe childof FunF
    -- - @callMeMaybe childof File
    -- - @undefined childof File
    callMeMaybe undefined

-- - @g defines/binding FunG
-- - @x childof FunG
g = x
  -- Even calls of local bindings are put under the top-level scope, since
  -- fine-grained scoping is not a goal for Kythe.
  -- - @callMeMaybe childof FunG
  -- - @x childof File
  where x = callMeMaybe undefined

callMeMaybe :: Int -> Int
callMeMaybe = undefined
