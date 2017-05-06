module CrossRef2 where

-- TODO(robinpalotai): verify imports, exports once emitted.
import CrossRef1 (foo)

bar :: Int
-- - @foo ref _
bar = foo + 1
