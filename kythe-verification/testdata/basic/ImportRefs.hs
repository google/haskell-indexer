module ImportRefs where

-- - @foo ref/imports FooVar
import ImportDefs (foo)
-- - @bar ref BarVar
import ImportDefs hiding (bar)
