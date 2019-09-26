module ImportRefs where

-- - @foo ref/imports FooVar
-- - @bar ref/imports BarVar
import ImportDefs (bar, foo)              -- IEVar

-- - @FooBar ref/imports TypeD
import ImportDefs (FooBar)                -- IEThingAbs

-- - @FooBar ref/imports TypeD
import ImportDefs (FooBar (..))           -- IEThingAll

-- - @FooBar ref/imports TypeD
-- - @MkFB ref/imports CtorD
-- - @fbFoo ref/imports FieldFbFoo
import ImportDefs (FooBar (MkFB, fbFoo))  -- IEThingWith

-- - @bar ref BarVar
import ImportDefs hiding (bar)
