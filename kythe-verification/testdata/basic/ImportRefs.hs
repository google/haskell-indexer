module ImportRefs where

-- - @foo ref/imports FooVar
-- - @bar ref/imports BarVar
import ImpExpDefs (bar, foo)              -- IEVar

-- - @FooBar ref/imports TypeD
import ImpExpDefs (FooBar)                -- IEThingAbs

-- - @FooBar ref/imports TypeD
import ImpExpDefs (FooBar (..))           -- IEThingAll

-- - @FooBar ref/imports TypeD
-- - @MkFB ref/imports CtorD
-- - @fbFoo ref/imports FieldFbFoo
import ImpExpDefs (FooBar (MkFB, fbFoo))  -- IEThingWith

-- - @bar ref BarVar
import ImpExpDefs hiding (bar)
