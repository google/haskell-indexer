module ImportRefs where

import ImportDefs (bar, foo)                        -- IEVar
import ImportDefs (FooBar)                          -- IEThingAbs
import ImportDefs (FooBar (..))                     -- IEThingAll
import ImportDefs (FooBar (MkFooBar, fbFoo, fbBar)) -- IEThingWith
