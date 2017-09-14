-- Haskell package+module combination corresponds to Kythe package.
-- - @Module defines/binding Pkg
-- - Pkg.node/kind package
module Module where

-- - @someTopLevel defines/binding TopDecl
-- - TopDecl childof Pkg
someTopLevel = 3

-- Files belonging to a package+module are children of that package+module.
-- - File = vname("", _, _, "testdata/basic/Module.hs", "").node/kind file
-- - File childof Pkg
