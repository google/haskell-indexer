module DocUri where

-- - IODecl.doc/uri "https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/src/GHC.Types.html#IO"
-- - PutStrLnDecl.doc/uri "https://hackage.haskell.org/package/base-4.12.0.0/docs/src/System.IO.html#putStrLn"

-- - @IO ref IODecl
f :: IO ()
-- - @putStrLn ref PutStrLnDecl
f = putStrLn "hello"
