module DataConWrap where

data D a = A { unA :: !a }

make = A 5
