module Utf8 where

-- | Bad UTF-8 sequence in comment is ok: [�]. Between the brackets there's a
-- sole continuation byte - see
-- http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt section 3.1.1.
ólé = "今日は"

foo = ólé ++ ólé
