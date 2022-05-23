module PartialMap where

-- import Data.Attoparsec.Text
-- import qualified Data.Map.Strict as M
-- import Data.Maybe
-- import Data.Text

-- type Input = Text

-- newtype PartialMap k v = PartialMap (M.Map k v) (Parser (k,v)) Input

-- parseTillKey :: Ord k => PartialMap k v -> k -> PartialMap k v
-- parseTillKey (PartialMap m p i) k = if M.member m k 
--     then m
--     else parseTillKey (insert ) k

-- (!!) :: Ord k => PartialMap k v -> k -> v
-- (!!) (PartialMap m _ _) k = m M.! k

-- (!) :: Ord k => PartialMap k v -> k -> v
-- (!) pm k = parseTillKey pm k !! k