module FileOperations where

import Prelude

import Data.Array ((:), concatMap, filter, foldl)
import Data.Path
import Data.Array.Partial (head)
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\f -> not $ isDirectory f) $ allFiles file


maxSizeFile :: Path -> Path
maxSizeFile file = foldl (\m f -> if size f > size m then f else m) (unsafePartial head $ onlyFiles file) $ onlyFiles file

minSizeFile :: Path -> Path
minSizeFile file = foldl (\m f -> if size f < size m then f else m) (unsafePartial head $ onlyFiles file) $ onlyFiles file

-- TODO
-- whereIs :: String -> Path -> Maybe Path
