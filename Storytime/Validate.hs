module Storytime.Validate (missingTargets, orphans) where

import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Storytime.Types

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

allTargets :: Story -> [Tag]
allTargets s = unique $ target <$> concatMap links (M.elems $ sects s)

missingTargets :: Story -> [Tag]
missingTargets s = filter (`M.notMember` sects s) $ allTargets s

orphans :: Story -> [Tag]
orphans s = filter (`S.notMember` targets) $ M.keys (sects s)
  where
    targets = S.fromList $ allTargets s


