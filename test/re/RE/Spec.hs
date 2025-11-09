module RE.Spec where

import RE.Match
import RE.Types
import Test.Mutagen

prop_optimize :: (RE ASCII, [ASCII]) -> Result
prop_optimize (re, str) =
  not (null str)
    && re
      `matches` str
      ==> optimize re
      `matches` str
