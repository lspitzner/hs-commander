{-# LANGUAGE ScopedTypeVariables #-}

module Commander.Utils (

) where

import Commander.Commands (
     Fn(..)
   , injectParams
   , extractParams
   , Parameter(..)
   , Command(..)
   , Commands
   )

--
-- - general autocomplete (with class to complete param flags/values)
-- - parse string to values/flags
-- - attempt to run route given values/flags
--