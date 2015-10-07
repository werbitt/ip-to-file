module Data.Changeable where

-- | Changeable is used to tag whether something has changed or not. This
-- is used to prevent unneccessary writes to disk.

data Changeable a = Same a | Changed a deriving (Show)
