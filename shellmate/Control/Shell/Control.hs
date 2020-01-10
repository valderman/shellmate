{-# LANGUAGE TypeFamilies #-}
-- | Control flow constructs.
module Control.Shell.Control
  ( module Control.Monad
  , Guard (..)
  , guard, when, unless, orElse
  ) where
import Control.Shell.Internal
import Control.Monad hiding (when, unless, guard)

-- | Attempt to run the first command. If the first command fails, run the
--   second. Forces serialization of the first command.
orElse :: Shell a -> Shell a -> Shell a
orElse a b = do
  ex <- try a
  case ex of
    Right x -> return x
    _       -> b

class Guard guard where
  -- | The type of the guard's return value, if it succeeds.
  type Result guard

  -- | Perform a Shell computation; if the computation succeeds but returns
  --   a false-ish value, the outer Shell computation fails with the given
  --   error message.
  assert :: String -> guard -> Shell (Result guard)

instance Guard (Either l r) where
  type Result (Either l r) = r
  assert _ (Right x) = return x
  assert desc _      = fail desc

instance Guard (Maybe a) where
  type Result (Maybe a) = a
  assert _ (Just x) = return x
  assert desc _     = fail desc

instance Guard Bool where
  type Result Bool = ()
  assert _ True = return ()
  assert desc _ = fail desc

instance Guard a => Guard (Shell a) where
  type Result (Shell a) = Result a
  assert desc m = m >>= \x -> assert desc x

-- | Perform a Shell computation; if the computation succeeds but returns
--   a false-ish value, the outer Shell computation fails.
--   Corresponds to 'CM.guard'.
guard :: Guard g => g -> Shell (Result g)
guard = assert "Guard failed!"

-- | Perform the given computation if the given guard passes, otherwise do
--   nothing. The guard raising an error counts as failure as far as this
--   function is concerned.
--   Corresponds to 'CM.when'.
when :: Guard g => g -> Shell () -> Shell ()
when g m = do
  res <- try (guard g)
  case res of
    Right _ -> m
    _       -> return ()

-- | Perform the given computation if the given guard fails, otherwise do
--   nothing. The guard raising an error counts as failure as far as this
--   function is concerned.
--   Corresponds to 'CM.unless'.
unless :: Guard g => g -> Shell () -> Shell ()
unless g m = void (guard g) `orElse` m
