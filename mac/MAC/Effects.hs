{-# LANGUAGE Unsafe #-}

-- | It provides functions which map read and write effects into security checks.
module MAC.Effects
    (
       create
     , writeup
     , readdown
     , fix
     , read_and_fix
     , write_and_fix
     , rw_read
     , rw_write
    )

where

import MAC.Lattice
import MAC.Core

{-|
    It lifts functions which create resources into secure functions which
    create labeled resources
-}
create :: (Monad m, Less l l') => m (d a) -> MACT l m (Res l' (d a))
create m = lift m >>= return . MkRes


{-|
    It lifts an 'IO'-action which writes into a data type @d a@
    into a secure function which writes into a labeled resource
-}
writeup :: (Monad m, Less l l') => (d a -> m ()) -> Res l' (d a) -> MACT l m ()
writeup m (MkRes a) = lift $ m a

{-|
    It lifts an 'IO'-action which reads from a data type @d a@
    into a secure function which reads from a labeled resource
-}
readdown :: (Monad m, Less l' l) => (d a -> m a) -> Res l' (d a) -> MACT l m a
readdown io (MkRes da) = lift $ io da

-- | Proxy function to set the index of the family member 'MAC'
fix :: Monad m => l -> MACT l m ()
fix _ = return ()

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
read_and_fix :: (Monad m, Less l l) => (d a -> m a) -> Res l (d a) -> MACT l m a
read_and_fix m r = fix (labelOf r) >> readdown m r

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
write_and_fix :: (Monad m, Less l' l') => (d a -> m ()) -> Res l' (d a) -> MACT l' m ()
write_and_fix io r = fix (labelOf r) >> writeup io r

{-|
    It lifts an operation which perform a read on data type @d a@, but
    it also performs a write on it as side-effect
-}
rw_read :: (Monad m, Less l l', Less l' l) => (d a -> m a) -> Res l' (d a) -> MACT l m a
rw_read io r = writeup (\_ -> return ()) r >> readdown io r

{-|
    It lifts an operation which perform a write on data type @d a@, but
    it also performs a read on it as side-effect
-}
rw_write :: (Monad m, Less l l', Less l' l) => (d a -> m ()) -> Res l' (d a) -> MACT l m ()
rw_write io r = readdown (\_ -> return undefined) r >> writeup io r
