{-# LANGUAGE Trustworthy #-}

-- | Labeled expressions.
module MAC.Labeled
    (
       Labeled ()
     , Id (MkId, unId)
     , label
     , unlabel
    )

where

import MAC.Lattice
import MAC.Core (MACT(), Res())
import MAC.Effects

-- | Type denoting values of type @a@
newtype Id a = MkId { unId :: a }

-- | Labeled expressions
type Labeled l a = Res l (Id a)

-- | Creation of labeled expressions
label :: (Monad m, Less l l') => a -> MACT l m (Labeled l' a)
label = create . return . MkId

-- | Observing labeled expressions
unlabel :: (Monad m, Less l' l) => Labeled l' a -> MACT l m a
unlabel = readdown (return . unId)
