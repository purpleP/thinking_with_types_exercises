{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Constraint, Type)
import Data.Ord


main = undefined

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#


type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    HNil == HNil = True
    (a :# as) == (b :# bs) = a == b && as == bs


instance ((All Ord ts), (All Eq ts)) => Ord (HList ts) where
    compare HNil HNil = EQ
    compare (a :# as) (b :# bs) = case compare a b of
                                    EQ -> compare as bs
                                    x -> x

class HShow ts where
    hshow :: HList ts -> String -> String

instance HShow '[] where
    hshow HNil _ = ""

instance (Show t, HShow ts) => HShow (t ': ts) where
    hshow (y :# ys) prefix = prefix ++ show y ++ hshow ys ", "

instance HShow ts => Show (HList ts) where
    show xs = "[" ++ (hshow xs "") ++ "]"
