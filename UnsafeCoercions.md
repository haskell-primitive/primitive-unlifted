This package offers operations on *unlifted* datatypes using GHC primitives
intended for working with *lifted* datatypes. This requires fairly liberal use
of `unsafeCoerce#`, which is always a bit tricky to use safely. One
particularly tricky aspect has to do with extracting values from primop
results. For example, one might imagine implementing `readUnliftedArray#` like
so:

```haskell
readUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readUnliftedArray# (MutableUnliftedArray# mary) i s
  = case Exts.readArray# mary i s of
      (# s', a #) -> (# s', unsafeCoerce# a #)
```

Now we define

```haskell
readUnliftedArray :: PrimUnlifted a
  => MutableUnliftedArray s a
  -> Int
  -> ST s a
{-# inline readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray arr) (I# ix) =
  ST $ \s -> case readUnliftedArray# arr ix s of
    (# s', a #) -> (# s', fromUnlifted# a #)
```

With the above definition of `readUnliftedArray#`, GHC rejiggers things and comes
up with the following Core:

```
readUnliftedArray1_ra6c
  :: forall a s.
     PrimUnlifted a =>
     MutableUnliftedArray_ s a (Unlifted a)
     -> Int -> State# s -> (# State# s, a #)
[GblId,
 Arity=4,
 Caf=NoCafRefs,
 Str=<L,1*U(A,1*C1(U))><S,1*U(U)><S,1*U(U)><L,U>,
 Unf=OtherCon []]
readUnliftedArray1_ra6c
  = \ (@ a_a6FT)
      (@ s_a6FU)
      ($dPrimUnlifted_a6FW :: PrimUnlifted a_a6FT)
      (eta_X2C :: MutableUnliftedArray_ s_a6FU a_a6FT (Unlifted a_a6FT))
      (eta1_X5d :: Int)
      (eta2_X2 :: State# s_a6FU) ->
      case eta_X2C of { MutableUnliftedArray arr_a57v ->
      case eta1_X5d of { I# ix_a57w ->
      case Exts.readArray#
             @ s_a6FU
             @ Exts.Any
             (arr_a57v
              `cast` (Data.Primitive.Unlifted.Array.Primops.N:MutableUnliftedArray#[0]
                          <s_a6FU>_N <Unlifted a_a6FT>_R
                      :: MutableUnliftedArray# s_a6FU (Unlifted a_a6FT)
                         ~R# Exts.MutableArray# s_a6FU Exts.Any))
             ix_a57w
             eta2_X2
      of
      { (# ipv_sT3, ipv1_sT4 #) ->
      case ipv1_sT4
           `cast` (UnsafeCo representational Exts.Any (Unlifted a_a6FT)
                   :: Exts.Any ~R# Unlifted a_a6FT)
      of wild2_Xa
      { __DEFAULT ->
      (# ipv_sT3, fromUnlifted# @ a_a6FT $dPrimUnlifted_a6FW wild2_Xa #)
      }
      }
      }
      }
```

The problem is near the end: `case ipv1_sT4 `cast` ... of wild2_Xa`. This
attempts to force the value extracted from the array. But that's an *unlifted*
value, so forcing it will produce an unchecked exception. Yikes. What we do
instead is define

```haskell
readUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readUnliftedArray# (MutableUnliftedArray# mary) i s
  = unsafeCoerce# (Exts.readArray# mary i s)
```

This seems to dodge the problem, at least in our testing thus far. We get the
following Core, which is much more reasonable:

```
-- RHS size: {terms: 23, types: 50, coercions: 21, joins: 0/0}
readUnliftedArray1_r9mM
  :: forall a s.
     PrimUnlifted a =>
     MutableUnliftedArray_ s a (Unlifted a)
     -> Int -> State# s -> (# State# s, a #)
[GblId,
 Arity=4,
 Caf=NoCafRefs,
 Str=<L,1*U(A,1*C1(U))><S,1*U(U)><S,1*U(U)><L,U>,
 Unf=OtherCon []]
readUnliftedArray1_r9mM
  = \ (@ a_a5X9)
      (@ s_a5Xa)
      ($dPrimUnlifted_a5Xc :: PrimUnlifted a_a5X9)
      (eta_X2C :: MutableUnliftedArray_ s_a5Xa a_a5X9 (Unlifted a_a5X9))
      (eta1_X5d :: Int)
      (eta2_X2 :: State# s_a5Xa) ->
      case eta_X2C of { MutableUnliftedArray arr_a32J ->
      case eta1_X5d of { I# ix_a32K ->
      case (Exts.readArray#
              @ s_a5Xa
              @ Exts.Any
              (arr_a32J
               `cast` (Data.Primitive.Unlifted.Array.Primops.N:MutableUnliftedArray#[0]
                           <s_a5Xa>_N <Unlifted a_a5X9>_R
                       :: MutableUnliftedArray# s_a5Xa (Unlifted a_a5X9)
                          ~R# Exts.MutableArray# s_a5Xa Exts.Any))
              ix_a32K
              eta2_X2)
           `cast` (((#,#)
                      <'Exts.TupleRep '[]>_R
                      (UnsafeCo representational 'Exts.LiftedRep 'Exts.UnliftedRep)
                      <State# s_a5Xa>_R
                      (UnsafeCo representational Exts.Any (Unlifted a_a5X9)))_R
                   :: (# State# s_a5Xa, Exts.Any #)
                      ~R# (# State# s_a5Xa, Unlifted a_a5X9 #))
      of
      { (# ipv_s77D, ipv1_s77E #) ->
      (# ipv_s77D,
         fromUnlifted# @ a_a5X9 $dPrimUnlifted_a5Xc ipv1_s77E #)
      }
      }
      }
```
