This package offers operations on *unlifted* datatypes using GHC primitives
intended for working with *lifted* datatypes. This requires fairly liberal use
of `unsafeCoerce#`, which is always a bit tricky to use safely. One
particularly tricky aspect has to do with extracting values from primop
results. For example, one might imagine implementing `readSmallUnliftedArray#` like
so:

```haskell
readSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i s
  = case Exts.readSmallArray# mary i s of
      (# s', a #) -> (# s', unsafeCoerce# a #)
```

Unfortunately, GHC is *really* not designed to support unsafe coercions between
lifted and unlifted types. One thing that can happen is that `unsafeCoerce#`
can be floated around `case` in various ways. Since Core `case` with a *lifted*
scrutinee will *force* (specifically, *enter*) that scrutinee, things go very
wrong. The above code would actually end up being compiled to something like

```haskell
readSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i s
  = case Exts.readSmallArray# mary i s of _ { (# s', a #) ->
    case a of _ { DEFAULT__ ->
      (# s', unsafeCoerce# a #)}}
```

Since `a` has a *lifted* type there (even though it's really an *unlifted*
object, it will be *entered*, which causes an unchecked exception. Ugh.
It turns out that we get the code we want for `readSmallUnliftedArray#` by
writing it like this:

```haskell
readSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i s
  = unsafeCoerce# (Exts.readSmallArray# mary i s)
```

Knowing this works, we `NOINLINE` it, hiding all the coercions from the
optimizer and guaranteeing that we only need to do a deep dive into the Core
for the modules defining the primops; coercion-related problems can't be
introduced elsewhere.

For `Data.Primitive.Unlifted.Array.Primops`, we take a different approach.  An
`ArrayArray#` can actually hold unlifted things, so we use that to represent
`UnliftedArray#`, letting almost everything `INLINE`. Unfortunately, some
`Array#` operations aren't available for `ArrayArray#`. But fortunately, an
`ArrayArray#` is actually the same as an `Array#` under the hood! So we can
`unsafeCoerce#` primops for `Array#` to work with `ArrayArray#`. In the very
few cases where that involves coercing between lifted and unlifted types, we
`NOINLINE`.
