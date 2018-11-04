# How
Given

```haskell
-- | Generic named field accessor class, where 'l' is label to specify the field in structure.
class HasField (l :: k) p f s t a b | l s -> a, l t -> b, l s a b -> t, l t a b -> s where
  hasField :: proxy l -> Optic p f s t a b

-- | convenience function if you have access to 'TypeApplications' extension
field :: forall l p f s t a b. (HasField l p f s t a b) => Optic p f s t a b
field = hasField (Proxy :: Proxy l)
```

## Simple fields of records

```haskell
data Foo a = Foo
  { name   :: Text
  , length :: a
  }

makeHasFields ''Foo
```

would generate instances

```haskell
instance (Functor f) => HasField "name" (->) f (Foo a) (Foo a) Text Text where
  hasField = -- lens is generated here

instance (Functor f) => HasField "length" (->) f (Foo a) (Foo b) a b where
  hasField = --  lens is generated here
```

## Traversals for flaky fields

```haskell
data Bar
  = Bar { name :: Text }
  | BarNameless

makeHasFields ''Bar
```

would generate

```haskell
instance (Applicative f) => HasField "name" (->) f Bar Bar Text Text where
  hasField = -- traversal is generated here (while not all constructors has field)
```

## ISO for newtypes

```haskell
newtype SomeId = SomeId
  { someId :: Int
  }

makeHasLens ''SomeId
```

would generate

```haskell
instance (Profunctor p, Functor f) => HasField "someId" p f SomeId SomeId Int Int where
  hasField = -- ISO is generated here
```

## Numeric accessors to nameless fields

Provide out of the box instances like

```haskell
instance (Functor f) => HasField '1 (->) f (a,x) (b,x) a b where
  hasField _ = _1

-- and so on for all combinations (until sane limits) of number and tuple sizes
```

we would also can generate such accessors for multiple nameless fields constructors like

```haskell
data Buzz = Buzz Int Text

makeNumFields ''Buzz
```

which would generate

```haskell
instance (Functor f) => HasField '1 (->) f Buzz Buzz Int Int where
  HasField = -- lens is generates here

instance (Functor f) => HasField '2 (->) f Buzz Buzz Text Text where
  HasField = -- lens is generates here
```

# Motivation

## Good for 'TypeApplications' and 'DuplicateRecordFields' extensions

## Base for generic code

## 'makeFields' generates classes which still may clash

## generic-lens has problems

### not supports flaky fields

### typeclass 'HasLens' is not generic enough

### generics are still not very
