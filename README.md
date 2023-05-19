# Machinator

> v.tr. To devise (a scheme or plot). v.intr. To scheme or
> plot. [Latin māchinārī, māchināt-, to design, contrive, from māchina,
> device; see machine.]

Machinator is a data description language in the spirit of
[MaxML](https://github.com/mxswd/maxml/blob/master/Data/MaxML.hs).

We want to specify very simple types that would be idiomatic in
Haskell or Purescript. From this specification, we should be able to derive:

- Idiomatic types in Haskell and Purescript
- A human-friendly JSON encoding
- JSON encoders/decoders for Haskell and Purescript
- Exhaustive generators (exhaustive in each shape)
- Randomised generators (QuickCheck/Strongcheck style)
- The ability to create other such schemes

We also want a surface syntax, accessible to non-Haskellers, that can
be published, versioned, and statically analysed. This is the main
motivation to build a DSL, rather than embedding directly in Haskell.

We have ways to write type specifications and generic functions, but
no nice way to treat them as data or to decouple them from a specific
platform.

## Types

### Primitive types

Primitive types will be accreted according to need.

We currently support:

- Text

We expect to eventually support:

- Int32
- Int64
- Bool
- Bytes

### Compound types

- Sum types / variants
- Records with overloadable fields

## Syntax

### V1

The v1 syntax is Haskell-esque. We can change this down the track if
anyone develops Strong Syntax Opinions.

```haskell
-- machinator @ v1

data Foo
  = FooV1 Bar Baz Text
  | FooV2 Integer Double (Maybe Baz)
  | FooV3 [Baz]

record Baz = {
    name : Text
  , age : Int
  }

record Bar = Bar {
    name : Text
  , quux : Boolean
  }
```

### V2

`v2` introduced Haskell-style comments:

```haskell
data Foo = -- Ignore this bit
  {- Can also
    put multi line comments
      -}
  Foo Bar
```

Backends will be functions over such declarations. You should be able
to use only the backends you need, and in a flexible manner (as
codegen, as a library via TH, etc)

## Alternatives

Most IDLs target embedded systems or Java. We have decidedly fewer
constraints; we just need an expressive common subset of Haskell and
Purescript that is easy to encode in Projector's type system. Anything
else is a bonus.

- [Cauterize](https://github.com/cauterize-tools/cauterize) is one of
  the better IDLs, supporting most of the features we want. Since it
  targets embedded, it is not possible to have an arbitrary-length
  string or array, and we do not suffer this constraint.
- Protobuf has a set of assumptions reflecting its Java and Go
  targets, e.g. every field is nullable, no sums. Existing Haskell
  libs are shallow embeddings. It could be made to fit, probably, but
  would not generate idiomatic Haskell.

## Conceptual reviewers

- Tim H
- Jacob
- Charles

## Building

### Prerequisite
The Machinator repo makes use of submodules (in the lib/ folder); to initialise or update:
```
> git submodule init
> git submodule update
```


### Nix users
If you use [Nix](https://nixos.org/download.html):

Enter a Nix shell from the project root:
```
> nix develop
```

Run the cabal build:
```
> cabal build all
```

Alternativley building individual packages:

```
> cabal build exe:gen-scala
> cabal build exe:gen-python
```

Alternativley you can use `cabal install` to install the packages to `~/.cabal/bin` and add `~/.cabal/bin` to your path:

```
> cabal install exe:gen-scala
> cabal install exe:gen-python
```

### Mac Homebrew users

```
brew install ghc@8.10
brew install cabal-install

# You may want to add this to your .bashrc / .zshenv
export PATH="/opt/homebrew/opt/ghc@8.10/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/ghc@8.10/lib"
```

Build the binaries:
```
cabal build exe:gen-python
cabal build exe:gen-scala
```

After running cabal build the last line should be something like:

```Linking /Users/adam.evans/Code/simple-machines/machinator/dist-newstyle/build/aarch64-osx/ghc-8.10.7/ambiata-machinator-scala-1.0.0/x/gen-scala/build/gen-scala/gen-scala ...
Copy the file in the output to your path, i.e.:
cp /Users/adam.evans/Code/simple-machines/machinator/dist-newstyle/build/aarch64-osx/ghc-8.10.7/ambiata-machinator-scala-1.0.0/x/gen-scala/build/gen-scala/gen-scala \
  /usr/local/bin/machinator-gen-scala
```

Shorthand:
```
sudo cp "$(cabal list-bin gen-python)" /usr/local/bin/machinator-gen-python

sudo cp "$(cabal list-bin gen-scala)" /usr/local/bin/machinator-gen-scala
```
