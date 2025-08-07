:ramen: :boom: miso-reactive
====================

See live [here](https://reactive.haskell-miso.org/)

This demonstrates "reactivity" between `Component` in `miso`.

## Example

```haskell
----------------------------------------------------------------------------
childComponent :: MisoString -> Component ParentModel ChildModel ChildAction
childComponent childComponentName = (component (ChildModel 0) noop view_)
  { bindings =
      [ parentField <---> childField
        -- ^ dmj: Bidirectional synch between parent and child `model`, using `Lens`
      ]
  } where
      view_ :: ChildModel -> View ChildModel ChildAction
      view_ (ChildModel x) =
        div_
        []
        [ h3_ [] [ text ("Child Component " <> childComponentName) ]
        , button_ [ onClick ChildAdd ] [ "+" ]
        , text (ms x)
        , button_ [ onClick ChildSubtract ] [ "-" ]
        ]
```

## Introduction

As of `1.9`, `miso` is now recursive. This means `miso` applications can embed other `miso` applications, and be distributed independently. The type `Component` has been introduced to facilitate this, and is equipped with lifecycle mounting hooks (`mount` / `unmount`). This has necessitated a runtime system to manage `Component` internally.

This means `miso` now forms a graph of `Component` nested on the Virtual DOM, where each `Component` has its own `IORef model` state (a.k.a. "reactive variable") that can be synchronized between the parent / child relationship (unidirectionally or bidirectionally) in a type-safe, composable manner.

`miso` has added the `"bindings"` field to establish edges in the `Component` graph (between immediate ancestor and descendant). This allows data to "pulsate" between `Component` keeping data in synch. When used at multiple levels in the tree this creates a cascade effect.

The `-->`, `<--`, `<-->` reactive combinators have been introduced to allow users to establish edges between `Component` in the graph, in a declarative way. This creates dependencies in the graph between `Component` `model` changes. The combinators take two `Lens` as arguments, which synchronize changes between `Component` `model` in the direction the user desires.

Under the hood this is done through a broadcast `TChan`, to synchronize the `IORef model` of various `Component`. This is accomplished without imposing a recursive interface on end users (`miso` handles all the recursion under the hood).

This is similar to [React props](https://react.dev/learn/passing-props-to-a-component), where a parent component can pass properties to its descendants, and they will inherit any changes the parent makes to that "prop". The difference with `miso` is that we accomplish this in a declarative way using `Lens` to synchronize state. This allows us to keep the `View` pure, and retain the isomorphism property. Furthermore, `miso` takes it a step further and allows declarative upstream communication with the `parent`. Whereas in React a callback would need to be passed to the child to invoke parent model changes, creating a more convoluted programming model. A bidirectional synch can also be established between `parent` and `child` using the `(<-->`) combinator. This allows sibling communication, where the `parent` is used as a proxy.

Lastly, this is all done in a type-safe way. `Component` is parameterized by `parent`, which is the type of the ancestor's `model` ("reactive variable"). This gives us type-safe, reactive `Component` composition.

## Development

[The source](https://github.com/haskell-miso/miso-reactive/blob/master/app/Main.hs) maintains an example of sibling communication using the `<-->` reactive combinator.

> [!TIP]
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop --experimental-features nix-command --extra-experimental-features flakes
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
