:ramen: :boom: miso-reactive
====================

This demonstrates [reactivity](https://en.wikipedia.org/wiki/Functional_reactive_programming) between `Component` in [miso](https://github.com/dmjio/miso). See live [here](https://reactive.haskell-miso.org/)

<a href="https://reactive.haskell-miso.org/">
<img width="1262" height="517" alt="image" src="https://github.com/user-attachments/assets/a49f8746-7959-4081-9490-4892cd07a989" />
</a>

## Example

```haskell
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

As of `1.9`, `miso` is now recursive. This means `miso` applications can embed other `miso` applications, and be distributed independently. The type `Component` has been introduced to facilitate this and is equipped with lifecycle mounting hooks (`mount` / `unmount`). This has necessitated a runtime system to manage `Component` internally. Component are now parameterized by `parent`, which is the type of the ancestor's `model`. `Component` `model` can be synchronized between the parent / child relationship (unidirectionally or bidirectionally) in a type-safe, composable manner.

`miso` has added the `bindings` field to establish edges in the `Component` graph (between immediate ancestor and descendant). This allows effects to be applied from one `Component` `model` to the next, along the user-defined edge (`Binding`) via a `Lens`. When used at multiple levels in the tree this can create a cascading effect.

The `-->`, `<--`, `<-->` reactive combinators have been introduced to allow users to establish edges between `Component` in the graph, in a declarative way. This creates dependencies in the graph between `Component` `model` changes. The combinators take two `Lens` as arguments, which synchronize changes between `Component` `model` in the direction the user desires.

Under the hood this is done through a scheduler and a depth-first traversal of the `Component` graph. This is accomplished without imposing a recursive interface on end users (`miso` handles all the recursion under the hood).

Furthermore, `miso` takes it a step further and allows declarative upstream communication with the `parent`. Whereas in React a callback would need to be passed to the child to invoke parent model changes, creating a more convoluted programming model. A bidirectional synch can also be established between `parent` and `child` using the `(<-->)` combinator. This can allow sibling communication, where the `parent` is used as a proxy (as seen in the [example](https://reactive.haskell-miso.org)).

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
