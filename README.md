:ramen: :boom: miso-reactive
====================

See live [here](https://github.com/haskell-miso/miso-reactive)

This demonstrates "reactivity" between `Component` in `miso`.

As of `1.9`, `miso` is now recursive. This means `miso` applications can embed other `miso` applications, and be distributed independently. The type `Component` has been introduced to facilitate this, and it is equipped with lifecycle mounting hooks (`mount` / `unmount`). This has necessitated a runtime system to manage `Component` internally.

This means `miso` now forms a graph of `Component` nested on the Virtual DOM tree, where each `Component` has its own `IORef model` state (a.k.a. "reactive variable") that can be synchronized between the parent / child relationship (unidirectionally or bidirectionally) in a type-safe, composable manner.

`miso` has added the `"bindings"` field to establish edges in the `Component` graph (between immediate ancestor and descendant). This allows data to "pulsate" through `Component` keeping data in synch. When used at multiple levels in the tree this creates a cascade effect.

The `-->`, `<--`, `<-->` combinators have been introduced to allow users to establish eges between `Component` in the graph, in a declarative way. It takes as an argument two `Lens` which will synchronize changes between the `model` in the direction the user desires.

Under the hood this is done through a broadcast `TChan`, to synchronize the `IORef model` of various `Component`. This is accomplished without imposing a recursive interface on end users (`miso` handles all the recursion under the hood).

This is similar to [React props](https://react.dev/learn/passing-props-to-a-component), where a parent component can pass properties to its descendants, and they will inherit any changes the parent makes to that "prop". The difference with `miso` is that we accomplish this in a declarative way using `Lens` to sychronize state. This allows us to keep the `View` pure, and retain the isomorphism property. Furthermore, `miso` takes it a step further and allows declarative upstream communication with the `parent`. Whereas in React a callback would need to be passed to the child to invoke parent model changes, creating a more convoluted programming model. Lastly, a bidirectional synch can be done between `parent` and `child` using the `(<-->`) combinator. This allows sibling communication, using the parent as a proxy.

Lastly, this is all done in a type-safe way. `Component` is parameterized by `parent`, which is the type of the ancestor's `model` ("reactive variable"). This gives us type-safe, reactive `Component` composition.

> [!TIP]
> This requires installing [nix](https://nixos.org) with [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) enabled.
> Although not required, we recommend using [miso's binary cache](https://github.com/dmjio/miso?tab=readme-ov-file#binary-cache).

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop --experimental-features nix-command --extra-experimental-features flakes
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make" --experimental-features 'nix-command flakes'
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build" --experimental-features 'nix-command flakes'
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve" --experimental-features 'nix-command flakes'
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
