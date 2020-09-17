# Front

This project uses Elm because of :
- type safety allowing quick & safe refactoring
- expressiveness : less boilerplate, less code
- [small assets](https://github.com/err0r500/nix)
- [very good performances](https://medium.com/dailyjs/a-realworld-comparison-of-front-end-frameworks-2020-4e50655fe4c1)

## Setup your dev env

If you have [nix](https://nixos.org/) you can simply run this from the project's root
```
nix-shell dev.nix
```

It will open a shell with everything needed, both for running and for working on the project.
You can then use any editor you want supporting the [elm-language-server](https://github.com/elm-tooling/elm-language-server) (its dependencies are already installed, so you just have to figure out how to configure your editor)

## Start your dev env
Run 

```
elm-live ./src/Main.elm
```

