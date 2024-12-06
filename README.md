# vite-plugin-ocaml

Easily turn ocaml code into javascript whenever you're using vite :)

## Goals

Two Paths:

- "no dune": `melangeWithoutDune`
    - You run dune separately from the Vite plugin.
    - You're probably running `dune build -w` or similar in your repo
    - You commit the generated JS files
    - This plugin finds all the JS files, gives correct imports, and then bundles

- "yes dune": `melangeWithDune`
    - FEAR IS THE MIND KILLER
    - Dune is controlled and executed by the Vite plugin.
    - You need dune (preview) installed in your CI/Build/etc. to then generate the Javascript files
    - This plugin finds all generated JS files, gives correct imports, and then bundles

I would recommend most people use the `no dune` style for this plugin, since that means anyone who uses your JS code doesn't have to worry whether or not they have dune installed.
