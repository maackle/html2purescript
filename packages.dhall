let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200724/packages.dhall sha256:bb941d30820a49345a0e88937094d2b9983d939c9fd3a46969b85ce44953d7d9

let overrides = {=}

let additions = {=}

in      upstream
    //  https://raw.githubusercontent.com/srghma/my-purescript-package-sets/f81370b/packages.dhall sha256:4060d30d829e5c6aa942c0f986f0bc86ad30c274efdb3cad5dce65eb64172170
          upstream.(https://raw.githubusercontent.com/srghma/my-purescript-package-sets/f81370b/upstreamTypeChunk.dhall sha256:1f07f2737ec9a052fa448d4f2c3058ed5bb68ea66622ac3d1f74bd78eeeac09b)
    //  overrides
    //  additions
