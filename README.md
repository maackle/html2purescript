# html2purescript

> **THIS REPO IS NOT MAINTAINED**. It's designed to quickly bootstrap from HTML into Halogen markup, but it is not robust or complete. Feel free to fork!

Latest version live at https://srghma.github.io/html2purescript/

Previous version [(see branch)](https://github.com/maackle/html2purescript/tree/pre-fork) live at http://michaeldougherty.info/html2purescript/

Very basic parser/renderer to convert between raw HTML and (something close to) valid, readable PureScript syntax. Right now only outputs to Halogen.

For HTML parsing, the app uses https://github.com/carymrobbins/purescript-html-parser, which is itself incomplete and not published to Pursuit, so it is included in my project as a submodule for now.

For Halogen output, it is assumed the following are imported:

```purescript
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
```

The project itself is written in Halogen and is based off https://github.com/qwaneu/purescript-halogen-template

# Alternatives

https://github.com/flip111/purescript-html2halogen
