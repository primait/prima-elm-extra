# prima-elm-extra

`prima-elm-extra` is a collection of reusable utilities and helpers

### Namespaces

The modules are organized around elm module pattern, and are meant to be imported as-is, without aliases
E.g.

**Do**

```elm
import PrimaUpdate

update msg model = model |> PrimaCmd.withoutCmds 
```

**Don't**

```
import PrimaUpdate as UpdateExtra

update msg model = model |> UpdateExtra.withoutCmds 
```
