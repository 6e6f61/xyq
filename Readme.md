# xyq

`xyq` traverses structured data to find a specified key or value.

## Examples

Files used here are available in `samples/`.

### Find by key

```bash
$ xyq key lang < samples/example.json
.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[1].lang
```

### Find by value

```bash
$ xyq value XML < samples/example.json
.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[1].lang
```