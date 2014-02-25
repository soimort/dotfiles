
## Script-Fu

### Open Script-Fu interactive console

```sh
script-fu
```

Equals to:

```sh
gimp -i -b -
```

### Execute a Script-Fu script

```sh
script-fu '(simple-unsharp-mask "foo.jpg" 5.0 0.5 0)'
```

Equals to:

```sh
gimp -i -b '(simple-unsharp-mask "foo.jpg" 5.0 0.5 0)' -b '(gimp-quit 0)'
```

### Auto White Balance

___NOTE: Single quotes are necessary!___

```sh
autowhite '*.jpg'
```

Equals to:

```sh
script-fu '(batch-levels-stretch "*.jpg")'
```
