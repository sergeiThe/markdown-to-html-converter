scan line by line

title: # title
paragraph: text

```haskell
([""],["# heading"])
(["#"],[" heading"])
(["# "],["heading"])
```

two modes: md seeking | reading until delimiter

## Markdown seeking

- goes ch by ch, saves to read buffer
- checks the buffer for md indicators
- no indicators start with the buffered sequence ? switch mode

takeWhile
