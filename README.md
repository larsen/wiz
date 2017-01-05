# Wiz [![Build Status](https://travis-ci.org/larsen/wiz.svg?branch=master)](https://travis-ci.org/larsen/wiz)

Wiz is my attempt to write a toy Scheme interpreter in Haskell.

## Development

I'm trying to document the process of developing Wiz on my personal
website:
[Implementing a Scheme interpreter in Haskell](http://stefanorodighiero.net/posts/2015-04-28-implementing-a-scheme-interpreter-in-haskell.html).

This document is usually a couple of iteration behind what I'm actually
doing. So you could find discrepancies between the code in this
repository and the code described in the article.

## Trying Wiz

Wiz has to be considered experimental, but I don't exclude simple
programs will work. If you want to have a try, once you cloned 
the repository:

```
$ stack setup
$ stack build
$ stack exec wiz
```

You can find examples in the [test/](test) directory.
