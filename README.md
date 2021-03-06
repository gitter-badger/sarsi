Sarsi
=====
A universal quickfix toolkit and his protocol.

# Philosophy

*[Sarsi](https://en.wiktionary.org/wiki/sarcio#Latin)* is at it's core a binary protocol to exchange quickfix messages, it approach the operating system as an integrated development environment, it's design follow the holy [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) principles.

On one side we produce messages from our favorite build tools, on the other we want them to be consumed as soon as possible by our favorite text editors.

That's basically what `sarsi` is doing, and will always do, any other integration with  build tools/text editors should be designed separately.

# Modules

Producers

 - `sarsi-hs` - A generic command line wrapper for Haskell tools (GHC/Cabal/[Stack](http://haskellstack.org/)/...)
 - `sarsi-sbt` - A [SBT](http://www.scala-sbt.org/) specific wrapper for the Scala programming language

Consumers

 - `sarsi-nvim` - A lightweight [Neovim](https://neovim.io/) RPC client

# Install

#### Hackage

*Sarsi* is published on Hackage and can be installed using `cabal`.

	cabal install sarsi

This will install all the modules as well.

#### Source

Alternatively, it can be installed from source using `stack`.
	
	git clone git@github.com:aloiscochard/sarsi.git
	cd sarsi
	stack install


### Neovim Integration

Once `sarsi` installed, simply add the following line in your `init.vim`.

	call rpcstart('sarsi-nvim') 


# Usage

By default, when a consumer/producer start it will use a Unix pipe with a name generated according the directory in which it was launched.

It basically means you have to start consumers/producers from the same directory for them to be connected.

### Haskell

The `sarsi-hs` command line wrapper allow you to run an arbitrary command and get it's output transparently feeded into an active consumer.

	sarsi-hs stack build

It works nicely with `inotifywait`, or any other hooks you would like to use.

### Scala

You can simply use it in place of your `sbt` command, interactively or not (you should surely prefer the former for performance reasons).

	sarsi-sbt ~test:compile

It will behind the scene call the `sbt` program available in the path.
