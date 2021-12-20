# Gresley #

[![Build Status](https://travis-ci.org/matt-thomson/gresley.svg)](https://travis-ci.org/matt-thomson/gresley)
[![Coverage Status](https://coveralls.io/repos/matt-thomson/gresley/badge.png)](https://coveralls.io/r/matt-thomson/gresley)

Gresley is a [general game player](http://en.wikipedia.org/wiki/General_Game_Playing) written in Scala.

It is named after [George Gresley Perry](http://en.wikipedia.org/wiki/George_Perry_(priest)).

# Installation

```bash
$ sbt universal:packageBin
$ cd target/universal
$ unzip gresley-0.0.2.zip
$ cd gresley-0.0.2
$ ./bin/gresley
```

Interact with game server by POSTing messages to port 5000 on localhost:

```bash
$ curl -XPOST --data-raw '(info)' http://localhost:5000/
```

Look at the test cases for the language.
