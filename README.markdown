# Clw-Sample-Game-Algorithm - Samples to try some game algorithms

## Usage

```lisp
> (ql:quickload :qlot)
> (qlot:quickload :clw-sample-game-algorithm)
> (clw-sample-game-algorithm:start :port <port number>)
```

After starting, you can access to the tetris by `http://localhost:<port number>` using a web browser.

## Installation

This project depends on liblaries that are not registered in the quicklisp repository. So I recommend to use [qlot](https://github.com/fukamachi/qlot) to install them.

```bash
# Under a directory managed by quicklisp
$ git clone https://github.com/eshamster/clw-sample-game-algorithm.git
$ cd clw-tetris
$ ros install qlot # if you haven't installed
$ qlot install
```

## Author

- eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2018 eshamster (hamgoostar@gmail.com)

## License

Licensed under the MIT License.