# g-pcc-dyadic-codec

[![Build status](https://img.shields.io/travis/com/joseedil/g-pcc-dyadic-codec/master?logo=travis)](https://travis-ci.com/joseedil/g-pcc-dyadic-codec)
[![Hackage](https://img.shields.io/hackage/v/g-pcc-dyadic-codec.svg?logo=haskell)](https://hackage.haskell.org/package/g-pcc-dyadic-codec)
[![Stackage Lts](http://stackage.org/package/g-pcc-dyadic-codec/badge/lts)](http://stackage.org/lts/package/g-pcc-dyadic-codec)
[![Stackage Nightly](http://stackage.org/package/g-pcc-dyadic-codec/badge/nightly)](http://stackage.org/nightly/package/g-pcc-dyadic-codec)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## To execute

- Build the project:

`$ stack build`

- To execute:

`$ stack exec -- hGeometryCoder <option> <input_file_path> <axis>`

Where:

- option:
  - `-e` : Enconding
  - `-d` : Decoding
  
- input_file_path: input file for the chosen option
- axis: axis throughout the encoding will be made 


## Benchmarks
There are benchmarks for several parts of the codec pipeline. They are intended to help optimize specific parts of the code base, but can also be used as reference to compare with other implementations.

Results are displayed in the terminal. An HTML file is created in the folder `benchmark/results/` with a nicer summary. 

- To run everything (go grab some coffee, this will probably take quite a long time to finish):

`$ stack bench`

- To list the available benchmarks:

`$ stack bench --ba "--list`

- To run specific benchmarks:

`$ stack bench --ba "<benchmark name>"`

in which `<banchmark name>` may be the total name or the prefix for a set of benchmarks.

- Each benchmark runs for 1 minute, but some benefit from a longer timeout. To set it:

`$ stack bench --ba "<benchmark name> --time-limit <seconds>" `
