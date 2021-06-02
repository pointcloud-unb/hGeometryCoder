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

`$ stack exec -- storm-point-cloud-exe <option> <input_file_path> <axis>`

Where:

- option:
  - `-e` : Enconding
  - `-d` : Decoding
  
- input_file_path: input file for the chosen option
- axis: axis throughout the encoding will be made 
