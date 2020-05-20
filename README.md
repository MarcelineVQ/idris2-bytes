Bytes
=====


This is a [ByteString](http://hackage.haskell.org/package/bytestring)-style library for the [Idris2](https://github.com/edwinb/Idris2/) dependent programming language.

Its name is due to how overused the term String is.  
Bytes are an immutable chunk of 0 or more bytes, nothing more is implied.

A standard interface is provided that should cover startng cases and more useful things are on the way such as Builder's for efficient creation of more complicated/interesting Bytes.

PR's are very welcome, please mind that the intent of this library is to provide a solid base for people to build libraries/softeware off of. We should be lean, powerful, and _especially_ compatible.  
[Idris2](https://github.com/edwinb/Idris2/) is intended to be used with many possible backends so we need to have a library here that is maximally portable.
