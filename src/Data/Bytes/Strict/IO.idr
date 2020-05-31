module Data.Bytes.Strict.IO

-- functions for reading/writing Bytes to and from files

-- Data.Bytes.Strict.IO should be safe and stable to depend on, though the
-- module might move over time. Conversely Data.Bytes.Lazy.IO is not a good
-- idea to depend on despite being the more useful API. It is being provided
-- for the purposes of developing this library and for people to prototype with
-- but it is lazy IO, and one of the lessons of Haskell is that while lazyness
-- is good, lazy io isn't. There's just no way to deal with IO errors in pure
-- code, and doing IO lazily will give you exactly that. Say we've read some
-- file into a lazy bytes and pass it along to our pure code. If the file is
-- closed before we start consuming the LBytes, we'll get a file error in a
-- place it can't be addressed/handled. We don't have an exception mechanism in
-- idris2 currently so this is slightly less of a problem than it is in haskell
-- but at the same time it'd be a real hassle to make every LBytes method end
-- with `Either Error a`. The ideal way to go is a streaming IO library that
-- provides our LBytes rather than lazy io because you can structure your
-- program around the streaming lib and have confidence about when issues will
-- crop up and where to handle them.