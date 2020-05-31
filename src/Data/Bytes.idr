module Data.Bytes

-- This probably won't continue to exist in this form once `import X as Y`
-- works since that's enough control to create re-exports pleasently. At that
-- time we can move Data.Bytes.Strict.API into this file verbatim.

import public Data.Bytes.Strict.Internal -- probably don't need this here
import public Data.Bytes.Strict.API
