There's a lesson here:
http://www.ilikebigbits.com/2014_04_29_myth_of_ram_3.html

What does this imply for our implementation?  
Is `getByte buffer 0` a random access or is this memory location known?
Are we already in trouble when we iterate a buffer by writing `getByte buffer n` for each n?  
As opposed to  ptr = getByte buffer 0  and iterate the ptr? And accessing the memory locations ourselves.  
This bears investigating.

---

Covering is the default for idris2 now so I need to determine what the best
route forward is for this lib. We call crash sometimes and it's partial, and
partial is infectious. Is it correct to be partial anywhere an allocation can
occur? A crash is a crash so if there was a primitive to lie about the totallity
of a definition would it be morally correct to do so in the case of calling
crash? We're already crashing either way. I don't actually think it would be
correct but it sure would be more convenient. There's many things that are
currently partial that are only partial because memory might fail to allocate
and I want a better error message for when that happens. This is not a nice
trade. One may ask "if a thing can fail is it not partial?" but consider the
total expression x :: xs, if we fail to allocate space when applying :: does
that mean it was partial? The current decision is to lie about totality in the
crash we're using for allocation, because :: would also crash if allocation
failed and it isn't partial.

---

Primitives we probably want: nat-to-int cast, making mod actually modulo and having a rem for remainder

Investigate the casting story between int/integer and nat in general, ideally there'd be no work to do at runtime between Nat and Integer but the way things are written now it seems like we have to convert by repeatedly +1. That's no bueno for our nice interface.

---
fixity declarations are to be located with the declarations they mention.
mutual blocks are to be used where possible as opposed to forward declarations.
It's not nice to make your reader scan all over a source file to find what they need to know about a definition
```idris
-- Comment having details about foo
infixr 3 `foo`
export
%inline
foo : Type -> blah -> a
t `foo` b = ...
```
---

Some thoughts on Bytes representation:
```
-- Our Bytes type, a Ptr to a `block` of memory, its size in bytes,
-- and the current 0-based offset into that memory.
-- Nat is technically too big here, it'd be better to have some bounded type.
-- Perhaps it would be best to use Int and provide a NonEmpty in terms of
-- believe_me or So (which seems a little tedious) since I really only use the
-- Nat to prove NonEmpty. The problem is that the compiler isn't going to help
-- us supply NonEmpty then. It'd be really awful to have to `with` anywhere I
-- want NonEmpty to be inferred. Especially since it's just for a few
-- operations. Adding inconvience for a convenience feature isn't choice.
public export
data Bytes : Type where
     MkB : (b : Buffer) -> (pos : Int) -> (len : Nat) -> Bytes

-- Performance: For the purposes of proving NonEmpty, if Nat is compiled to Int
-- then this is as performant as if we didn't use Nat but we gain static size
-- computing. However if it's compiled to Integer then we didn't gain anything
-- over simply having two constructors for Bytes, empty and not. The default
-- for Nat to compile to is Integer but realistically Int is safe since you'll
-- run out of memory before you fill up an Int. That's especially the case once
-- we have Word (uint) types if we compile Nat to those, which is only
-- reasonable since Nat can't be negative anyway.
```
Decision made: NonEmpty in this form doens't give enough of an advantage for the hassle it causes in the codebase.