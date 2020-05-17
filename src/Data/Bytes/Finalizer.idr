module Data.Bytes.Finalizer

-- make a scheme hook to call a c cleanup func when object is gc'd

export
data Guardian : Type where
  MkGuardian : Guardian

public export
data ForeignPtr : Type where
  FoPtr : (1 p : AnyPtr) -> ForeignPtr

-- there's a need to investigate how this impacts liftimes
unsafeForeignPtrToPtr : ForeignPtr -> AnyPtr
unsafeForeignPtrToPtr (FoPtr p) = p

export
withForeignPtr : ForeignPtr -> (AnyPtr -> IO a) -> IO a
withForeignPtr fp f = f (unsafeForeignPtrToPtr fp)

guard : String
guard = "
(lambda (val finalizer)
  (let ([foreign-fin (make-guardian)])
    (foreign-fin val)
    0)
  (collect-request-handler
    (lambda ()
      (collect)
      (let f ()
        (let ([x foreign-fin])
          (when x
            (finalizer val)
            (f)))))))
"



-- Register a guardian that will run a finalizer when a ptr goes out of scope.
export
%foreign "scheme:" ++ guard
guardian : (p : AnyPtr) -> (1 fin : AnyPtr -> IO ()) -> PrimIO ()

registerGuardian : (p : AnyPtr) -> (1 fin : AnyPtr -> IO ()) -> IO ()
registerGuardian p fin = primIO $ guardian p fin

export
makeForeignPtr : AnyPtr -> (fin : AnyPtr -> IO ()) -> IO ForeignPtr
makeForeignPtr p f = do
  registerGuardian p f
  pure (FoPtr p)




