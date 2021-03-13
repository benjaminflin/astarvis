module JS.Console

%foreign "browser:lambda:x=>console.log(x)"
prim__log : String -> PrimIO ()

export 
log : String -> IO ()
log = primIO . prim__log


