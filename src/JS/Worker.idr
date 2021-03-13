module JS.Worker

%foreign "browser:lambda:x => new Worker(x)" 
prim__mkWorker : String -> PrimIO AnyPtr

%foreign "browser:lambda:(w, x) => w.postMessage(x)" 
prim__postMessage : AnyPtr -> String -> PrimIO ()

%foreign "browser:lambda:x => postMessage(x)"
prim__workerPostMessage : String -> PrimIO () 

%foreign "browser:lambda:(w, c) => {w.onMessage = c}" 
prim__onMessage : AnyPtr -> (String -> PrimIO ()) -> PrimIO ()

%foreign "browser:lambda:c => {self.onMessage = c}"
prim__workerOnMessage : (String -> PrimIO ()) -> PrimIO ()

data Worker = MkWorker AnyPtr 

export
mkWorker : String -> IO Worker
mkWorker s = pure $ MkWorker !(primIO $ prim__mkWorker s)


export
postMessage : Worker -> String -> IO ()   
postMessage (MkWorker w) s = primIO $ prim__postMessage w s


export
onMessage : Worker -> (String -> IO ()) -> IO ()
onMessage (MkWorker w) f = primIO $ prim__onMessage w (\s => toPrim $ f s)


export
workerPostMessage : String -> IO ()
workerPostMessage s = primIO $ prim__workerPostMessage s


export
workerOnMessage : (String -> IO ()) -> IO ()
workerOnMessage f = primIO $ prim__workerOnMessage (\s => toPrim $ f s)  

