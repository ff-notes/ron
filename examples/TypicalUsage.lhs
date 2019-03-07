> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE QuasiQuotes #-}
>
> module Main where
>
> import RON.Data
> import RON.Schema.TH
> import RON.Storage.FS as Storage
>
> [mkReplicated|
>     (struct_lww Note
>         active Boole
>         text RgaString)
> |]
>
> instance Collection Note where
>     collectionName = "note"
>
> main :: IO ()
> main = do
>     let dataDir = "./data/"
>     h <- Storage.newHandle dataDir
>     runStorage h $ do
>         obj <- newObject
>             Note{active = True, text = "Write a task manager"}
>         createDocument obj
