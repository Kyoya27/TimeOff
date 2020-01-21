namespace Storage.Events

open Storage
open System.IO

type private FileStream<'TValue> (path: string) =
  interface IStream<'TValue> with
    member __.ReadAll() =
      let values = new ResizeArray<'TValue>()
      let stream = new FileStream(path, FileMode.OpenOrCreate, FileAccess.Read)
      use reader = new StreamReader(stream)
      let mutable keepLooping = true
      while keepLooping do
        let line = reader.ReadLine()
        if isNull line then
          keepLooping <- false
        else
          let value = Serialization.deserialize<'TValue> line
          values.Add value
      values :> seq<_>

    member __.Append (values:'TValue list) =
      let stream = new FileStream(path, FileMode.Append, FileAccess.Write)
      use writer = new StreamWriter(stream)
      for value in values do
        let line = Serialization.serialize value
        writer.WriteLine line

type private DirectoryStore<'TKey, 'TValue when 'TKey: comparison> (path: string, keyFormatter: 'TKey -> string) =
  do
    if not (Directory.Exists path) then
      Directory.CreateDirectory(path) |> ignore

  interface IStore<'TKey, 'TValue> with
    member __.GetStream(key: 'TKey) =
      let filePath = Path.Combine(path, keyFormatter key)
      FileStream<'TValue>(filePath) :> IStream<'TValue>

type FileSystemStore =
  static member Create<'TKey, 'TValue when 'TKey: comparison>(path: string, keyFormatter: 'TKey -> string) =
    DirectoryStore<'TKey, 'TValue>(path, keyFormatter) :> IStore<'TKey, 'TValue>
