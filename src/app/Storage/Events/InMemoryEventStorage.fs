namespace Storage.Events

type private InMemoryStream<'TValue> () =
  let stream = ResizeArray<'TValue>()

  interface IStream<'TValue> with
    member __.ReadAll() =
      stream :> seq<_>

    member __.Append (values:'TValue list) =
      stream.AddRange values

type InMemoryStream =
  static member Create<'TValue>() =
    InMemoryStream<'TValue>() :> IStream<'TValue>

type private InMemoryStore<'TKey, 'TValue when 'TKey: comparison> () =
  let mutable streams: Map<'TKey, IStream<'TValue>> = Map.empty

  interface IStore<'TKey, 'TValue> with
    member __.GetStream(key: 'TKey) =
      match streams.TryFind key with
      | Some stream -> stream
      | None ->
          let stream = InMemoryStream.Create()
          streams <- streams.Add (key, stream)
          stream

type InMemoryStore =
  static member Create<'TKey, 'TValue when 'TKey: comparison>() =
    InMemoryStore<'TKey, 'TValue>() :> IStore<'TKey, 'TValue>
