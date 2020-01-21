namespace Storage.Events

type IStream<'TValue> =
  abstract member ReadAll: unit -> 'TValue seq
  abstract member Append: values:'TValue list -> unit

type IStore<'TKey, 'TValue when 'TKey: comparison> =
  abstract member GetStream: 'TKey -> IStream<'TValue>
