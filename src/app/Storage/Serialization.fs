namespace Storage

module private Serialization =
  open Newtonsoft.Json
  open Newtonsoft.Json.Serialization
  let jsonSerializerSettings = JsonSerializerSettings()
  jsonSerializerSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let serialize value = JsonConvert.SerializeObject(value, jsonSerializerSettings)
  let deserialize<'TValue> json = JsonConvert.DeserializeObject<'TValue>(json)