import Ash.JSON

namespace Ash.Body

class ToBody (e : Type) where
  toBody : e -> String

instance : ToBody String where
  toBody s := s

partial def toBodyJSON : Ash.JSON → String 
  | Ash.JSON.obj obj           => 
    let entries := obj.map $ λ (k, v) => 
      let body := toBodyJSON v
      s!"\"{k}\": {body}"
    let entries := String.intercalate ", " entries
    s!"\{{entries}}"
  | Ash.JSON.null              => "null"
  | Ash.JSON.bool true         => "true"
  | Ash.JSON.bool false        => "false"
  | Ash.JSON.num n             => s!"{n}"
  | Ash.JSON.str str           => s!"\"{str}\""
  | Ash.JSON.arr arr           => 
    let entries := arr.map toBodyJSON
    let entries := String.intercalate ", " entries
    s!"[{entries}]"

instance : ToBody Ash.JSON where
  toBody := toBodyJSON

end Ash.Body