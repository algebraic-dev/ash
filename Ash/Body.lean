import Ash.JSON

namespace Ash.Body

class ToBody (e : Type) where
  toBody : e -> String

instance : ToBody String where
  toBody s := s

instance [ToJSON t] : ToBody t where
  toBody t := toString (ToJSON.toJSON t)

instance : ToBody Ash.JSON where
  toBody := toString

end Ash.Body