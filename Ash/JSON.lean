import Soda

import Soda.Grape
import Soda.Grape.Text

open Grape
open Function

def sequence : List (Option t) → Option (List t)
  | some x :: y => (x :: ·) <$> (sequence y)
  | none   :: _ => none
  | []          => some []

namespace Ash

inductive JSON where
  | obj  : List (String × JSON) → JSON
  | arr  : List JSON → JSON
  | str  : String → JSON
  | num  : Nat → JSON
  | bool : Bool → JSON
  | null : JSON
  deriving Repr, Inhabited

class FromJSON (e : Type) where
  fromJSON : JSON → Option e

instance : FromJSON String where
  fromJSON 
    | (JSON.str s) => some s
    | _            => none

instance : FromJSON Nat where
  fromJSON 
    | (JSON.num s) => some s
    | _            => none

instance : FromJSON Bool where
  fromJSON 
    | (JSON.bool s) => some s
    | _            => none

instance : FromJSON JSON where
  fromJSON f := f

instance [FromJSON t] : FromJSON (Option t) where
  fromJSON 
    | JSON.null       => none
    | t               => some (FromJSON.fromJSON t)

instance [FromJSON t] : FromJSON (List t) where
  fromJSON 
    | (JSON.arr arr)  => sequence $ FromJSON.fromJSON <$> arr
    | _               => none

class ToJSON (e : Type) where
  toJSON : e → JSON

instance [ToJSON f] : ToJSON (List f) where
  toJSON := JSON.arr ∘ List.map ToJSON.toJSON

instance [ToJSON f] : ToJSON (Array f) where
  toJSON := JSON.arr ∘ Array.toList ∘ Array.map ToJSON.toJSON

instance [ToJSON f] : ToJSON (List (String × f)) where
  toJSON := JSON.obj ∘ List.map (λ (k, v) => (k, ToJSON.toJSON v))

instance : ToJSON String where
  toJSON := JSON.str

instance : ToJSON Nat where
  toJSON := JSON.num

instance : ToJSON Bool where
  toJSON := JSON.bool

instance : ToJSON JSON where
  toJSON := id

def JSON.token : Grape α → Grape α := Text.trailing

def JSON.pString : Grape String :=
  string "\"" *> Text.takeToStr (· != 34) <* string "\""

def JSON.number : Grape Nat :=
  Text.number

def JSON.space : Grape Unit := list (oneOf " \n\r\t") *> Grape.pure ()

partial def JSON.expr : Grape JSON := token $
        (str <$> label "string" pString)
    <|> (num <$> label "number" number)
    <|> ((λ_ => JSON.null)       <$> label "null" (string "null"))
    <|> ((λ_ => JSON.bool true)  <$> label "true" (string "true"))
    <|> ((λ_ => JSON.bool false) <$> label "false" (string "false"))
    <|> (arr <$> (string "[" *> space *> sepBy expr (space *> (token $ string ",") <* space) <* space <* (token $ string "]")))
    <|> (obj <$> (string "{" *> space *> sepBy pair (space *> (token $ string ",") <* space) <* space <* (token $ string "}")))
  where
    pair := Prod.mk <$> (JSON.pString <* (space *> (token $ string ":") <* space)) <*> expr

def JSON.parse (s: String) : Option JSON := 
  match Grape.run JSON.expr (s.toSlice) with
  | Result.done res _ => some res
  | _                 => none

def JSON.find? [FromJSON e] (json: JSON) (k: String) : Option e :=
  match json with
  | JSON.obj object => List.lookup k object >>= FromJSON.fromJSON
  | _               => none

namespace JSON

syntax (priority := high) "`{" term,* "}" : term

/- Declares two expansions/syntax transformers -/
macro_rules
  | `(`{})           => `(JSON.obj [])
  | `(`{$xs:term,*}) => `(ToJSON.toJSON [$xs,*])

notation:max k "+:" v   => (k, ToJSON.toJSON v) 

end JSON
end Ash