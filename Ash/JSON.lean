import Soda

import Soda.Grape
import Soda.Grape.Text

open Grape
open Function

inductive JSON where
  | obj  : List (String × JSON) → JSON
  | arr  : List JSON → JSON
  | str  : String → JSON
  | num  : Nat → JSON
  | bool : Bool → JSON
  | null : JSON
  deriving Repr, Inhabited

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
