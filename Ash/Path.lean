-- A simple URI Parser based on https://www.w3.org/Addressing/URL/5_URI_BNF.html
import Lean.Data.HashMap

import Soda
import Soda.Grape
import Soda.Grape.Text

import Ash.Map

open Grape
open Grape.Text
open Function

namespace Ash

inductive Pattern
  | Literal : String → Pattern
  | Variable : String → Pattern
  deriving Repr

@[inline]
def takeToStrEmpty (pred: UInt8 → Bool) : Grape String :=
  Grape.map ByteSlice.toASCIIString (takeWhile pred)

def Path.Pattern.text : Grape String := takeToStrEmpty (· != 47)

def Path.Pattern.part : Grape Pattern :=
  (Pattern.Variable <$> (string ":" *> text)) <|>
  (Pattern.Literal  <$> text)

def Path.Pattern.pat : Grape (List Pattern) :=
  sepBy Path.Pattern.part (chr '/')

def Path.Pattern.parse (pattern: String) : List Pattern :=
    garantee (Grape.run (Path.Pattern.pat) (pattern.toSlice))
  where
    garantee : Result (List Pattern) → List Pattern
      | Result.done res _ => res
      | Result.cont e     => garantee (e "".toSlice)
      | Result.error _ e  => [Pattern.Literal e]

-- Path parsing

structure Path where
  segments : List String
  query : Option (Lean.HashMap String  String)

def Path.toHex : Char -> Nat
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | 'a' | 'A' => 10
  | 'b' | 'B' => 11
  | 'c' | 'C' => 12
  | 'd' | 'D' => 13
  | 'e' | 'E' => 14
  | 'f' | 'F' => 15
  | _ => 0

def escaped (a b : Char) : UInt8 :=
  let fst := UInt8.ofNat (Path.toHex a * 16)
  let snd := UInt8.ofNat (Path.toHex b)
  fst + snd

def Path.hex := Char.ofNat <$> UInt8.toNat <$> is Char.hex
def Path.digit := is (λchr => chr >= 48 && chr <= 57)
def Path.reserved := oneOf "=;/#?: "
def Path.extra := oneOf "!*\"'(),"
def Path.safe := oneOf "$-_.+"
def Path.alpha := is (λchr => Char.upperCase chr || Char.lowerCase chr)
def Path.escape := chr '%' *> (escaped <$> Path.hex <*> Path.hex)

def Path.xalpha := alpha <|> digit <|> safe <|> extra <|> escape
def Path.xalphas := String.fromUTF8Unchecked <$> List.toByteArray <$> list Path.xalpha

partial def Path.parse_query : Grape (Lean.HashMap String String) := do
  let key ← xalphas
  let value ← string "=" *> xalphas
  let rest ← option (string "&" *> Path.parse_query)
  match rest with
  | none => Grape.pure (Lean.HashMap.empty.insert key value)
  | some rest => Grape.pure (rest.insert key value)

def Path.parse_path := do
  let segments ← sepBy Path.xalphas (string "/")
  let query ← option (string "?" *> Path.parse_query)
  Grape.pure (Path.mk segments query)

def Path.parse (str: String) : Option Path :=
    garantee (Grape.run parse_path (str.toSlice))
  where
    garantee : Result Path → Option Path
      | Result.done res _ => some res
      | Result.cont e     => garantee (e "".toSlice)
      | Result.error _ _  => none

inductive RouteMap (α: Type)
  | fork : HashMap String (RouteMap α) → Option α → Option String → RouteMap α
  deriving Repr

def RouteMap.empty : RouteMap α := RouteMap.fork HashMap.empty none none

def RouteMap.insert : RouteMap α → List Pattern → α → RouteMap α
  | RouteMap.fork m _ o, [], x =>
      RouteMap.fork m (some x) o
  | RouteMap.fork m r o, Pattern.Literal x  :: xs, a =>
      RouteMap.fork (m.insert x (RouteMap.insert (m.findD x RouteMap.empty) xs a)) r o
  | RouteMap.fork m r _, Pattern.Variable x :: xs, a =>
      RouteMap.insert (RouteMap.fork m r (some x)) xs a

def RouteMap.get (x: RouteMap α) (path: Path) : Option (α × Lean.HashMap String String) :=
  let rec match' : RouteMap α → List String → Lean.HashMap String String → Option (α × Lean.HashMap String String)
    | RouteMap.fork _ res _, [], bindings => do
      let resp ← res
      return (resp, bindings)
    | RouteMap.fork m resp (some var) , x :: xs , bindings =>
      let bindings := bindings.insert var x
      match xs with
      | []      => do
        let res ← resp
        return (res, bindings)
      | y :: xs => do
        let res ← m.find? y
        match' res xs bindings
    | RouteMap.fork m _ none, x :: xs, bindings => do
        let res ← m.find? x
        match' res xs bindings
  match' x path.segments ({})

end Ash