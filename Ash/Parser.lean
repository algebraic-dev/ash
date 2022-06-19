import Soda.Grape
import Soda.Grape.Text
import Soda.Data.String

open Grape
open Function

namespace HTTP

inductive Method where
  | Get
  | Post
  deriving Repr

structure Request where
  version: Nat × Nat
  method: Method
  uri: String
  headers: List (String × String)
  body: Option String
  deriving Repr

namespace Parser
namespace Char

@[inline] def isSpace := String.oneOf " \t"

@[inline] def isCr    := String.oneOf "\r¬"

@[inline] def isSpaceOrColon := String.oneOf " \t:"

@[inline] def isSpaceOrColonCR (x: UInt8) := isSpaceOrColon x || isCr x

@[inline] def isSeparator := String.oneOf "()<>@,;:\\\"/[]?={} \t"

@[inline] def isToken (imp: UInt8) := imp > 31 && imp != 127 && (not $ isSeparator imp)

end Char

@[inline] def token  := takeWhile1 Char.isToken

@[inline] def spaces := takeWhile Char.isSpace

@[inline] def number := Text.number

@[inline] def eol    := label "eol" (string "\r\n")

def version: Grape (Nat × Nat) := label "version" $ do
  let major ← string "HTTP/" *> number
  let minor ← chr '.' *> number
  Grape.pure (major, minor)

def header: Grape (String × String) := label "header" $ do
  let key ← takeWhile (not ∘ Char.isSpaceOrColonCR) <* chr ':' <* spaces
  let val ← takeWhile (not ∘ Char.isCr) <* eol
  Grape.pure (String.fromUTF8Unchecked key,  String.fromUTF8Unchecked val)

def headers: Grape (List (String × String)) := list1 header

def throwOpt : Option n → String → Grape n
  | some x, _ => Grape.pure x
  | none, err => throw err

def toMethod (s: ByteArray): Option Method :=
  match String.fromUTF8Unchecked s with
  | "GET" => some Method.Get
  | "POST" => some Method.Post
  |  _ => none

def method : Grape Method := label "method" $ do
  let met ← token
  throwOpt (toMethod met) "not a valid method"

-- PROBABLY UNSAFE.
def toStr (p: Grape ByteArray): Grape String := String.fromUTF8Unchecked <$> p

-- It not parses the URI as an URI, it's the task of another module.
def uri := label "uri" $ toStr $ takeWhile1 (not ∘ Char.isSpace)

def parseRequest: Grape Request := do
    let req ← mkReq
      <$> method   <* chr ' '
      <*> uri      <* chr ' '
      <*> version  <* eol
      <*> headers  <* eol

    let size := req.headers.lookup "Content-Length" >>= String.toNat?

    match size with
    | none      => Grape.pure req
    | some size => do
        let body ← toStr (takeN size)
        Grape.eof
        Grape.pure { req with body }

  where
    mkReq method uri version headers: Request :=
      { version, method, uri, headers, body := none }

end Parser
end HTTP