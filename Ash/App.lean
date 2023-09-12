-- Entrypoint of the framework. It defines the App monad and the App.run function. 

import Ash.JSON
import Ash.Path
import Ash.Body

import Melp
import Lina

open Ash.Body

open Melp.Request

namespace Ash

structure Request where
  melp     : Melp.Request
  bindings : Lean.HashMap String String
  path     : Ash.Path
  query    : Lean.HashMap String String

def headers (s: String) : List (String × String) :=
  [ ("Content-Length", toString s.length),
    ("Content-Type", "application/json")
  ]

def Request.body (req: Request) : String :=
  req.melp.data.body

def Request.json [FromJSON e] (req: Request) : Option e :=
  FromJSON.fromJSON =<< JSON.parse req.melp.data.body

def Request.ok [ToBody e] (_req: Request) (body : e) : IO Melp.Response :=
  let body := ToBody.toBody body
  pure { status := Melp.Status.Ok, headers := headers body, body := body }

def Request.created [ToBody e] (_req: Request) (body : e) (location: String) : IO Melp.Response :=
  let body := ToBody.toBody body
  let location := ⟨"Location", location⟩
  pure { status := Melp.Status.Created, headers := location :: headers body, body := body }

def Request.unprocessableEntity [ToBody e] (_req: Request) (body : e) : IO Melp.Response :=
  let body := ToBody.toBody body
  pure { status := Melp.Status.UnprocessableEntity, headers := headers body, body := body }

def Request.badRequest [ToBody e] (_req: Request) (body : e) : IO Melp.Response :=
  let body := ToBody.toBody body
  pure { status := Melp.Status.BadRequest, headers := headers body, body := body }

def Request.notFound [ToBody e] (_req: Request) (body : e) : IO Melp.Response :=
  let body := ToBody.toBody body
  pure { status := Melp.Status.NotFound, headers := headers body, body := body }

def Component : Type
  := Ash.Request
   → IO (Melp.Response)

structure Route where
  path      : List Ash.Pattern
  method    : Melp.Method
  component : Component

structure Router (α: Type) where
  routes : HashMap Melp.Method (Ash.RouteMap α)

def Router.push (router: Router Component) (path: List Ash.Pattern) (method: Melp.Method) (comp: Component) : Router Component :=
  { router with routes := router.routes.update method (·.insert path comp) }

def Router.get (router: Router Component) (path: Path) (method: Melp.Method) : Option (Component × Lean.HashMap String String) := do
  let map ← router.routes.find? method
  map.get path

-- Yeah it's a state monad but I don't care ok? I'll adjust it in the future.
structure App (e : Type) where
  data : (Router Component -> IO (e × Router Component))

-- Maps the first element
def Prod.first {α β} :  (α → γ) → Prod α β →Prod γ β
  | f, ⟨a, b⟩ => ⟨f a, b⟩

-- Horrible looking code. I wish I could use a state monad here but it's too late, I already wrote the code.
instance : Monad App where
  map f b := App.mk (λroutes => Prod.first f <$> (b.data routes))
  pure f  := App.mk (λroutes => pure (f, routes))
  seq f a := App.mk $ λroutes => do
    let ⟨f, routes'⟩ ← f.data routes
    (λ⟨a, routes⟩ => ⟨f a, routes⟩) <$> ((a ()).data routes')
  bind f a := App.mk $ λroutes => do
    let ⟨f, routes'⟩ ← f.data routes
    (a f).data routes'


def App.on (method : Melp.Method) (path : String) (component : Component) : App Unit :=
  App.mk $ λroutes => do
    let path   := Ash.Path.Pattern.parse path
    let routes := routes.push path method component
    pure ((), routes)

def App.get (path : String) (component : Component) : App Unit :=
  on Melp.Method.Get path component

def App.post (path : String) (component : Component) : App Unit :=
  on Melp.Method.Post path component

def App.put (path : String) (component : Component) : App Unit :=
  on Melp.Method.Put path component

def App.delete (path : String) (component : Component) : App Unit :=
  on Melp.Method.Delete path component

def App.run (app: App f) (addr : String) (port : String) (callback: IO Unit) : IO Unit := do
    let server ← Melp.Server.new

    let ⟨_, routes⟩ ← app.data (Router.mk HashMap.empty)

    let server := server.onConnection $ λconn => do
      let mut foundRoute := none

      let path := Ash.Path.parse conn.data.path

      match conn.method, path with
      | none, _ | _, none => pure ()
      | some method, some path =>
          match routes.get path method with
          | none => pure ()
          | some (comp, params) => do
            let request :=
              { melp     := conn
              , bindings := params
              , path     := path
              , query    := path.query.getD Lean.HashMap.empty }
            foundRoute := some (comp request)

      match foundRoute with
      | some result => do
          let response ← result
          conn.answer response.status response.headers response.body
      | none => conn.answer Melp.Status.NotFound [] "Not found"

    let server := server.onBind $ λ_ => callback

    Melp.Server.start server addr port

end Ash