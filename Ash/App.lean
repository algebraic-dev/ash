import Ash.JSON
import Ash.Path
import Melp
import Lina

open Melp.Request

namespace Ash

inductive Method where
  | Get
  | Post
  | Put
  | Delete
  | Patch
  | Head
  | Options

def Method.toString : Method → String
  | Get => "GET"
  | Post => "POST"
  | Put => "PUT"
  | Delete => "DELETE"
  | Patch => "PATCH"
  | Head => "HEAD"
  | Options => "OPTIONS"

def Component : Type := Melp.Request Melp.Request.Status.Open 
                     → Lean.HashMap String String
                     → Ash.Path
                     → IO (Melp.Request Melp.Request.Status.Closed)

structure Route where
  path      : List Ash.Pattern
  method    : Method
  component : Component

structure App (e : Type) where
  data : (Array Route -> IO (e × Array Route))

-- Maps the first element
def Prod.first {α β} :  (α → γ) → Prod α β →Prod γ β
  | f, ⟨a, b⟩ => ⟨f a, b⟩


instance : Monad App where
  map f b := App.mk (λroutes => Prod.first f <$> (b.data routes))
  pure f  := App.mk (λroutes => pure (f, routes))
  seq f a := App.mk $ λroutes => do
    let ⟨f, routes'⟩ ← f.data routes
    (λ⟨a, routes⟩ => ⟨f a, routes⟩) <$> ((a ()).data routes')
  bind f a := App.mk $ λroutes => do
    let ⟨f, routes'⟩ ← f.data routes
    (a f).data routes'

def App.on (method : Method) (path : String) (component : Component) : App Unit :=
  App.mk $ λroutes => do
    let path   := Ash.Path.Pattern.parse path
    let routes := routes.push { path := path, method := method, component := component }
    pure ((), routes)

def App.get (path : String) (component : Component) : App Unit :=
  on Method.Get path component

def App.post (path : String) (component : Component) : App Unit :=
  on Method.Post path component

def App.put (path : String) (component : Component) : App Unit :=
  on Method.Put path component

def App.delete (path : String) (component : Component) : App Unit :=
  on Method.Delete path component
  
def App.run (app: App f) (addr : String) (port : String) : IO Unit := do
    let server ← Melp.Server.new 

    let ⟨_, routes⟩ ← app.data #[]

    let server := server.onConnection $ λconn => do
      let mut foundRoute := none

      for route in routes do
        if route.method.toString == conn.data.method 
          then 
            match parsePath route.path conn.data.path with
            | some (params, path) => do
              foundRoute := some (route.component conn params path)
              break
            | none => continue
          else continue
      
      match foundRoute with
      | some result => result
      | none => conn.notFound

    Melp.Server.start server addr port
  where
    parsePath (pattern: List Ash.Pattern) (path : String) : Option (Lean.HashMap String String × Ash.Path) := do
      let path   ← Ash.Path.parse path
      let match' ← Ash.Path.matchPats pattern path
      pure (match', path)

end Ash