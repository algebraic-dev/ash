
import Ash
import Ash.Parser
import Soda.Grape

import Socket

open Socket
open HTTP


def main : IO Unit := do

  let localAddr ← SockAddr.mk "localhost" "8080" AddressFamily.inet SockType.stream
  IO.println s!"Local Addr: {localAddr}"

  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.bind localAddr
  IO.println "Socket Bound."

  socket.listen 5
  IO.println s!"Listening at http://localhost:8080."

  -- serving
  repeat do
    let (remoteAddr, socket') ← socket.accept
    let t ← IO.asTask do
      let rec looper (p: ByteArray → Result Request): Nat → IO Request
        | 0 => panic! "end"
        | n+1 => do
          let received ← socket'.recv 200
          match p received with
          | Result.done r _  => pure r
          | Result.error _ r => panic! r
          | Result.cont p    => looper p n
      let res ← looper (Grape.run Parser.parseRequest) 10
      IO.println s!"Incoming: {repr res}"
      socket'.close
    IO.println s!"Incoming: {remoteAddr}"