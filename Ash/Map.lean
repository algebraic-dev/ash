inductive HashMap (α: Type) (β: Type) : Type
  | empty
  | fork (left: HashMap α β) (key: α) (hash: UInt64) (val: β) (right: HashMap α β)
  deriving Inhabited, Repr

def HashMap.insert' (map: HashMap α β) (k: α) (h: UInt64) (v: β) [BEq α] [Hashable α] : HashMap α β :=
  match map with
  | empty         => HashMap.fork HashMap.empty k (Hashable.hash k) v HashMap.empty
  | fork l k' h' v' r =>
    if h == h'     then HashMap.fork l k h v r
    else if h > h' then HashMap.fork l k' h' v' (r.insert' k h v)
    else                HashMap.fork (l.insert' k h v) k' h' v' r

def HashMap.insert (map: HashMap α β) (k: α) (v: β) [BEq α] [Hashable α] : HashMap α β :=
  map.insert' k (Hashable.hash k) v

def HashMap.get' (map: HashMap α β) (h: UInt64) : Option β :=
  match map with
  | empty => none
  | fork l _ h' v' r =>
    if h == h'     then some v'
    else if h > h' then r.get' h
    else                l.get' h

def HashMap.find? (map: HashMap α β) (k: α) [BEq α] [Hashable α] : Option β := map.get' (Hashable.hash k)

def HashMap.findD (map: HashMap α β) (k: α) (d: β) [BEq α] [Hashable α] : β :=
  (map.get' (Hashable.hash k)).getD d

def HashMap.toList (map: HashMap α β) : List (α × β) :=
    go [] map
  where
    go (list: List (α × β)) : HashMap α β → List (α × β)
      | empty => list
      | fork l k _ v r =>
          let ls := (k, v) :: list
          let ls := go ls l
          go ls r

def HashMap.fromList [BEq α] [Hashable α] (ls: List (α × β)) : HashMap α β :=
    go HashMap.empty ls
  where
    go (map: HashMap α β) : List (α × β) → HashMap α β
      | (k, v) :: xs => go (map.insert k v) xs
      | []           => map


def HashMap.update' (map: HashMap α β) (k: α) (h: UInt64) (v: β → β) [BEq α] [Hashable α] : HashMap α β :=
  match map with
  | empty         => HashMap.empty
  | fork l k' h' v' r =>
    if h == h'     then HashMap.fork l k h' (v v') r
    else if h > h' then HashMap.fork l k' h' v' (r.update' k h v)
    else                HashMap.fork (l.update' k h v) k' h' v' r

def HashMap.update (map: HashMap α β) (k: α) (v: β → β) [BEq α] [Hashable α] : HashMap α β :=
  map.update' k (Hashable.hash k) v
