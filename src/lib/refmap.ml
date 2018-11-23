module M = Map.Make(String)
type 'a inner = 'a M.t
type 'a t = { m : 'a M.t }

let mem key m = M.mem key m.m
let add key v m = {m = M.add key v m.m}
let remove key m = {m = M.remove key m.m}
let update key v m =
  let key' = key in
  {m = M.add key' v (M.remove key' m.m)}
let mem key m = M.mem key m.m 
let find key m = M.find key m.m
let keys m = M.fold (fun k _ acc -> k::acc) m.m []
let empty = {m=M.empty}
