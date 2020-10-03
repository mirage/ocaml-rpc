module AD = struct
  include Async.Deferred

  let bind x f = Async.Deferred.bind ~f x
  let fail = raise
end

module AsyncIdl = Idl.Make (AD)
include AsyncIdl
