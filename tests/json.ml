let bad = "
 { foo\"a\":\"b\" }
"
let good = "
 { \"a\":\"b\" }
"

let _ =
  Printf.printf "Parsing good JSON ... %!";
  let _ = Jsonrpc.of_string good in

  try
    Printf.printf "OK\nParsing bad JSON ... %!";
    let _ = Jsonrpc.of_string bad in
    failwith "The bad JSON should have generated a parse failure"
  with e ->
    Printf.printf "Caught %s:\nOK\n%!" (Printexc.to_string e)
