open Lwt
open Js

let do_rpc enc dec content_type ~url call =
  let method_ = "POST" in
  let contents = enc call in
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in

  req##_open (Js.string method_, Js.string url, Js._true);
  req##setRequestHeader (Js.string "Content-type", Js.string content_type);
  req##onreadystatechange <- Js.wrap_callback
    (fun _ ->
       (match req##readyState with
		   | XmlHttpRequest.DONE ->
			   Lwt.wakeup w (dec (Js.to_string req##responseText))
		   | _ -> ()));

  req##send (Js.some (Js.string contents));

  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

let do_xml_rpc = do_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string "text/xml"
let do_json_rpc = do_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string "text/json"
let do_json_rpc_opt = do_rpc Rpc_client_js_helper.string_of_call Rpc_client_js_helper.response_of_string "text/json"
