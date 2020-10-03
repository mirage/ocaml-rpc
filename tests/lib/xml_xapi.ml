let array_call =
  "<methodCall>\n\
  \  <methodName>event.register</methodName>\n\
  \  <params>\n\
  \    <param>\n\
  \      <value>OpaqueRef:8ecbbb2a-a905-d422-1153-fadc00639b12</value>\n\
  \    </param>\n\
  \    <param>\n\
  \      <value>\n\
  \        <array>\n\
  \          <data>\n\
  \            <value>pbd</value>\n\
  \          </data>\n\
  \        </array>\n\
  \      </value>\n\
  \    </param>\n\
  \  </params>\n\
   </methodCall>\n"


let simple_call =
  "<methodCall>\n\
  \  <methodName>session.login_with_password</methodName>\n\
  \  <params>\n\
  \    <param>\n\
  \      <value/>\n\
  \    </param>\n\
  \    <param>\n\
  \      <value/>\n\
  \    </param>\n\
  \    <param>\n\
  \      <value>1.4</value>\n\
  \    </param>\n\
  \  </params>\n\
   </methodCall>\n"


let error =
  "<methodResponse>\n\
   <fault>\n\
   <value><struct>\n\
   <member>\n\
   <name>faultCode</name>\n\
   <value><int>143</int></value>\n\
   </member>\n\
   <member>\n\
   <name>faultString</name>\n\
   <value><string>Failed to parse the request</string></value>\n\
   </member>\n\
   </struct></value>\n\
   </fault>\n\
   </methodResponse>\n"


let sm =
  "<?xml version='1.0'?>\n\
   <methodResponse>\n\
   <params>\n\
   <param>\n\
   <value><struct>\n\
   <member>\n\
   <name>required_api_version</name>\n\
   <value><string>1.0</string></value>\n\
   </member>\n\
   <member>\n\
   <name>vendor</name>\n\
   <value><string>Citrix Systems Inc</string></value>\n\
   </member>\n\
   <member>\n\
   <name>name</name>\n\
   <value><string>Local EXT3 VHD</string></value>\n\
   </member>\n\
   <member>\n\
   <name>copyright</name>\n\
   <value><string>(C) 2008 Citrix Systems Inc</string></value>\n\
   </member>\n\
   <member>\n\
   <name>capabilities</name>\n\
   <value><array><data>\n\
   <value><string>SR_PROBE</string></value>\n\
   <value><string>SR_UPDATE</string></value>\n\
   <value><string>VDI_CREATE</string></value>\n\
   <value><string>VDI_DELETE</string></value>\n\
   <value><string>VDI_ATTACH</string></value>\n\
   <value><string>VDI_DETACH</string></value>\n\
   <value><string>VDI_UPDATE</string></value>\n\
   <value><string>VDI_CLONE</string></value>\n\
   <value><string>VDI_SNAPSHOT</string></value>\n\
   <value><string>VDI_RESIZE</string></value>\n\
   <value><string>VDI_RESIZE_ONLINE</string></value>\n\
   </data></array></value>\n\
   </member>\n\
   <member>\n\
   <name>driver_version</name>\n\
   <value><string>1.0</string></value>\n\
   </member>\n\
   <member>\n\
   <name>configuration</name>\n\
   <value><array><data>\n\
   <value><struct>\n\
   <member>\n\
   <name>description</name>\n\
   <value><string>local device path (required) (e.g. /dev/sda3)</string></value>\n\
   </member>\n\
   <member>\n\
   <name>key</name>\n\
   <value><string>device</string></value>\n\
   </member>\n\
   </struct></value>\n\
   </data></array></value>\n\
   </member>\n\
   <member>\n\
   <name>description</name>\n\
   <value><string>SR plugin which represents disks as VHD files stored on a local EXT3 \
   filesystem, created inside an LVM volume</string></value>\n\
   </member>\n\
   </struct></value>\n\
   </param>\n\
   </params>\n\
   </methodResponse>\n"


let base64 =
  "\n\
  \  <methodResponse>\n\
   <params>\n\
   <param>\n\
   <value><base64>SGVsbG8sIHdvcmxkIQ==</base64></value>\n\
   </param>\n\
   </params>\n\
   </methodResponse>\n\n"


let base64_call =
  "<methodCall>\n\
  \  <methodName>send_file</methodName>\n\
  \  <params>\n\
  \    <param>\n\
  \      <value>\n\
  \   <base64>SGVsbG8sIHdvcmxkIQ==</base64> </value>\n\
  \    </param>\n\
  \  </params>\n\
   </methodCall>\n"


let empty = "<value></value>"

let run () =
  Printf.printf "Parsing SM XML ... %!";
  let _ = Xmlrpc.response_of_string sm in
  Printf.printf "OK\nParsing empty tags ... %!";
  let _ = Xmlrpc.of_string empty in
  Printf.printf "OK\nParsing error ... %!";
  let _ = Xmlrpc.response_of_string error in
  Printf.printf "OK\nParsing simple call ... %!";
  let _ = Xmlrpc.call_of_string simple_call in
  Printf.printf "OK\nParsing array call ... %!";
  let _ = Xmlrpc.call_of_string array_call in
  Printf.printf "OK\n%!";
  let b64 = Xmlrpc.response_of_string base64 in
  Printf.printf "Base64 response: %s\n" (Rpc.to_string b64.contents);
  assert (b64.contents = Rpc.Base64 "Hello, world!");
  let b64_req = Xmlrpc.call_of_string base64_call in
  Printf.printf "Base64 request: %s\n" (Rpc.string_of_call b64_req);
  assert (List.hd b64_req.params = Rpc.Base64 "Hello, world!")


let tests = [ "Xapi XML tests", `Quick, run ]
