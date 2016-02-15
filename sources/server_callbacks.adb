package body server_callbacks is
   function Service_HTTP (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build (Content_Type  => "text/plain",
                                 Message_Body  => "Hello World!");
   end Service_HTTP;
end server_callbacks;
