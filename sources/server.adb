with AWS.Config,
     AWS.Config.Set,
     AWS.Net,
     AWS.Net.WebSocket,
     AWS.Net.WebSocket.Registry.Control,
     AWS.Status,
     AWS.Server;
with GNATCOLL.JSON;
with client_handler,
     exceptions,
     logging,
     quitting,
     server_callbacks;

package body server is
   -----------
   -- Types --
   -----------
   WebServer : AWS.Server.HTTP;
   type WS_type is new AWS.Net.WebSocket.Object with null record;
   overriding procedure On_Open (Socket : in out WS_type; Message : String);
   overriding procedure On_Message (Socket : in out WS_type; Message : String);
   overriding procedure On_Close (Socket : in out WS_type; Message : String);
   overriding procedure On_Error (Socket : in out WS_type; Message : String);

   --------
   -- WS --
   --------
   function Create (Socket : AWS.Net.Socket_Access;
                    Request : AWS.Status.Data) return AWS.Net.WebSocket.Object'Class is
   begin
      logging.L ("Create WS Called: " & AWS.Status.URL (Request));
      return WS_type'(AWS.Net.WebSocket.Object
                      (AWS.Net.WebSocket.Create (Socket, Request)) with null record);
   end Create;

   overriding procedure On_Open (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      logging.L ("WebSocket Open: " & Message);
   end On_Open;
   overriding procedure On_Message (Socket : in out WS_type; Message : String) is
   begin
      declare
         R : constant GNATCOLL.JSON.JSON_Value :=
           client_handler.Client_Message (GNATCOLL.JSON.Read (Message));
         R_String : constant String := R.Write;
      begin
         logging.L ("WebSocket Message: " & Message & " - Responding with: " & R_String);
         Socket.Send (R_String);
         if R.Has_Field ("Close") and then R.Get ("Close") then
            Socket.Close ("Closing");
         end if;
      end;
   exception when E : others =>
         logging.E (exceptions.Error_Message ("Error in WS On_Message", E));
         Socket.Close ("Failed to Interpret Message");
   end On_Message;
   overriding procedure On_Close (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      logging.L ("WebSocket Close: " & Message);
   end On_Close;
   overriding procedure On_Error (Socket : in out WS_type; Message : String) is
      pragma Unreferenced (Socket);
   begin
      logging.E ("WebSocket Error: " & Message);
   end On_Error;

   ----------------
   -- Start Stop --
   ----------------
   procedure Start is
      Config : AWS.Config.Object;
   begin
      AWS.Config.Set.Reuse_Address (Config, True);
      AWS.Config.Set.Server_Host (Config, IP);
      AWS.Config.Set.Server_Port (Config, Port);
      AWS.Config.Set.Server_Name (Config, "Voting Server");
      AWS.Config.Set.Max_Connection (Config, Max_Connections);

      logging.L ("Starting Server");
      AWS.Server.Start (Web_Server => WebServer,
                        Callback   => server_callbacks.Service_HTTP'Access,
                        Config     => Config);

      AWS.Net.WebSocket.Registry.Control.Start;
      AWS.Net.WebSocket.Registry.Register
        (URI => "/ws", Factory => Create'Access);

      quitting.protected_quit.Register_Last_Wish (Stop'Access);
   end Start;

   procedure Stop is
   begin
      logging.L ("Stopping Server");
      AWS.Server.Shutdown (WebServer);
   end Stop;
end server;
