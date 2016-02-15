package server is
   IP : constant String := "0.0.0.0";
   Port : constant := 55000;
   Max_Connections : constant := 50;

   procedure Start;
   procedure Stop;
end server;
