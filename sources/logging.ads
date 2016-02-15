with Ada.Strings.Unbounded;

package logging is
   Log_Folder : constant String := "log";
   Log_Name_Path : constant String := Log_Folder & "/voting.log";

   procedure L (S : String);
   procedure E (S : String);

   procedure L (S : Ada.Strings.Unbounded.Unbounded_String);
   procedure E (S : Ada.Strings.Unbounded.Unbounded_String);

   procedure Close_Log;
end logging;
