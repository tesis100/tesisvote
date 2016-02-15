with Ada.Calendar.Formatting,
     Ada.Directories,
     Ada.Text_IO;

package body logging is
   protected log is
      function Is_Open return Boolean;

      procedure Log_Out (T : String) with
        Pre => Is_Open;
      procedure Error_Out (T : String);

      procedure Open with
        Pre => not Is_Open,
        Post => Is_Open;
      procedure Close with
        Pre => Is_Open,
        Post => not Is_Open;
   private
      Log_File : Ada.Text_IO.File_Type;
   end log;

   protected body log is
      -- Is Open --
      function Is_Open return Boolean is
      begin return Ada.Text_IO.Is_Open (Log_File);
      end Is_Open;

      -- Outputs --
      procedure Log_Out (T : String) is
         pragma Precondition (Ada.Text_IO.Is_Open (Log_File));
      begin
         Ada.Text_IO.Put_Line (Log_File, T);
         Ada.Text_IO.Flush (Log_File);
      end Log_Out;
      procedure Error_Out (T : String) is
         Error_Prepend : constant String := "***ERROR*** ";
      begin
         Log_Out (Error_Prepend & T);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Error_Prepend & T);
      end Error_Out;

      -- Manage Log File --
      procedure Open is
         use type Ada.Directories.File_Kind;
         pragma Assert (Ada.Directories.Exists (Log_Folder),
                        "Folder doesn't exist: " & Ada.Directories.Full_Name (Log_Folder));
         pragma Assert (Ada.Directories.Kind (Log_Folder) = Ada.Directories.Directory,
                        Ada.Directories.Full_Name (Log_Folder) & " is not a directory");
      begin
         case Ada.Directories.Exists (Log_Name_Path) is
            when True =>
               Ada.Text_IO.Open (File => Log_File,
                                 Mode => Ada.Text_IO.Append_File,
                                 Name => Log_Name_Path);
            when False =>
               Ada.Text_IO.Create (File => Log_File,
                                   Mode => Ada.Text_IO.Out_File,
                                   Name => Log_Name_Path);
         end case;
      end Open;
      procedure Close is
      begin
         Ada.Text_IO.Close (Log_File);
      end Close;
   end log;

   -- Public Functions --
   procedure L (S : String) is begin
      log.Log_Out (S);
   end L;
   procedure E (S : String) is begin
      log.Error_Out (S);
   end E;

   procedure L (S : Ada.Strings.Unbounded.Unbounded_String) is begin
      L (Ada.Strings.Unbounded.To_String (S));
   end L;
   procedure E (S : Ada.Strings.Unbounded.Unbounded_String) is begin
      E (Ada.Strings.Unbounded.To_String (S));
   end E;

   procedure Close_Log is begin
      log.Close;
   end Close_Log;

begin
   log.Open;
   L ("");
   L (" --- Server Started " & Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " --- ");
--     quitting.protected_quit.Register_Last_Wish (Close_Log'Access);
end logging;
