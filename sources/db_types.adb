with Ada.Characters.Handling;
with logging;

package body db_types is
   function Image (S : Code_Status_Type) return String is
   begin
      case S is
         when Unused_Code => return "unused";
         when Used_Code => return "used";
         when Voting_Code => return "voting";
      end case;
   end Image;

   function Value (S : String) return Code_Status_Type is
      T : constant String := Ada.Characters.Handling.To_Lower (S);
   begin
      if    T = "unused" then return Unused_Code;
      elsif T = "used"   then return Used_Code;
      elsif T = "voting" then return Voting_Code;
      else raise Program_Error with "Invalid Code_Status_Type: " & S;
      end if;
   end Value;

   function Image (C : GNATCOLL.SQL.Exec.Forward_Cursor) return String is
      function Image_Sub (F : GNATCOLL.SQL.Exec.Field_Index) return String is
         use type GNATCOLL.SQL.Exec.Field_Index;
      begin
         case F = C.Field_Count - 1 is
            when True => return C.Value (F);
            when False => return C.Value (F) & " | " & Image_Sub (F + 1);
         end case;
      end Image_Sub;
   begin
      return Image_Sub (0);
   end Image;

   --------------
   -- Formater --
   --------------
   overriding function Field_Type_Autoincrement (Self : Formatter_Type) return String is
      pragma Unreferenced (Self);
   begin return "AUTOINCREMENT"; end Field_Type_Autoincrement;

   overriding function Field_Type_Money (Self : Formatter_Type) return String is
      pragma Unreferenced (Self);
   begin return "FLOAT"; end Field_Type_Money;

   --------------------
   -- Error Reporter --
   --------------------
   overriding procedure On_Database_Corrupted
     (Self       : in out Error_Reporter;
      Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class) is
      pragma Unreferenced (Self, Connection);
   begin
      logging.E ("Database Corrupt Error");
   end On_Database_Corrupted;

   overriding procedure On_Warning
     (Self       : in out Error_Reporter;
      Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class;
      Message    : String) is
      pragma Unreferenced (Self, Connection);
   begin
      logging.L ("Database Warning: " & Message);
   end On_Warning;

   overriding procedure On_Error (Self       : in out Error_Reporter;
                                  Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class;
                                  Message    : String) is
      pragma Unreferenced (Self, Connection);
   begin
      logging.E ("Database ERROR: " & Message);
   end On_Error;
end db_types;
