with GNATCOLL.SQL_Impl,
     GNATCOLL.SQL.Exec;

package db_types is
   DB_Filename : constant String   := "codes.db";

   type Code_Status_Type is (Unused_Code, Voting_Code, Used_Code);
   function Image (S : Code_Status_Type) return String;
   function Value (S : String) return Code_Status_Type;

   function Image (C : GNATCOLL.SQL.Exec.Forward_Cursor) return String;

   type Formatter_Type is new GNATCOLL.SQL_Impl.Formatter with null record;
   overriding function Field_Type_Autoincrement (Self : Formatter_Type) return String;
   overriding function Field_Type_Money         (Self : Formatter_Type) return String;
   Formatter : Formatter_Type;

   type Error_Reporter is new GNATCOLL.SQL.Exec.Error_Reporter with null record;
   overriding procedure On_Database_Corrupted (Self       : in out Error_Reporter;
                                               Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class);
   overriding procedure On_Warning (Self       : in out Error_Reporter;
                                    Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class;
                                    Message    : String);
   overriding procedure On_Error (Self       : in out Error_Reporter;
                                  Connection : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class;
                                  Message    : String);
   E_Reporter : aliased Error_Reporter;
end db_types;
