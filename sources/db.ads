with GNATCOLL.JSON,
     GNATCOLL.SQL.Exec;

package db is
   pragma Elaborate_Body;
   Backup_Cycle_Duration : constant Duration := 60.0;

   protected db_lock is
      procedure Open;
      procedure Backup;
      procedure Close;
      procedure Initial_Check;
      function Check_Code (S : String) return Boolean;
      function Get_Candidates return GNATCOLL.JSON.JSON_Value;
      procedure Vote (Safe_Votes : GNATCOLL.JSON.JSON_Value; Code : String);
   private
      Database_Description : GNATCOLL.SQL.Exec.Database_Description;
      Database_Connection : GNATCOLL.SQL.Exec.Database_Connection;

      Candidates : GNATCOLL.JSON.JSON_Value;
   end db_lock;
end db;
