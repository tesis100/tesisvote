with Ada.Calendar.Formatting,
     Ada.Characters.Handling,
     Ada.Directories,
     Ada.Strings.Unbounded;
with GNATCOLL.SQL.Sqlite;
with codes_db,
     db_types,
     exceptions,
     logging,
     quitting;

package body db is
   protected body db_lock is
      procedure Initial_Check is
         use type GNATCOLL.SQL.SQL_Field_List;
         C : GNATCOLL.SQL.Exec.Forward_Cursor;

         Code_Counts : array (db_types.Code_Status_Type) of Natural := (others => 0);
         Codes : codes_db.T_Codes (null);
      begin
         C.Fetch (Connection => Database_Connection,
                  Query      =>
                    GNATCOLL.SQL.SQL_Select (Fields => Codes.Id & Codes.Code & Codes.Valid,
                                             From   => Codes));
         while C.Has_Row loop
            declare
               S : constant db_types.Code_Status_Type := db_types.Value (C.Value (2));
            begin
               Code_Counts (S) := Natural'Succ (Code_Counts (S));
            end;
            C.Next;
         end loop;

         logging.L ("Codes check --" &
                      " Unused:" & Natural'Image (Code_Counts (db_types.Unused_Code)) &
                      " Used:" & Natural'Image (Code_Counts (db_types.Used_Code)) &
                      " Voting:" & Natural'Image (Code_Counts (db_types.Voting_Code)));
      exception
         when E : others =>
            exceptions.Reraise_With_Error (M => "Error in Initial DB Check - " & db_types.Image (C), E => E);
            quitting.protected_quit.Start_Quitting;
      end Initial_Check;

      procedure Build_Candidates is
         Positions_DB  : codes_db.T_Positions (null);
         Nomination_DB : codes_db.T_Candidates_Nominations (null);
         Candidate_DB  : codes_db.T_Candidates (null);
         use type GNATCOLL.SQL.SQL_Field_List;

         Q : GNATCOLL.SQL.SQL_Query;
         Positions_Cursor,
         Nomination_Cursor,
         Candidate_Cusor   : GNATCOLL.SQL.Exec.Forward_Cursor;
      begin
         Candidates := GNATCOLL.JSON.Create_Object;

         Q := GNATCOLL.SQL.SQL_Select (Fields => Positions_DB.Id &
                                         Positions_DB.Fullname,
                                       From   => Positions_DB);
         Positions_Cursor.Fetch (Connection => Database_Connection,
                                 Query      => Q);
         while Positions_Cursor.Has_Row loop
            declare
               Position_JSON : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
            begin
               Q := GNATCOLL.SQL.SQL_Select
                 (Fields => Nomination_DB.Id & Nomination_DB.Candidate,
                  From   => Nomination_DB,
                  Where  => Nomination_DB.Position."=" (Positions_Cursor.Integer_Value (0)));
               Nomination_Cursor.Fetch (Database_Connection, Q);
               while Nomination_Cursor.Has_Row loop
                  declare
                     Candidate_JSON : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
                  begin
                     Q := GNATCOLL.SQL.SQL_Select
                       (Fields => Candidate_DB.Name & Candidate_DB.ShortName,
                        From   => Candidate_DB,
                        Where  => Candidate_DB.Id."=" (Nomination_Cursor.Integer_Value (1)));
                     Candidate_Cusor.Fetch (Database_Connection, Q);
                     if Candidate_Cusor.Has_Row then
                        Candidate_JSON.Set_Field ("Name", Candidate_Cusor.Value (0));
                        Candidate_JSON.Set_Field ("ShortName", Candidate_Cusor.Value (1));
                        Candidate_JSON.Set_Field ("NominationID", Nomination_Cursor.Value (0));
                        Candidate_JSON.Set_Field ("Vote", Integer (0));

                        Position_JSON.Set_Field (Candidate_Cusor.Value (1), Candidate_JSON);
                     else
                        raise Program_Error with "Failed to find Candidate for Nomination";
                     end if;
                  end;
                  Nomination_Cursor.Next;
               end loop;

               Candidates.Set_Field (Positions_Cursor.Value (1), Position_JSON);
            end;
            Positions_Cursor.Next;
         end loop;
      end Build_Candidates;

      procedure Open is
      begin
         Database_Description := GNATCOLL.SQL.Sqlite.Setup (Database => db_types.DB_Filename,
                                                            Errors   => db_types.E_Reporter'Access);
         Database_Connection := Database_Description.all.Build_Connection;
         Initial_Check;
         Build_Candidates;
      end Open;

      procedure Backup is
      begin
         Ada.Directories.Copy_File
           (Source_Name => db_types.DB_Filename,
            Target_Name => "../db_backups/" & db_types.DB_Filename & '.' &
              Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
      end Backup;

      procedure Close is
      begin
         logging.L ("Closing DB");
         Database_Connection.all.Close;
         Database_Description.all.Free;
         Backup;
      end Close;

      function Check_Code (S : String) return Boolean is
         S_Lower : constant String := Ada.Characters.Handling.To_Lower (S);
         use type GNATCOLL.SQL.SQL_Field_List;
         Codes : codes_db.T_Codes (null);
         C : GNATCOLL.SQL.Exec.Forward_Cursor;
         Q : constant GNATCOLL.SQL.SQL_Query := GNATCOLL.SQL.SQL_Select
           (Fields => Codes.Code & Codes.Valid,
            From   => Codes,
            Where  => GNATCOLL.SQL."and"
              (Codes.Code."=" (GNATCOLL.SQL.Expression (S_Lower)),
              (Codes.Valid."=" (db_types.Image (db_types.Unused_Code)))));
         use type Ada.Strings.Unbounded.Unbounded_String;
      begin
         C.Fetch (Connection => Database_Connection,
                  Query      => Q);
         return C.Has_Row;
      end Check_Code;

      procedure Vote (Safe_Votes : GNATCOLL.JSON.JSON_Value; Code : String) is
         Codes_Type : codes_db.T_Codes (null);
         Votes_DB : codes_db.T_Votes (null);
         Q : GNATCOLL.SQL.SQL_Query;
         procedure Iterate_Positions (Position_Name  : GNATCOLL.JSON.UTF8_String;
                                      Position_Value : GNATCOLL.JSON.JSON_Value) is
            pragma Unreferenced (Position_Name);
            procedure Iterate_Candidates (Candidate_Name  : GNATCOLL.JSON.UTF8_String;
                                          Candidate_Value : GNATCOLL.JSON.JSON_Value) is
               pragma Unreferenced (Candidate_Name);
               use type GNATCOLL.SQL.SQL_Assignment;
               NomID            : constant Integer := Integer'Value (Candidate_Value.Get ("NominationID"));
               Vote_Preference  : constant Integer := Candidate_Value.Get ("Vote");
            begin
               if Vote_Preference > 0 then
                  Q := GNATCOLL.SQL.SQL_Insert
                    (Votes_DB.Code."=" (Code) &
                       Votes_DB.Nom_Id."=" (NomID) &
                       Votes_DB.Preference."=" (Vote_Preference));
                  Database_Connection.all.Execute (Q);
               end if;
            end Iterate_Candidates;
         begin
            Position_Value.Map_JSON_Object (Iterate_Candidates'Access);
         end Iterate_Positions;
      begin
         pragma Assert (Database_Connection.all.Start_Transaction);
         Safe_Votes.Map_JSON_Object (Iterate_Positions'Access);
         Q := GNATCOLL.SQL.SQL_Update (Table => Codes_Type,
                                       Set   => Codes_Type.Valid."=" (db_types.Image (db_types.Used_Code)),
                                       Where => Codes_Type.Code."=" (Code));
         Database_Connection.all.Execute (Q);
         Database_Connection.all.Commit_Or_Rollback;
      end Vote;

      function Get_Candidates return GNATCOLL.JSON.JSON_Value is
         Candidates_String : constant String := Candidates.Write;
      begin
         return GNATCOLL.JSON.Read (Candidates_String);
      end Get_Candidates;
   end db_lock;

   procedure Close is
   begin
      db_lock.Close;
   end Close;

   task type Backup_Cycle;
   task body Backup_Cycle is
   begin
      select quitting.protected_quit.Queue_For_Quitting;
      then abort
         db_lock.Backup;
         delay Backup_Cycle_Duration;
      end select;
   end Backup_Cycle;
   Backup_Cycle_Task : access Backup_Cycle; pragma Unreferenced (Backup_Cycle_Task);
begin
   db_lock.Open;
   quitting.protected_quit.Register_Last_Wish (Close'Access);
   Backup_Cycle_Task := new Backup_Cycle;
end db;
