with Ada.Strings.Fixed,
     Ada.Text_IO,
     Ada.Containers.Vectors;
with GNATCOLL.SQL,
     GNATCOLL.SQL.Exec,
     GNATCOLL.SQL.Sqlite;
with GNAT.Formatted_String;
with codes_db,
     db_types;

use type GNATCOLL.SQL.SQL_Field_List,
    GNATCOLL.SQL.SQL_Criteria,
    GNAT.Formatted_String.Formatted_String;

procedure export_blt is
   package Integer_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                         Element_Type => Positive);
   function Image (I : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Source => Integer'Image (I),
                                     Side   => Ada.Strings.Both);
   end Image;

   Database_Description : GNATCOLL.SQL.Exec.Database_Description;
   Database_Connection  : GNATCOLL.SQL.Exec.Database_Connection;

   Positions_Cursor     : GNATCOLL.SQL.Exec.Forward_Cursor;
   Positions_Type       : codes_db.T_Positions (null);
   Codes_Cursor         : GNATCOLL.SQL.Exec.Forward_Cursor;
   Codes_Type           : codes_db.T_Codes (null);
   Nominations_Cursor   : GNATCOLL.SQL.Exec.Forward_Cursor;
   Nominations_Type     : codes_db.T_Candidates_Nominations (null);
   Votes_Cursor         : GNATCOLL.SQL.Exec.Forward_Cursor;
   Votes_Type           : codes_db.T_Votes (null);
   Candidate_Cursor     : GNATCOLL.SQL.Exec.Forward_Cursor;
   Candidate_Type       : codes_db.T_Candidates (null);
begin
   Database_Description := GNATCOLL.SQL.Sqlite.Setup (Database => db_types.DB_Filename,
                                                      Errors   => db_types.E_Reporter'Access);
   Database_Connection := Database_Description.all.Build_Connection;

   Positions_Cursor.Fetch
     (Database_Connection,
      GNATCOLL.SQL.SQL_Select (Fields => Positions_Type.Id & Positions_Type.Pos_Name & Positions_Type.Aval_Pos,
                               From   => Positions_Type));

   while Positions_Cursor.Has_Row loop
      declare
         Position_BLT         : Ada.Text_IO.File_Type;
         Position_ID          : constant Natural := Positions_Cursor.Integer_Value (0);
         Position_Name        : constant String  := Positions_Cursor.Value (1);
         Position_Vacancies   : constant Natural := Positions_Cursor.Integer_Value (2);
         Position_Nominations :          Natural;
         Nomination_IDs       : Integer_Vector.Vector := Integer_Vector.Empty_Vector;
      begin
         Nominations_Cursor.Fetch
           (Database_Connection, GNATCOLL.SQL.SQL_Select (Fields => Nominations_Type.Id,
                                                          From   => Nominations_Type,
                                                          Where  => Nominations_Type.Position."=" (Position_ID)));
         while Nominations_Cursor.Has_Row loop
            Nomination_IDs.Append (Nominations_Cursor.Integer_Value (0));
            Nominations_Cursor.Next;
         end loop;
         Position_Nominations := Nominations_Cursor.Processed_Rows;

         Ada.Text_IO.Create (File => Position_BLT,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "../votes/" & Position_Name & ".blt");

         Ada.Text_IO.Put_Line (Position_BLT, -(+"%d %d" & Position_Nominations & Position_Vacancies));

         Codes_Cursor.Fetch
           (Database_Connection,
            GNATCOLL.SQL.SQL_Select (Fields => Codes_Type.Code,
                                     From   => Codes_Type,
                                     Where  => Codes_Type.Valid."=" (db_types.Image (db_types.Used_Code))));

         while Codes_Cursor.Has_Row loop
            Votes_Cursor.Fetch
              (Database_Connection,
               GNATCOLL.SQL.SQL_Select
                 (Fields => Votes_Type.Nom_Id,
                  From   => Votes_Type,
                  Where  => GNATCOLL.SQL."and" (Votes_Type.Code."=" (Codes_Cursor.Value (0)), GNATCOLL.SQL.SQL_In
                    (Votes_Type.Nom_Id,
                         GNATCOLL.SQL.SQL_Select
                           (Fields => Nominations_Type.Id,
                            From   => Nominations_Type,
                            Where  => Nominations_Type.Position."=" (Position_ID)))),
                  Order_By => GNATCOLL.SQL.Asc (Votes_Type.Preference)));

            Ada.Text_IO.Put (Position_BLT, "1 ");
            while Votes_Cursor.Has_Row loop
               Ada.Text_IO.Put (Position_BLT,
                                Image (Nomination_IDs.Find_Index (Votes_Cursor.Integer_Value (0))) & ' ');
               Votes_Cursor.Next;
            end loop;
            Ada.Text_IO.Put (Position_BLT, '0');
            Ada.Text_IO.New_Line (Position_BLT);
            Codes_Cursor.Next;
         end loop;

         Ada.Text_IO.Put (Position_BLT, '0');
         Ada.Text_IO.New_Line (Position_BLT);

         for Candidate_Nomination of Nomination_IDs loop
            Candidate_Cursor.Fetch
              (Database_Connection,
               GNATCOLL.SQL.SQL_Select
                 (Fields => Candidate_Type.Shortname,
                  From   => Candidate_Type,
                  Where  => GNATCOLL.SQL.SQL_In
                    (Candidate_Type.Id,
                     GNATCOLL.SQL.SQL_Select (Fields => Nominations_Type.Candidate,
                                              From   => Nominations_Type,
                                              Where  => Nominations_Type.Id."=" (Candidate_Nomination)))));
            Ada.Text_IO.Put_Line (Position_BLT, '"' & Candidate_Cursor.Value (0) & '"');
         end loop;

         Ada.Text_IO.Put (Position_BLT, '"' & Position_Name & '"');
         Ada.Text_IO.Close (Position_BLT); pragma Unreferenced (Position_BLT);
      end;
      Positions_Cursor.Next;
   end loop;
end export_blt;
