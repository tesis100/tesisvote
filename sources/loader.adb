with Ada.Strings.Bounded,
     Ada.Text_IO;
with GNATCOLL.SQL.Exec,
     GNATCOLL.SQL.Sqlite;
with codes_db,
     db_types,
     exceptions;

procedure loader is
   package Bounded_300 is new Ada.Strings.Bounded.Generic_Bounded_Length (300);
   type Bounded_Array is array (Positive range <>) of Bounded_300.Bounded_String;
   Null_Bounded_Array : constant Bounded_Array (2 .. 1) := (others => Bounded_300.Null_Bounded_String);
   function To_B (S : String) return Bounded_300.Bounded_String is (Bounded_300.To_Bounded_String (S));
   function To_S (S : Bounded_300.Bounded_String) return String is (Bounded_300.To_String (S));
   function Split (S : String; D : Character) return Bounded_Array is
   begin
      for i in S'Range loop
         if S (i) = D then
            return To_B (S (S'First .. i - 1)) & Split (S (i + 1 .. S'Last), D);
         end if;
      end loop;
      return To_B (S) & Null_Bounded_Array;
   end Split;

   Database_Description : GNATCOLL.SQL.Exec.Database_Description;
   Database_Connection : GNATCOLL.SQL.Exec.Database_Connection;

   Codes : codes_db.T_Codes (null);
   Candidates : codes_db.T_Candidates (null);
   Positions : codes_db.T_Positions (null);
   Nominations : codes_db.T_Candidates_Nominations (null);
   use type GNATCOLL.SQL.SQL_Assignment;
begin
   Database_Description := GNATCOLL.SQL.Sqlite.Setup (Database => "codes.db",
                                                      Errors   => db_types.E_Reporter'Access);
   Database_Connection := Database_Description.all.Build_Connection;
   pragma Assert (Database_Connection.all.Check_Connection, "Database Connected");

   pragma Assert (Database_Connection.all.Start_Transaction, "Starting Transaction");

   Ada.Text_IO.Put_Line ("Loading Positions...");
   declare
      F : Ada.Text_IO.File_Type;
      Q : GNATCOLL.SQL.SQL_Query;
   begin
      Ada.Text_IO.Open (File => F,
                        Mode => Ada.Text_IO.In_File,
                        Name => "../data/positions");
      while not Ada.Text_IO.End_Of_File (F) loop
         declare
            Line_S : constant String := Ada.Text_IO.Get_Line (F);
            Line : constant Bounded_Array := Split (Line_S, ',');
         begin
            case Line'Length is
               when 2 =>
                  Q := GNATCOLL.SQL.SQL_Insert
                    (Positions.Pos_Name."=" (To_S (Line (1))) &
                       Positions.Fullname."=" (To_S (Line (2))) &
                       Positions.Aval_Pos."=" (1)
                    );
               when 3 =>
                  Q := GNATCOLL.SQL.SQL_Insert
                    (Positions.Pos_Name."=" (To_S (Line (1))) &
                       Positions.Fullname."=" (To_S (Line (2))) &
                       Positions.Aval_Pos."=" (Integer'Value (To_S (Line (3))))
                    );
               when others => raise Program_Error with "Expected 2 or 3 arguments for Position: " & Line_S;
            end case;

            Database_Connection.all.Execute (Q);
         end;
      end loop;
      Ada.Text_IO.Close (F); pragma Unreferenced (F);
   end;

   Ada.Text_IO.Put_Line ("Loading Candidates...");
   declare
      C : Positive := 1;
      NC : Positive := 1;
      F : Ada.Text_IO.File_Type;
      Q : GNATCOLL.SQL.SQL_Query;
   begin
      Ada.Text_IO.Open (File => F,
                        Mode => Ada.Text_IO.In_File,
                        Name => "../data/candidates");
      while not Ada.Text_IO.End_Of_File (F) loop
         declare
            Line_S : constant String := Ada.Text_IO.Get_Line (F);
            Line : constant Bounded_Array := Split (Line_S, ',');
         begin
            pragma Assert (Line'Length = 3, "Incorrect Nm of Fields");
            Q := GNATCOLL.SQL.SQL_Insert
              (Candidates.Id."=" (C) &
                 Candidates.Name."=" (To_S (Line (1))) &
                 Candidates.Shortname."=" (To_S (Line (2)))
               );
            Database_Connection.all.Execute (Q);

            declare
               Nom_Positions : constant Bounded_Array := Split (To_S (Line (3)), ';');
               Cursor : GNATCOLL.SQL.Exec.Forward_Cursor;
            begin
               for i of Nom_Positions loop
                  Q := GNATCOLL.SQL.SQL_Select (Fields => Positions.Id,
                                                From   => Positions,
                                                Where  => Positions.Pos_Name."=" (To_S (i)));
                  Cursor.Fetch (Connection => Database_Connection,
                                Query      => Q);
                  pragma Assert (Cursor.Has_Row, "Couldn't find position " & To_S (i) & " for " &
                                   To_S (Line (1)) & " searching " & To_S (Line (2)));

                  Q := GNATCOLL.SQL.SQL_Insert
                    (Nominations.Id."=" (NC) &
                       Nominations.Candidate."=" (C) &
                       Nominations.Position."=" (Cursor.Integer_Value (0)));
                  Database_Connection.all.Execute (Q);
                  NC := Positive'Succ (NC);
               end loop;
            end;

            C := Positive'Succ (C);
         exception when E : others =>
               exceptions.Reraise_With_Error (M => "Loader failed with: " & Line_S,
                                              E => E);
         end;
      end loop;
      Ada.Text_IO.Close (F); pragma Unreferenced (F);
   end;

   Ada.Text_IO.Put_Line ("Loading Voting Codes...");
   declare
      F : Ada.Text_IO.File_Type;
      C : Positive := 1;
      Q : GNATCOLL.SQL.SQL_Query;
   begin
      Ada.Text_IO.Open (File => F,
                        Mode => Ada.Text_IO.In_File,
                        Name => "../data/codes");
      while not Ada.Text_IO.End_Of_File (F) loop
         Q := GNATCOLL.SQL.SQL_Insert
           (Codes.Id."=" (C) &
              Codes.Code."=" (Ada.Text_IO.Get_Line (F)) &
              Codes.Valid."=" (db_types.Image (db_types.Unused_Code)));
         Database_Connection.all.Execute (Q);
         C := Positive'Succ (C);
      end loop;
      Ada.Text_IO.Close (F); pragma Unreferenced (F);
   end;

   Database_Connection.all.Commit;
   pragma Assert (Database_Connection.all.Success, "Commit");

   Database_Connection.all.Close;
end loader;
