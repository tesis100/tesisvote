with db;
with Ada.Containers.Vectors;

package body client_handler is
   package Positive_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Positive);
   package Positive_Vector_Sort is new Positive_Vector.Generic_Sorting;

   function Verify_Votes (M : GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value is
      use type GNATCOLL.JSON.JSON_Value;
      Positions_In : constant GNATCOLL.JSON.JSON_Value := M.Get ("Candidates");
      Template     : constant GNATCOLL.JSON.JSON_Value := db.db_lock.Get_Candidates;
      procedure Iterate_Positions (Position_Name : GNATCOLL.JSON.UTF8_String;
                                   Position_Value : GNATCOLL.JSON.JSON_Value) is
         Candidates_In : constant GNATCOLL.JSON.JSON_Value := Positions_In.Get (Position_Name);
         Position_Votes : Positive_Vector.Vector := Positive_Vector.Empty_Vector;

         procedure Iterate_Candidates (Candidate_Name : GNATCOLL.JSON.UTF8_String;
                                       Candidate_Value : GNATCOLL.JSON.JSON_Value) is
            Candidate_In          : constant GNATCOLL.JSON.JSON_Value := Candidates_In.Get (Candidate_Name);
            Template_Name         : constant String := Candidate_Value.Get ("Name");
            Template_Shortname    : constant String := Candidate_Value.Get ("ShortName");
            Template_NominationID : constant String := Candidate_Value.Get ("NominationID");
            Vote                  : constant Natural := Candidate_In.Get ("Vote");
         begin
            pragma Assert (Candidate_In.Get ("Name") = Template_Name);
            pragma Assert (Candidate_In.Get ("ShortName") = Template_Shortname);
            pragma Assert (Candidate_In.Get ("NominationID") = Template_NominationID);
            if Vote > 0 then
               Candidate_Value.Set_Field ("Vote", Vote);
               Position_Votes.Append (Candidate_In.Get ("Vote"));
            end if;
         end Iterate_Candidates;
      begin
         Position_Value.Map_JSON_Object (Iterate_Candidates'Access);
         Positive_Vector_Sort.Sort (Position_Votes);
         for i in Positive'First .. Natural (Position_Votes.Length) loop
            pragma Assert (i = Position_Votes.Element (i));
         end loop;
      end Iterate_Positions;
   begin
      Template.Map_JSON_Object (Iterate_Positions'Access);
      return Template;
   end Verify_Votes;

   function Client_Message (M_In : GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value is
      M_Out  : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
      M_Type : constant String                   := M_In.Get ("Type");
   begin
      if M_Type = "Verify" then
         M_Out.Set_Field ("Type", "Verify");
         declare
            Validity : constant Boolean := db.db_lock.Check_Code (M_In.Get ("Code"));
         begin
            M_Out.Set_Field ("Valid", Validity);
            if not Validity then M_Out.Set_Field ("Close", True); end if;
         end;
      elsif M_Type = "InfoRequest" then
         M_Out.Set_Field ("Type", "Candidates");
         M_Out.Set_Field ("Candidates", db.db_lock.Get_Candidates);
      elsif M_Type = "Submit" then
         if not db.db_lock.Check_Code (M_In.Get ("Code")) then
            M_Out.Set_Field ("Worked", False);
            M_Out.Set_Field ("Invalid Code", False);
            M_Out.Set_Field ("Close", True);
            return M_Out;
         end if;

         db.db_lock.Vote (Verify_Votes (M_In), M_In.Get ("Code"));
         M_Out.Set_Field ("Worked", True);
         M_Out.Set_Field ("Close", True);
      else
         raise Program_Error with "Unknown M_Type: " & M_Type;
      end if;
      return M_Out;
   end Client_Message;

end client_handler;
