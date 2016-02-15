with Ada.Characters.Handling,
     Ada.Text_IO;
with db,
     quitting;

package body cl is
   function Operator (T : String) return String;
   function Option   (T : String) return String;

   function Operator (T : String) return String is
      S : constant String := Ada.Characters.Handling.To_Lower (T);
   begin
      for i in S'Range loop
         if S (i) = ' ' then return S (S'First .. i - 1); end if;
      end loop;
      return S;
   end Operator;
   function Option (T : String) return String is
      S : constant String := Ada.Characters.Handling.To_Lower (T);
   begin
      for i in S'Range loop
         if S (i) = ' ' then return S (i + 1 .. S'Last); end if;
      end loop;
      return "";
   end Option;

   task type CL_Task_Type;
   task body CL_Task_Type is
   begin
      select quitting.protected_quit.Queue_For_Quitting;
      then abort
         loop
            Ada.Text_IO.Put ("=> ");
            declare
               L : constant String := Ada.Text_IO.Get_Line;
               O : constant String := Operator (L);
            begin
               if O = "checkcode" then
                  case db.db_lock.Check_Code (Option (L)) is
                  when True => Ada.Text_IO.Put_Line ("Valid Code");
                  when False => Ada.Text_IO.Put_Line ("Invalid Code");
                  end case;
               elsif O = "quit" then
                  quitting.protected_quit.Start_Quitting;
                  exit;
               else
                  Ada.Text_IO.Put_Line ("Unrecognised Option: " & O);
               end if;
            end;
         end loop;
      end select;
   end CL_Task_Type;

   CL_Task : access CL_Task_Type; pragma Unreferenced (CL_Task);

   procedure Start_CL is begin
      CL_Task := new CL_Task_Type;
   end Start_CL;
end cl;
