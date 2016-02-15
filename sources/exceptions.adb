with Ada.Task_Identification,
     Ada.Task_Termination;
with GNAT.Traceback.Symbolic;
with logging;

package body exceptions is
   function Error_Message (M : String;
                           E : Ada.Exceptions.Exception_Occurrence) return String is
   begin
      return M & ASCII.LF & " -> " &
        Ada.Exceptions.Exception_Name (E) & " - " & Ada.Exceptions.Exception_Message (E) & ASCII.LF &
        " -> " & GNAT.Traceback.Symbolic.Symbolic_Traceback (E);
   end Error_Message;

   procedure Reraise_With_Error (M : String;
                                 E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Exceptions.Raise_Exception
        (E       => Ada.Exceptions.Exception_Identity (E),
         Message => Error_Message (M, E));
   end Reraise_With_Error;

   protected Protected_Task_Handler is
      procedure Handler (Cause : Ada.Task_Termination.Cause_Of_Termination;
                         T     : Ada.Task_Identification.Task_Id;
                         X     : Ada.Exceptions.Exception_Occurrence);
   end Protected_Task_Handler;
   protected body Protected_Task_Handler is
      procedure Handler (Cause : Ada.Task_Termination.Cause_Of_Termination;
                         T     : Ada.Task_Identification.Task_Id;
                         X     : Ada.Exceptions.Exception_Occurrence) is
         use type Ada.Task_Termination.Cause_Of_Termination;
      begin
         if Cause = Ada.Task_Termination.Abnormal then
            logging.E ("Abnormal Task Termination: " & Ada.Task_Identification.Image (T) & ASCII.LF &
                         " -> " & Ada.Exceptions.Exception_Name (X) & " - " & Ada.Exceptions.Exception_Message (X));
         end if;
      end Handler;
   end Protected_Task_Handler;

begin
   Ada.Task_Termination.Set_Specific_Handler (T       => Ada.Task_Identification.Current_Task,
                                              Handler => Protected_Task_Handler.Handler'Access);
end exceptions;
