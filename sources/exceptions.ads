with Ada.Exceptions;

package exceptions is
   pragma Elaborate_Body;

   function Error_Message (M : String;
                           E : Ada.Exceptions.Exception_Occurrence) return String;
   procedure Reraise_With_Error (M : String;
                                 E : Ada.Exceptions.Exception_Occurrence);
end exceptions;
