with Ada.Containers.Vectors,
     Ada.Interrupts.Names;

package quitting is
   type Last_Wish_Procedure is access procedure;
   package Last_Wish_Vector_Type is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Last_Wish_Procedure);

   protected protected_quit is
      procedure Start_Quitting;
      pragma Interrupt_Handler (Start_Quitting);
      pragma Unreserve_All_Interrupts;
      pragma Attach_Handler (Start_Quitting, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler (Start_Quitting, Ada.Interrupts.Names.SIGTERM);

      procedure Register_Last_Wish (P : Last_Wish_Procedure);
      entry Queue_For_Quitting;
   private
      Last_Wish_Vector : Last_Wish_Vector_Type.Vector := Last_Wish_Vector_Type.Empty_Vector;
      Is_Quitting : Boolean := False;
   end protected_quit;
end quitting;
