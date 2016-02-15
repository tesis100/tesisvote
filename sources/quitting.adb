with exceptions,
     logging;

package body quitting is
   protected body protected_quit is
      procedure Start_Quitting is
         procedure Last_Wish_Granter (C : Last_Wish_Vector_Type.Cursor) is
            P : constant Last_Wish_Procedure := Last_Wish_Vector_Type.Element (C);
         begin
            if P /= null then P.all; end if;
         exception when E : others =>
               logging.E (exceptions.Error_Message ("Error in Last Wish", E));
         end Last_Wish_Granter;
      begin
         logging.L ("Shutting Down ...");
         Is_Quitting := True;
         Last_Wish_Vector.Iterate (Process => Last_Wish_Granter'Access);
         logging.L ("Shut Down ...");
      end Start_Quitting;

      procedure Register_Last_Wish (P : Last_Wish_Procedure) is
      begin
         case Is_Quitting is
            when True => P.all;
            when False => Last_Wish_Vector.Append (P);
         end case;
      end Register_Last_Wish;

      entry Queue_For_Quitting when Is_Quitting is
      begin
         null;
      end Queue_For_Quitting;
   end protected_quit;
end quitting;
