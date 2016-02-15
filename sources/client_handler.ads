with GNATCOLL.JSON;

package client_handler is
   function Client_Message (M_In : GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value;
end client_handler;
