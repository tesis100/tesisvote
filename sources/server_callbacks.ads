with AWS.Response,
     AWS.Status;

package server_callbacks is
   function Service_HTTP (Request : AWS.Status.Data) return AWS.Response.Data;
end server_callbacks;
