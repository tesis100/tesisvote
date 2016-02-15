with server;
with cl;

procedure voting is

begin
   server.Start;
   cl.Start_CL;
end voting;
