with A7;

separate (Sub.Test)

task body Server is
begin
   select
      accept Shut_Down;
   or delay (1.0);
   end select;

end Server;
