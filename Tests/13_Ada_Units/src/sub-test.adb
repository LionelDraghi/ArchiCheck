with Rational_IO;
-- with Enum_IO;

procedure Sub.Test is

   -- separate procedure
   procedure Put (Text : in String);
   procedure Put (Text : in String) is separate;
   
   -- separate package
   package Tools is
   end Tools;
   -- package Tools is separate;
   
   -- separate function
   function Get (Id : in Natural) return String;
   function Get (Id : in Natural) return String is separate;
   
   -- separate task
   
   -- separate protected
   

begin
   null;

end Sub.Test;
