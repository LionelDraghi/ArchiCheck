with Rational_IO;
-- with Enum_IO;
with New_Page; -- separate procedure that is a rename
with Util.New_Page;

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
   task Server is
      entry Shut_Down;
   end Server;
   task body Server is separate;
     
   -- separate protected
   protected Ressource is
      entry Seize;
      procedure Release;
   private
      Buzy : Boolean := False;
   end Ressource;
   protected body Ressource is separate;

begin
   New_Page;

end Sub.Test;
