with A6;

separate (Sub.Test)

protected body Ressource is

   entry Seize when not Buzy is
   begin
      Buzy := True;
   end Seize;

   procedure Release is
   begin
      Buzy := False;
   end Release;

end Ressource;

