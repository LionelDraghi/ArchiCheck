

separate (Main)

function Is_A_Component_Declaration (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " contains ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Component_Declaration;
