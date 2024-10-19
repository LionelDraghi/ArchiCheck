
separate (Main)

function Is_A_Dependency_Rule (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " depends ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Dependency_Rule;
