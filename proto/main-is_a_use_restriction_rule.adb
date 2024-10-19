

separate (Main)

function Is_A_Use_Restriction_Rule (Line : in String) return Boolean is
begin
   return Index (Source  => Line,
                 Pattern => " may_use ",
                 Going   => Forward,
                 Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map) /= 0;
end Is_A_Use_Restriction_Rule;
