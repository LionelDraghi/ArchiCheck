

separate (Main)

function Is_Empty (Line : in String) return Boolean is
begin
   return Index_Non_Blank (Line) = 0;
end Is_Empty;
