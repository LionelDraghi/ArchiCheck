








separate (Main)

function Is_A_Comment (Line : in String) return Boolean is
begin
   return Head (Line, Count => 2) = "--";
end Is_A_Comment;
