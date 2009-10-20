with Verify_Aggregate.Parser; use Verify_Aggregate.Parser;
procedure Debug_Parser
is
begin
   Dump_Grammar;
   Dump_Parse_Table;
   Set_Trace (True);
   Execute_Command ("set (2 =>;");
end Debug_Parser;
