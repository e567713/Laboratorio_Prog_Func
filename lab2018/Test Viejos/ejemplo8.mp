program ejemplo8;
var x : integer;
begin
   readln(x);
   x := 2 + x * 0;
   x := (3 + 2) * x;
   x := x + (0 div 3);
   if  (x = 38) and False then
   begin
      writeln(x)
   end
   else
   begin
      writeln(x+1)      	 
   end;
   while ((2 = 3) or False) do
   begin
      writeln(x)      	 
   end 
end.
