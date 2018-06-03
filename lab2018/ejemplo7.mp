program ejemplo7;
var b : boolean;
    x : integer;
begin
   readln(b);
   readln(x);
   
   if b and x then
   begin
      writeln(x)
   end
   else
   begin
      while x * 4 do
      begin
         writeln(b)
      end	 
   end;
   x := (x + true) and (b or 8)  
end.
