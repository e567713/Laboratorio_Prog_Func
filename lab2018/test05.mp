program test05;
var x : integer;
    y : boolean;
begin
   readln(x);
   x := x + 76;
   if x = 109 then
   begin
     y := true;
     x := x + 2
   end
   else
   begin
     y := (x < 289) or (x = 300);
     x := x + 1
   end;
   writeln(x)
end.
