program test15;
var x : integer;
    f : integer;
begin
  f := 0;  
  readln(x);
  while not (x = 0) do
  begin
    f := f + x;
    readln(x)
  end;
  writeln(f)  
end.
