program test14;
var x : integer;
    f : integer;
begin
  readln(x);
  if x < 0 then
  begin
    x := - x
  end    
  else
  begin
    x := x
  end;
  f := 1;  
  while 0 < x do
  begin
    f := f * x;
    x := x - 1
  end;
  writeln(f)  
end.
