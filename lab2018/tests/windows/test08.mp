program test08;
var x : boolean;
    y : boolean;
begin
   x := not false and true;
   y := false;
   y := not y and x; 
   x := y or x 
end.
