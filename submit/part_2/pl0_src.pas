var  x, y, q, r;
procedure  divide;
var  w;
begin  
    r := x;  
    q := 0;  
    w := y;
    while w <= r do w := 2 * w;
    while w > y do
    begin  
        q := 2 * q;  
        w := w / 2;
        if w <= r then
        begin  
            r := r - w;  
            q := q + 1 
        end
    end
end;
begin 
  read(x); read(y);
  call divide;
  write(q);
  write(r);
end.
