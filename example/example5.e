% example: recurrsive function
fbnc := x -> ?(
    x = 0 => 1,
    x = 1 => 1,
    _ => fbnc(x-1) + fbnc(x-2)
);
loop := n -> ?(
    n = 0 => _,
    n > 0 =>
        print(fbnc(20-n));
        loop(n-1)
);
% semicolon at end of lines suppresses output.
loop 20;

