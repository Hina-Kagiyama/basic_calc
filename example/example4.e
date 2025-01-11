relu := x ->
    ?(
        0 < x < 5 => x,
        _ => 0
    );
relu 5
relu 4
relu 3
relu 2
relu 1
relu 0
relu (-1)
