a := 1 + 1,
b := x -> x + 1,
c := x -> tmp := x ^ 2; x + tmp,
print (a + 2),
print (b a),

add2 := x -> y -> x + y,
print (add2 1.2 2.3)
