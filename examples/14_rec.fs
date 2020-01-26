rec factorial x =
  if eq x 1
    1
  else
    mul x (factorial (sub x 1))

factorial 10
