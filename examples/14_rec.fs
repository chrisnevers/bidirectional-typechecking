rec factorial x =
  if x = 1
    1
  else
    x * factorial $ x - 1

factorial 10
