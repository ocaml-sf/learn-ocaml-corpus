let rec triangles n = 
  if n < 3 && n >= 0
  then 1
  else (triangles (n-2) + triangles (n-3))