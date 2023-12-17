
let rec kfact(x)(k) =
  if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(1)
  

let rec kfact2(x)(k) =
  if x > 0 then kfact(x-1)(fun res -> k(x * res)) else k(k(1))

let result =  kfact2(3)(fun res -> res)