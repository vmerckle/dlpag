let range i j =
  let rec aux accu n =
    if n < i then accu
    else aux (n :: accu) (n - 1) in
  aux [] (j - 1)

let sum l = List.fold_left (+) 0 l
let product l = List.fold_left ( * ) 1 l
