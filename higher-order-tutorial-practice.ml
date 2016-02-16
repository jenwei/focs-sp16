List.fold_right(fun h acc -> if (h=n) then acc+1 else acc) xs 0;;
