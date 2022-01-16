type 'a rb=Leaf | Red of 'a * 'a rb* 'a rb | Black of 'a * 'a rb * 'a rb

let rec plpt t e=
  match t with
  |Leaf->true
  |Red((c,_),Leaf,Leaf)->c<e
  |Red((c,_),left,right)->(plpt left e) && (plpt right e) && c<e
  |Black((c,_),Leaf,Leaf)->c<e
  |Black((c,_),left,right)->(plpt left e) && (plpt right e) && c<e ;;

let rec plgd t e=
  match t with
  |Leaf->true
  |Red((c,_),Leaf,Leaf)->c>e
  |Red((c,_),left,right)->(plgd left e) && (plgd right e) && c>e
  |Black((c,_),Leaf,Leaf)->c>e
  |Black((c,_),left,right)->(plgd left e) && (plgd right e) && c>e ;;

let rec is_bst t=
  match t with
  |Leaf->true
  |Red((c,_),left,right)->(is_bst left) && (is_bst right) && (plpt left c) && (plgd right c) 
  |Black((c,_),left,right)->(is_bst left) && (is_bst right) && (plpt left c) && (plgd right c) ;;


let is_Black t=
  match t with
  |Leaf->true
  |Red(_,_,_)->false
  |Black(_,_,_)->true ;;

let no_red_red t=
  match t with
  |Leaf->true
  |Black(_,_,_)->true
  |Red(_,Red(_,_,_),Red(_,_,_))->false
  |Red(_,Red(_,_,_),_)->false
  |Red(_,_,Red(_,_,_))->false ;;

let min_max_black_height t=

  let min_black_height t=
    let rec aux t cpt=
      match t with
        |Leaf->cpt
        |Red(_,left,right)->min (aux left cpt) (aux right cpt)
        |Black(_,left,right)->min (aux left (cpt+1)) (aux right (cpt+1))
    in aux t 0
  in
  let max_black_height t=
    let rec aux t cpt=
      match t with
      |Leaf->cpt
      |Red(_,left,right)->max (aux left cpt) (aux right cpt)
      |Black(_,left,right)->max (aux left (cpt+1)) (aux right (cpt+1))
    in aux t 0
  in
  ((min_black_height t),(max_black_height t)) ;;


let is_rb_classe t=
  let hn_min,hn_max=min_max_black_height t in
    is_bst t && is_Black t && no_red_red t && hn_min==hn_max ;;