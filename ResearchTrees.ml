type 'a tree=Empty | Node of 'a* 'a tree * 'a tree ;;
type 'a option=None|Some of 'a ;;
(* accès aux types *)
open Asda
(* accès aux fonctions de visualisation sans le préfixe View. *)
open View
(* accès aux générateurs sans le préfixe Rand. *)
open Rand

(*Pour convertir arbre dans list triee *)
let rec btree_to_list t=
  match t with
  |Empty->[]
  |Node(x,left,right)-> (btree_to_list left)@[x]@(btree_to_list right) ;;

let rec list_from_bst t=
    let rec mult c m l=
      if m=0
      then l 
      else mult c (m-1) (c::l)
    in match t with
    |Empty->[]
    |Node((x,m),left,right)-> let list_left_bst=list_from_bst left in
        let list_mult=mult x m [] in
        let list_right_bst=list_from_bst right in
        list_left_bst@list_mult@list_right_bst;;
let rec merge l1 l2=
  match l1,l2 with
  |[],[]->[]
  |[],l2->l2
  |l1,[]->l1
  |head1::tail1,head2::tail2->if head1<=head2 then head1::(merge tail1 l2)
    else head2::(merge l1 tail2) ;;

let rec merge_sort l=
  let size=List.length l in
  match size with
  |0 |1->l
  |_->let m=(size/2)-1 in
    let l1,l2=List.partition (fun x->x<List.nth l m) l in
      merge (merge_sort l1) (merge_sort l2) ;;


      
(*Fonctions *)
let bst_pred t x=
  let rec aux t root=
    match t with
    |Empty->root
    |Node((c,m),left,right)-> if x <=k then aux l root
      else aux right (Some (c,m)) ;;

      
let rec bst_min t=
  match t with
  |Empty->None
  |Node((c,m),Empty,_)->Some c
  |Node(_,l,_)->bst_min l ;;

let rec bst_max t=
  match t with
  |Empty->None
  |Node((c,m),_,Empty)->Some c
  |Node(_,_,r)->bst_max r ;;

let rec plpt t e=
  match t with
  |Empty->True
  |Node((c,_),Empty,Empty)->c<e
  |Node((c,_),left,right)->(plpt left e) && (plpt right e) && c<e ;;

let rec plgd t e=
  match t with
  |Empty->true
  |Node((c,_),Empty,Empty)->c>e
  |Node((c,_),left,right)->(plgd left e) && (plgd right e) && c>e ;;

let rec is_bst t=
  match t with
  |Empty->True
  |Node((c,_),left,right)->(is_bst left) && (is_bst right) && (plpt left c) && (plgd right c) ;;

let is_bstlinear t=
  if t=Empty then true 
  else
    let rec aux t=
      match t with
      |Empty->failwith "should not happen"

      |Node((c,_),Empty,Empty)->true,c,c

      |Node((c,_),left,Empty)->let isbstlinear left,min left ,max left=aux left
          in isbstlinear left && (max left<c),min left,c

      |Node((c,_),Empty,right)->let isbstlinear right, min right, max right=aux right
          in isbstlinear right && (c<min right),c, max right
          
      |Node((c,_),left,right)->let isbstlinear left, min left, max left=aux left in
          let isbstlinear right, min right, max right=aux right in
            isbstlinear left && isbstlinear right && (max left<c) && (c<min right),min left,max right
      in let x,y,z=aux t in x ;;

let rec bst_bigger t=
  match t with
  |Empty->0
  |Node(x,left,right)->let bigleft=bst_bigger left in
      let bigright=bst_bigger right in
      max bigleft bigright ;;


let rec bst_pop_max t=
  match t with
  |Empty->failwith "Pop max ne marche pas avec un arbre de recherche vide"
  |Node(x,left,right)->if right!=Empty then Node(x,left,fst(bst_pop_max right)),x
      else left,x  ;;



let rec bst_remove x t=
  match t with
  |empty->t
  |Node((c,m),left,right)->
    if x=c then
        if m>1 then Node((c,(m-1)),left,right)
        else if left=Empty then right
        else let l_without_max, max=bst_pop_max left
          in Node(max,l_without_max,right)
    else if x<c then Node((c,m),bst_remove x left,right)
    else Node((c,m),left,bst_remove x right) ;;


let rec bst_search t k=
  match t with
  |Empty->0
  |Node((c,m),left,right)->if c=k then m
    else if c<k then bst_search right k
    else bst_search left k ;; 


let rec bst_succ t k=
  let rec aux bstlist=
    match bstlist with
    |[]->failwith "Empty tree"
    |head::tail->if k<=head then (List.hd tail)
      else aux tail

  in merge_sort(btree_to_list(t)) ;;


  let rec bst_insert t x=
    match t with
    |Empty->Node((x,1),Empty,Empty)
    |Node((c,m),l,r)-> if c==x then Node((c,m+1),l,r)
      else if c<x then Node((c,m),l,(bst_insert r x))
      else Node((c,m),(bst_insert l x),r) ;;

  let bst_from_list l=
  let rec aux l cpt=
    match l with
    |[]->cpt
    |head::tail->aux tail (bst_insert cpt head)
  in aux l Empty ;;
