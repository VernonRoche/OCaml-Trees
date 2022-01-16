type htree = Leaf of char | Node of htree * htree

let string_to_charlist s =
  let rec aux i acc =
    let acc= s.[i]::acc
    in if i = 0 then acc
       else aux (i-1) acc
  in aux ((String.length s) - 1) [];;



let rec insert_sorted x l key_fun=
  match l with
  |[]->[x]
  |head::tail-> if ((key_fun x) <= (key_fun head)) then
        x::l
        else head::(insert_sorted x tail key_fun);;

let rec insertion_sort l key_fun =
  match l with
  [] -> []
  |h::tail ->  insert_sorted h (insertion_sort tail key_fun) key_fun;;


let rec add_char char l =
    match l with
    |[] -> [(char,1)]
    |(x, mult) :: tail ->
        if x = char then (x, (mult+1)) :: tail
        else if char < x then (char,1) :: l
        else (x,mult) :: (add_char char tail);;


let charlist_to_freqlist charlist=
  let rec aux charlist freqlist=
    match charlist with
    |[]->freqlist
    |head::tail->aux tail (add_char head freqlist)
  in insertion_sort (aux charlist []) snd;; 


let charlist_to_treelist charlist =
   List.map (fun (char, f) -> (Leaf(char), f)) (charlist_to_freqlist charlist);;


let rec treelist_to_hufftree treelist=
    match treelist with
    |[] -> failwith "should not be empty"
    | [(t, _)] -> t
    | (a0, f0)::(a1, f1)::rest ->
       treelist_to_hufftree (insert_sorted (Node(a0, a1), f0 + f1) rest snd)  ;;

let codelist_test hufftree=
  let rec aux hufftree code acc=
    match hufftree with
    |Leaf(x)->(x,acc)::code
    |Node(left,right)->(aux left code (0::acc))::(aux right code (1::acc))::code
  in aux hufftree [] [];;


let rec add_bit_left b char_codes=
  List.map (function (c,w)->(c,b::w)) char_codes;;

let rec codelist hufftree=
  match hufftree with
  |Leaf(c)->[(c,[])]
  |Node(left,right)->(add_bit_left 0 (codelist left))@ (add_bit_left 1 (codelist right));;


let rec chars_encode charlist dico =
    match charlist with
    | [] -> []
    | head::rest -> (codelist head dico) @ chars_encode rest dico;;

let encode charlist =
      let treelist= charlist_to_treelist charlist in
      let hufftree= treelist_to_hufftree treelist in
      let dico = codelist hufftree in
      chars_encode charlist dico;;

let rec nextchar_and_tail hufftree l =
        match hufftree, l with
          Leaf x, _ -> x, l
        | Node(a0, a1), b::l' -> nextchar_and_tail (if (b = 1) then a1 else a0) l'
        | _ -> failwith "no element";;

let rec decode hufftree l =
          match l with
          |  [] -> []
          | _ -> let c, l' = nextchar_and_tail hufftree l in
                 c :: (decode hufftree l') ;;

let charlist_to_string charlist =
                  let rec aux l acc=
                    match l with
                    |[] -> acc
                    |head::tail -> aux tail (acc^(String.make 1 head))
                  in aux charlist "";; 
                  
                  
let test_final=
charlist_to_string (decode (treelist_to_hufftree (charlist_to_treelist tentant_charlist)) (encode tentant_charlist));;
                