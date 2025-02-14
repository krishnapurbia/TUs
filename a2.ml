type vector = float list;;
let rec rev ( co : vector ) ( reversed : vector ) =
  match co  with |
  [] -> reversed |
  jj::kk -> rev ( kk ) ( jj :: reversed );;
 
let reversing ( co : vector ) = rev ( co ) ( [] );;  

  let rec sc ( co : vector ) ( vec : vector ) ( x : float )= 
    match vec with 
    | []->co
    | jj::kk-> sc ( (x*.jj):: co ) ( kk ) ( x );;
    
  exception DimensionError;;
let rec dimension ( vec:vector ) ( tl : int ) = 
  match vec with | [] -> tl |
  _::kk -> dimension ( kk ) ( 1 + tl)  ;;
let dim ( vec : vector ) = dimension ( vec )  (0)   ;;
  

let scale  ( x: float ) (vec : vector ) = let gg  = sc ( [] ) ( vec ) ( x ) 
in let rr = reversing ( gg ) in rr;;

let rec adv ( co : vector ) ( vec : vector ) ( newvec : vector )= 
  match vec with 
  | []->newvec
  | jj::kk-> match co with | [] -> newvec | 
  oo::pp-> adv( pp ) ( kk ) ( (jj +. oo ) :: newvec ) ;; 
  

let addv  (vec : vector ) ( co : vector  ) = 
  let x1 = dim vec in let x2 = dim co in
  if ( (x1 = x2) &&  ( vec != [] ) )  then 
  let ff = 
  adv ( vec ) ( co ) ( [] ) in let gh = reversing( ff ) in gh;
  else raise DimensionError;;

type types =  Bool    
            | Scalar   
            | Vector of int   
;;

type expr =  
T |
 F   
| ConstS of float  
| ConstV of float list    
| Add of expr * expr  
| Inv of expr     
| ScalProd of expr * expr 
| DotProd of expr * expr  
| Mag of expr   
| Angle of expr * expr  
| IsZero of expr 
| Cond of expr * expr * expr 
;;
exception Wrong of expr;;

    let rec type_of e = match e with
    | T | F -> Bool
    | Angle (e1, e2) ->
    let x = type_of e1 in
    let y = type_of e2 in
    if x = Bool || x = Scalar then raise (Wrong e)
    else if x = y then Scalar
    else raise (Wrong e)

| IsZero e ->
    let _ = type_of e in Bool;

| Cond (e0, e1, e2) ->
    let x = type_of e0 and  y = type_of e1 and z = type_of e2 in
    if x = Bool then
      if  y = z then y
      else raise (Wrong e)
    else raise (Wrong e)  
    | ConstS _ -> Scalar
    | ConstV v -> if List.length v < 1 then raise ( Wrong ( e ))  else Vector (List.length v)
  | Add (s, d) ->
      let c = type_of s in
      let a = type_of d in
      if c = a then c else raise (Wrong e)
  | Inv (d) -> type_of d 
  | ScalProd (s, d) ->
      let c = type_of s and a = type_of d in
      if c = Bool && a = Bool then Bool
      else if c = Scalar then 
      if a = Bool  then raise ( Wrong ( e ))
      else a
      else if a = Scalar then if c = Bool then raise ( Wrong ( e )) else c
      else raise ( Wrong ( e ))
    | DotProd (s, d) -> 
    let c = type_of s and a = type_of d in
    if  (c = a) then
      if c = Bool then raise (Wrong e)
      else if c = Scalar then raise (Wrong e)
      else Scalar 
    else raise ( Wrong ( e ))

| Mag xs ->
    let t = type_of xs in
    if ( t = Bool ) then 
    raise (Wrong e) else Scalar


 
      ;;

      
     
      let rec pdv ( co : vector ) ( vec : vector ) ( newvec : float )= 
        match vec with 
        | []->newvec
        | jj::kk-> match co with |
         [] -> newvec | 
        oo::pp-> pdv( pp ) ( kk ) ( (jj *. oo ) +. newvec ) ;; 
      let dot_prod  (vec : vector ) ( co : vector ) = 
        let x1 = dim vec in let x2 = dim co in if ( x1 = x2 ) then 
        pdv ( vec ) ( co ) ( 0.0 )
        else raise DimensionError;;
      
      let rec len ( vec : vector ) ( ff : float ) = 
        match vec with | 
         [] -> ff | 
         jj::kk -> len (kk) ( jj *. jj +. ff );;
    
    let length ( vec : vector ) = 
      match vec with [] -> raise DimensionError | _::_->
      let gg = Float.sqrt( len vec 0.0 )
      in
      gg;;
let angle (vec : vector) (vec1 : vector) =
        let sv = length vec in
        let sv1 = length vec1 in
        let ddp = dot_prod vec vec1 in
        if not ( sv < 0.000001 || sv1 < 0.000001) then
          let final = (ddp /. (sv *. sv1)) in
          if final > 1.0 then 0.0
          else if final < -1.0 then
            Float.pi 
          else
            Float.acos (final)
        else
          raise DimensionError
      ;;

type values = B of bool |  S of float | V of vector;;
let rec invers ( vec : vector ) ( nvec : vector ) = 
  match vec with |
  [] -> nvec|
  jj::kk->
  invers  (  kk ) ( ((-.jj)) :: nvec )     ;;
  let inv ( vec : vector ) = 
  match vec with | [] -> raise DimensionError | _::_ ->
  let x = 
    invers ( vec )  ([]) in let y = reversing ( x ) in y ;;

let md e = if e > 0.0 then e else (-1.0) *. (e) ;;
let rec len ( vec : vector ) ( ff : float ) = 
  match vec with | 
   [] -> ff | 
   jj::kk -> len (kk) ( jj *. jj +. ff );;

let length ( vec : vector ) = 
match vec with [] -> raise DimensionError | _::_->
let gg = Float.sqrt( len vec 0.0 )
in
gg;;
  let rec chk e acc  = match e with 
  | [] -> acc
  | s ::h -> let t =  if ( s < 0.000001 ) then true else false in chk h (acc && t )
  ;;
  let checknull e = match e with
  |[] -> raise DimensionError
  | _::_ -> chk e true ;;
  let rec eval e = 
    match e with
    | IsZero s ->
    let _ = type_of e and f = eval s in 
    (match f with | B true -> B false | B false -> B true | S a -> (if md a < 0.000001 then B true  else B false)
    |   V a -> (if ( try checknull (a) with DimensionError -> raise ( Wrong ( e )) ) then B true else B false)
    )
    | Cond (e0, e1, e2) ->
    let _ = type_of e 
    in let c = eval e0  in ( if c = B true then eval e1 else eval e2 ) 
  | ConstS x -> S x 
  | ConstV v -> V v
    | T -> B true
    | F -> B false
    | Add (s, d) ->
    let r = type_of e in 
    let x = eval s and y = eval d in 
    if r = Bool then 
      (match x, y with 
       | B xx, B yy -> if xx = false && yy = false then B false else B true | _ -> raise ( Wrong ( e )) ) 
    else if r = Scalar then  
      (match x, y with 
       | S xx, S yy -> S (xx +. yy) | _ -> raise ( Wrong ( e )) )
    else  
      (match x, y with 
       | V xx, V yy -> V (try addv xx yy with DimensionError -> raise ( Wrong ( e ))) | _ -> raise ( Wrong ( e )))
       | Inv (d) ->
       let r = type_of d in let x = eval d in 
       if r = Scalar then ( match x with S xx ->  S ((-1.0) *. (xx)) | _ -> raise ( Wrong ( e )))
       else if  r = Bool  then  ( match x with B true ->  B false |B false -> B true | _ -> raise ( Wrong ( e )))
       else (match x with V xx -> V (try inv xx with DimensionError -> raise ( Wrong ( e )))| _ -> raise ( Wrong ( e )))
       | ScalProd (s, d) ->
       let r = type_of e in let x = eval s and y = eval d in 
       if r = Scalar then (match (x, y) with | (S xx, S yy) -> S (xx *. yy) | _ -> raise ( Wrong ( e )))  
       else if r = Bool then (match (x, y) with | (B true, B true) -> B true  | (B _, B _) -> B false  | _ -> raise ( Wrong ( e )))  
       else (match (x, y) with | (S xx, V k) -> V (try scale xx k with DimensionError -> raise ( Wrong ( e ))) | 
       ( V k ,S xx) -> V (try scale xx k with DimensionError -> raise ( Wrong ( e ))) | _ -> raise ( Wrong ( e ))
       )   
       | DotProd (s, d) ->
       let _ = type_of e in
       let x = eval s and y  = eval d in (match (x , y) with (V xx , V yy) ->  S (try dot_prod xx yy with DimensionError -> raise ( Wrong ( e ))) | _ -> raise ( Wrong ( e )) )
       | Mag x ->
       let r = type_of x in 
          let w = eval x in 
          if r = Scalar then  ( match w with S ff -> S (md ff) | _ -> raise ( Wrong ( e )))
          else ( match w with V cc -> S (try length cc with DimensionError -> raise ( Wrong ( e ))) | _ -> raise ( Wrong ( e )))
           | Angle (e1, e2) ->
        let _ = type_of e in
        let xd = eval e1 in let yd = eval e2 in 
        ( match (xd,yd) with | (V (aa),V (ss)) -> S (try angle aa ss with DimensionError -> raise ( Wrong ( e )) ) | _ -> raise ( Wrong ( e )) )
      

  ;;

