type vector = float list;;
exception DimensionError

let rec dimension ( vec:vector ) ( tl : int ) = 
  match vec with | [] -> tl |
  jj::kk -> dimension ( kk ) ( 1 + tl)  
let dim ( vec : vector ) = dimension ( vec )  (0)   
  

let rec sc ( co : vector ) ( vec : vector ) ( x : float )= 
  match vec with 
  | []->co
  | jj::kk-> sc ( (x*.jj):: co ) ( kk ) ( x );;
let rec rev ( co : vector ) ( reversed : vector ) =
  match co  with |
  [] -> reversed |
  jj::kk -> rev ( kk ) ( jj :: reversed );;
 
let reversing ( co : vector ) = rev ( co ) ( [] );;  
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
  

 let rec invers ( vec : vector ) ( nvec : vector ) = 
  match vec with |
  [] -> nvec|
  jj::kk->
  invers  (  kk ) ( ((-.jj)) :: nvec )     ;;
  let inv ( vec : vector ) = 
  match vec with | [] -> raise DimensionError | kk::hh ->
  let x = 
    invers ( vec )  ([]) in let y = reversing ( x ) in y ;;
(* 1e9 is killed check with others *)
let rec len ( vec : vector ) ( ff : float ) = 
    match vec with | 
     [] -> ff | 
     jj::kk -> len (kk) ( jj *. jj +. ff );;

let length ( vec : vector ) = 
  match vec with [] -> raise DimensionError | kk::ll->
  let gg = Float.sqrt( len vec 0.0 )
  in
  gg;;

  let angle (vec : vector) (vec1 : vector) =
    let sv = length vec in
    let sv1 = length vec1 in
    let ddp = dot_prod vec vec1 in
    if not ( sv = 0.0 || sv1 = 0.0) then
      let final = (ddp /. (sv *. sv1)) in
      if final > 1.0 then 0.0
      else if final < -1.0 then
        Float.pi 
      else
        Float.acos (final)
    else
      raise Division_by_zero
  ;;
  let rec is_zr (vec : vector)  = 
    match vec with |
    [] -> true |  
    jj :: kk -> 
    if(jj = 0.0 ) then
      is_zr( kk ) 
    else 
      false;;

let is_zero ( vec : vector )  = 
 match vec with | [] -> raise DimensionError
 | _ -> is_zr( vec);;

let rec un ( n : int ) (  j : int ) ( vec : vector ) =
    if( j = 1 ) then 
      un ( n  - 1 ) ( j - 1 ) ( 1.0::vec)
    else if ( n > 0 ) then 
      un ( n - 1 ) ( j - 1) (0.0::vec)
    else 
      vec;;
    
let unit n j = 
if ( j > n ||  j < 1 ) then  
raise DimensionError
else
un n (n + 1 - j) ([]) ;;

let rec cr ( n : int ) ( x: float ) ( vec : vector ) =
    if ( n > 0 ) then cr (n-1) (x) ( x :: vec ) else vec;;

let create n x = 
if ( n < 1 ) then raise DimensionError
else cr n x [];;  


  (****************Test cases******************)

(* testcases for addv *)
let (a : vector) = [1.3];;
let (b : vector) = [0.7];;
let _ = addv a b;;
let (c : vector) = [1.3; 2.2; 24.32];;
let (d : vector) = [43.3; 21.2; 4.32];;
let _ = addv c d;;
let (d : vector) = [43.3; 21.2; 4.32];;
let (c : vector) = [1.3; 2.2; 24.32; 5.6];;
let _ = try addv c d with DimensionError -> [];;
let y = create 1234567 1.8;;
let x = create 1234567 1.8;;
let _ = addv x y;;

(* testcases for inv *)
let gg = create 1 9.999;;
let _ = inv gg;;
let ggg = create 1234567 98765.0;;
let _ = inv ggg;;
let gggg = create 1000 987654321.0;;
let _ = inv gggg;;
let gggggg = unit 100 7;;
let _ = inv gggggg;;
let ggggggg = unit 1234567 7;;
let _ = inv ggggggg;;

(* testcases for length *)
let gg = create 1 9.999;;
let _ = length gg;;
let (a : vector) = [];;
let _ = try length a with DimensionError -> -1.0;;
let ggg = create 1234567 98765.0;;
let _ = length ggg;;
let ggg = create 1000000 1.0;;
let _ = length ggg;;
let gh = create 3 0.0;;
let _ = length gh;;
let rr = create 3 1.414;;
let _ = length rr;;

(* testcases for angle *)
let rr = create 3 1.414;;
let (kl : vector) = [0.0; 0.0; 0.0];;
let _ = try angle kl rr with Division_by_zero -> -1.0;;
let yr = create 1234567 1.5;;
let ry = create 1234567 2.3;;
let _ = angle ry yr;;
let (a : vector) = [3.0; 0.9; 0.5; 0.0; 23.2];;
let (b : vector) = [3.0; 0.9; 0.5; 0.0; 23.2];;
let _ = angle a b;;
let (a : vector) = [100000000.0; 100000000.0; 100000000.0];;
let (b : vector) = [100000000.0 +. 1.0; 100000000.0; 100000000.0];;
let _ = angle a b;;
let b = create 1000000 4.000001;;
let a = create 1000000 4.0;;
let _ = angle a b;;
let a = unit 1234567 4567;;
let b = unit 1234567 7654;;
let _ = angle a b;;
let (b : vector) = [-3.0; 4.0];;
let (a : vector) = [4.0; 3.0];;
let _ = angle a b;;
(* testcases for Create *)
let a = try create 0 0.45 with DimensionError -> [];;
let rr = create 1 2.3;;
let rrr = create 1234567 2.3;;
let rc = create 12345 1.2;;

(* testcases for dim *)
let (ff : vector) = [1.2323];;
let _ = dim ff;;
let aaa = create 1234567 123456789.123123123;;
let _ = dim aaa;;
let onet = create 100 1.234;;
let _ = dim onet;;
let (onet : vector) = [];;
let _ = dim onet;;

(* testcases for is_zero *)
let (ww : vector) = [0.0];;
let _ = is_zero ww;;
let (www : vector) = [0.0; 0.0];;
let _ = is_zero www;;
let wwww = create 1234567 0.0;;
let _ = is_zero wwww;;
let wwwww = try unit 1234567 3 with _ -> [];;
let _ = is_zero wwwww;;
let wwwwww = create 234 2.3344;;
let _ = is_zero wwwww;;
let (empty : vector) = [];;
let _ = try is_zero empty with DimensionError -> false;;

(* Testcases for unit *)
let t = unit 1 1;;
let tt = try unit 4 5 with DimensionError -> [];;
let tt = try unit 4 0 with DimensionError -> [];;
let tt = unit 4 1;;
let tt = unit 4 4;;
let tt = try unit 1234567 10 with DimensionError -> [];;

(* testcases for scale *)
let gg = create 1 9.999;;
let _ = scale 9.999 gg;;
let ggg = create 1234567 98765.0;;
let _ = scale 12345.0 ggg;;
let gggg = create 1000 987654321.0;;
let _ = scale 0.0 gggg;;
let gggggg = unit 100 7;;
let _ = scale 2.3 gggggg;;
let ggggggg = unit 1234567 7;;
let _ = scale 0.00001 ggggggg;;

(**************** Proof of correctnesss ************)


(* ******************************  (Scalar Distribution over vector sums)     b.(u + v) = b.u + b.v

Predicate P(n):

For A  vectors u &  v of dim n, 
b.(u + v) = b.u + b.v holds.

Base case: P(1)

b.(u + v) = scale b (addv [u1] [v1])

(directly using def of add v u )= scale b [u1 + v1]

(using the prop of scale c v )   = [b * (u1 + v1)]

= [b*u1 + b*v1]

(using algebra we can write this )= [bu1] + [bv1]

( then use def of scale ) = scale b [u1] + scale b [v1]

( again , convert from scale )   = b.u + b.v

P(1) ---> true

Inductive step:

Assume P(m) holds --> To prove P(m+1)

b.(u + v) = (using add and scale) scale b (addv [u1, ..., um+1] [v1, ..., vm+1])

(using the def of scale   )  = scale b [u1 + v1, ..., um+1 + vm+1]

(using the def of scale )  = [b*(u1 + v1), ..., b*(um+1 + vm+1)]

= [bu1, ..., bum+1] + [bv1, ..., bvm+1]

= b.u + b.v

P(m) ---> P(m+1) 

Hence, Proved *)


(*********************************(Identity of addition)  v + O = v

Prood by Induction 

predicate P(n) = for all vectors  v 
of dim n Identity of addition holds

now , 

Base case :: P(1)

v + O = add v O  = ( v1 + 0 ) :: add [] [] ;;
and as addd [] [] = [];;;
also , algebraic (Identity of addition) holds 
v + O = ( v1 + 0 ) =  ( v1 )   = v  --- (1)
from ( 1 )  P(1) is true

Inductive Step 
assume P(m) holds true ::
now,

v + O  = add v O  = ( v1 + 0  ) ::  add  ( ( v2 , .... vm+1)) ((Om )) ;;
and as P(m) holds  ;;
also , algebraic (Identity of addition) holds 
v + O = ( (v1 + 0 ) , 
( vm+1 + 0  ) )   = ( v ) --- (1)

from ( 1 )  we can say P(m+1)  when P(m) holds

hence , form induciton Proved (Identity of addition)


 *)



 (* <<<<<<<<<<<<<<<<<<< (Identity scalar) 1.v = v >>>>>>>>>>>>>>>>>>> 

 Proof by Induction

Predicate P(n) : For A  vectors v of dim n,
 the identity  scalar property  holds.


Base case: P(1)

now , 

1.v = scale 1 v = sc [] [v1] 1 = [1 * v1] 
= [v1]
= v

 P(1) is true

Inductive step:                      :::::::--->>>> <<----::::::

Assume P(m) holds -->  To prove P(m+1)

1.v = scale 1 v = sc [] [v1, .., vk+1] 1
= sc [v1] [v2, .., vk+1] 1
since , Pm holds 
= [v1, ..., vk+1]
= v

 P(m) --->>>> P(m+1)
 
Hence ,Proved *)


(* ()********************************(Scalar sum-product distribution) (b + c).v = b.v + c.v

Predicate P(n): For A  vectors v of dim  n , SSPD holds 


Base case: P(1)

(b + c).v = scale (b + c) [v1]
using def of scale c v

= [(b + c) * v1]

= [b*v1 + c*v1]

= [b*v1] + [c*v1] = scale b [v1] + scale c [v1]
= b.v + c.v

P(1) -> true

Inductive step:

Assume P(m) holds --> To prove P(m+1)

(b + c).v = scale (b + c) [v1, ..., vm+1]
using prop of scale c v 


= [(b + c)*v1, ..., (b + c)*vm+1]

= [b*v1 + c*v1, ..., b*vm+1 + c*vm+1]

(accumulating one side ) = [bv1, ..., bvm+1] + [cv1, ..., cvm+1] 

(again applying reverse property of scale )   = scale b [v1, ..., vm+1] + scale c [v1, ..., vm+1]=b.v + c.v

P(m) --->> P(m+1)  *)


(* ----------->>>>>>>>>>>>>>>>>>>(Additive Inverse) v + (-v) = O-----------------


Proof by Induction

Predicate P(n): For all vectors v of dim n,
the additive inverse property v + (-v) = O holds.

Base case: P(1)

Now,

v + (-v) = addv v (inv v) = addv [v1] [-v1]
= [v1 + (-v1)]
= O

P(1) is true

Inductive step: :::::::--->>>> <<----::::::

Assume P(m) holds   -->   To prove P(m+1)

v + (-v) = addv v (inv v)

inv v = [ -v1 ,....-vm+1]

= addv [v1, ..., vm+1] [-v1, ..., -vm+1]

= [v1 + (-v1)] :: addv [v2, ..., vm+1] [-v2, ..., -vm+1]
since ,  P(m) holds 
= O

 P(m) -> P(m+1)

Hence, Proved 

*)

(* *************************(Scalar product combination) b.(c.v) = (b*c).v********************


Predicate P(n): For A vectors v of dim n, SPC holds 


Base case: P(1)

For a vector v of dim 1 :

b.(c.v) = scale b (
    scale c [v1]
    ) 
    = scale b [c * v1 ] ( using the def of scale c v )

= [   b * (c * v1) ]
= [   (b * c) * v1  ] ( using assc of the float values ) 
= scale (b * c) [v1] ( suing prop of flaot values )
= ( b*c ).v

P(1) ---->>>> true

Inductive step:

Assume P(m) holds -->>>  To prove P(m+1)


b.(c.v) = scale b (scale c [v1, ..., vm+1])
using the porperty of scale c v 

= scale b [cv1, ..., cvm+1]

= [b*(cv1), ..., b*(cvm+1)] = [(bc)*v1, ..., (bc)*vm+1]

= scale (bc) [  v1, ..., vm+1  ]

= ( b*c ) . v

 P(m) --->>P(m+1) 
 
Hence, Proved *)



(* *********************************************(Associativity) u + (v + w) = (u + v) + w

Predicate P(n):

For A  vectors u, v, w of dim n

u + (v + w) = (u + v) + w holds.

Base case: P(1)

u + (v + w) = addv [u1] (addv [v1] [w1])

(directly using def of addv) = addv [u1] [v1 + w1]

(using the prop of addv) = [u1 + (v1 + w1)]

= [(u1 + v1) + w1]

(using algebra we can write this) = [u1 + v1] + [w1]

(then use def of addv) = addv (addv [u1] [v1]) [w1]

(again, convert from addv) = (u + v) + w

P(1) ---> true

Inductive step:

Assume P(m) holds --> To prove P(m+1)

u + (v + w) = (using addv) addv [u1, ..., um+1] (addv [v1, ..., vm+1] [w1, ..., wm+1])

(using the def of addv) = addv [u1, ..., um+1] [v1 + w1, ..., vm+1 + wm+1]

(using the def of addv) = [u1 + (v1 + w1), ..., um+1 + (vm+1 + wm+1)]

= [(u1 + v1) + w1, ..., (um+1 + vm+1) + wm+1]

= addv [u1 + v1, ..., um+1 + vm+1] [w1, ..., wm+1]

= addv (addv [u1, ..., um+1] [v1, ..., vm+1]) [w1, ..., wm+1]

= (u + v) + w

P(m) ---> P(m+1)

Hence, Proved *)


(******************************* (Commutativity)  u + v = v + u

Prood by Induction 

predicate P(n) = for all vectors u, v 
of dim n Commutativity holds

now , 
Notations::
(v1 , v2 ,v3 ..) -> signifies the components /
of the particular vector v

Base case :: P(1)

u + v = add u v  = ( u1 + v1 ) ::  add [] [] ;;
and as addd [] [] = [];;;

also , algebraic additioin is  Commutativity
u + v = ( u1 + v1) = ( v1 + u1) --- (1)

v + u = add v u  = ( v1 + u1 ) :: add [] [] ;;
and as addd [] [] = [];;;
also , algebraic additioin is  Commutativity
v + u = ( u1 + v1) = ( v1 + u1) --- (2)
from ( 1 ) ( 2 ) P(1) is true

Inductive Step 
assume P(m) holds true ::
now,
u + v = add u v  = ( u1 + v1 ) ::  add ((u2,....um+1)) ( ( v2 , .... vm+1)) ;;
and as P(m) holds ;;
also , algebraic additioin is  Commutativity
u + v = (( u1 + v1) ,....., ( um+1 + vm+1) )  = ( (v1 + u1) ,
 ( vm+1 + um+1 ) )  --- (1)


v + u = add v u = ( v1 + u1 ) ::  add  ( ( v2 , .... vm+1)) ((u2,....um+1)) ;;
and as P(m) holds ;;
also , algebraic additioin is  Commutativity
v + u = (( u1 + v1) ,....., ( um+1 + vm+1) )  = ( (v1 + u1) , 
( vm+1 + um+1 ) )  --- (1)

from ( 1 ) ( 2 ) we can say P(m+1) when P(m) holds

hence , form induciton Proved Commutativity for vectors
 *)



(* <<<<<--------------(Annihilator scalar) 0.v = O --------->>>>>>>>>


Proof by  induction ::::-><---:::::


Predicate P(n)  For A vectors v of dim n, 0.v = O holds

Base case : P(1)

0.v = scale 0 v = sc [] [v1] 0 = [0 * v1] = O
 P(1) is true

Inductive step:

Assume P(m) holds -->  To prove P(m+1).

0.v = scale 0 v = sc [] [v1, v2, ..., vk+1] 0 
= sc [ 0*v1 ] [0 * v2 , ..., 0 * vk+1]
 = sc [ 0 ] [0 ,..,0]
= [0, 0, ..., 0]
= O

Hence , Proved using induciton  *)
 

(* state atleast three properties 


1. Commutativty V.U = U.V
2. length U + length O = lemgth U
3.  length ( c * U ) = c * length U  ( c is scaler )  *)

(***************>>>>>>>>>>>>>>>>>>>>>proof of 1. Commutativty V.U = U.V  *)
(*
  
Proof by  induction ::::-><---:::::

Predicate P(n) :  For A vectors v , u  of dim n , u.v = v.u  holds

Base case : P(1)

0.v = scale 0 v = sc [] [v1] 0 = [0 * v1] = O
 P(1) is true

u.v = dot_prod u v = (u1.v1) :: dot_prod [] [] 

(u1.v1) = (v1.u1) ( using algebra ) - (1)
 
v.u = dot_prod v u  = (v1.u1) :: dot_prod [] []  - (2)

from (1) and (2) P(1) holds 

Inductive step:
 
Assume P(m) holds -->  To prove P(m+1).

u.v = dot_prod u v = dot_prod [ u1 ,.....um+1] [v1,....vm+1]

 = pdv [ u1 . v1 ] [ u2,...um+1] [v2,....vm+1] (def of dot _prod )

  using algebra for u1.v1 = v1.u1 and P(m)

 = pdv [v1.u1 ] [ v2,... vm+1] [ u2,.. um+1]

 = pdv [ v ] [u] [] (using def of pdv )

= dot_prod v u 

= v.u   

P(m) -> P(m+1)

Hence , Proved using induciton 

*)

(*  ******************>>>>>>>>>>>>> proof ::  length ( c * U ) = c * length U  ( c is scaler )  
  
Proof by  induction ::::-><---:::::

Predicate P(n) :  For A vectors  u  of dim n ,  length ( c * U ) = c * length U holds

Base case : P(1)

length ( c * U ) =  sqrt len c*u1 [] using def of length 
= sqrt c*c*u1*u1    using def of len 
= c*u1
= c*length u ( using def of len and length )

P(1) is true

Inductive step:
 
Assume P(m) holds -->  To prove P(m+1).

length ( c * U ) =  sqrt len c*u 0 using def of length 
= sqrt len [(c*u2) ** 2 ,... (c*um+1) ** 2]   ( c*u1) ** 2 
using P(m)
= sqrt c*c * len [u2 ,... um+1] u1*u1
= sqrt c*c * ( u1*u1 + .... + um+1 * um+1 ) ( using def of len )
= c  sqrt len [u1 ,... um+1] [] ( again using def of len )
 = c * length u ( usign def of length )

P(m) -> P(m+1)
Hence , Proved using induciton 

*)
(*  ***************************>>>>>>>> proof ::  length U + length O = lemgth U
  
Proof by  induction ::::-><---:::::

Predicate P(n) :  For A vectors  u  of dim n ,  length U + length O = lemgth U holds 

Base case : P(1)

length ( U ) + length O  =  sqrt len u1 0 +  sqrt len 0 0 using def of length 
= len u1 0  + 0     using def of len 
= length u ( using def of len and length )

P(1) is true

Inductive step:
 
Assume P(m) holds -->  To prove P(m+1).

length ( U ) + length O  =  sqrt len u 0 + sqrt len O 0      using def of length 
using P(m)
= sqrt len [u2,...um+1] u1*u1 + sqrt len [0,...0] 0
= sqrt len[u2 ,....um+1] u1 *u1 + 0
= sqrt len[u1,....um+1]
= length u ( using def of u )

P ( m ) -> P (m+1)

Hence , Proved using induciton 

*)



