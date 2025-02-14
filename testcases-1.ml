open A2

(* Complex Nested Expression Tests *)

let%test "complex_nested_vector_ops" = 
  (eval (Add(
    ScalProd(
      Mag(Add(ConstV [1.0; 2.0], ConstV [-1.0; 3.0])), 
      ConstV [3.0; -1.0]
    ),
    ScalProd(
      DotProd(ConstV [2.0; 1.0], ConstV [1.0; -1.0]),
      ConstV [0.5; 0.5]
    )
  )) = V [15.5; -4.5])

let%test "nested_conditional_with_vector_ops" = 
  (eval (Cond(
    IsZero(Add(
      ConstV [1.0; -1.0], 
      ScalProd(ConstS (-1.0), ConstV [1.0; -1.0])
    )),
    Mag(Add(ConstV [3.0; 4.0], ConstV [1.0; -1.0])),
    DotProd(ConstV [1.0; 1.0], ConstV [2.0; 2.0])
  )) = S 5.0)

let%test "complex_angle_calculation" = 
  (eval (Angle(
    ScalProd(
      Mag(ConstV [3.0; 4.0]),
      ConstV [1.0; 0.0]
    ),
    Add(
      ScalProd(ConstS 2.0, ConstV [0.0; 1.0]),
      ConstV [0.0; -1.0]
    )
  )) = S (Float.pi /. 2.0))

let%test "nested_boolean_operations" = 
  (eval (Cond(
    ScalProd(
      IsZero(Add(ConstV [1.0; -1.0], ConstV [-1.0; 1.0])),
      Inv(IsZero(Mag(ConstV [0.0; 0.000001])))
    ),
    ConstS 1.0,
    ConstS 0.0
  )) = S 1.0)

let%test "complex_type_checking" = 
  (try 
    let _ = type_of (
      Add(
        ScalProd(
          Cond(
            IsZero(ConstV [0.0; 0.0]),
            ConstS 1.0,
            Mag(ConstV [3.0; 4.0])
          ),
          ConstV [1.0; 2.0]
        ),
        DotProd(
          ConstV [1.0; 1.0],
          ConstV [2.0; 2.0]
        )
      )
    ) in false
   with (Wrong _) -> true)

let%test "nested_magnitude_operations" = 
  (eval (Mag(
    Add(
      ScalProd(
        DotProd(ConstV [2.0; 3.0], ConstV [4.0; 5.0]),
        ConstV [1.0; 1.0]
      ),
      ScalProd(
        Mag(ConstV [3.0; 4.0]),
        ConstV [-1.0; 2.0]
      )
    )
  )) = S (sqrt 1413.0))

let%test "complex_error_propagation" = 
  (try 
    let _ = eval (
      DotProd(
        ScalProd(
          Mag(ConstV [1.0; 2.0]),
          ConstV [3.0; 4.0; 5.0]
        ),
        Add(
          ConstV [1.0; 2.0],
          ConstV [3.0; 4.0]
        )
      )
    ) in false
   with (Wrong _) -> true)

let%test "nested_zero_checks" = 
  (eval (IsZero(
    Add(
      ScalProd(
        DotProd(
          ConstV [1.0; -2.0],
          ConstV [2.0; 1.0]
        ),
        ConstV [1.0; 1.0]
      ),
      ScalProd(
        ConstS (-0.0),
        ConstV [0.0; 0.0]
      )
    )
  )) = B true)

let%test "complex_conditional_type_mismatch" = 
  (try 
    let _ = type_of (
      Cond(
        IsZero(Mag(ConstV [0.0; 0.0])),
        Add(
          ScalProd(ConstS 2.0, ConstV [1.0; 1.0]),
          ConstV [2.0; 2.0]
        ),
        DotProd(
          ConstV [1.0; 1.0],
          ConstV [2.0; 2.0]
        )
      )
    ) in false
   with (Wrong _) -> true)


   open A2

   (* Advanced Vector Manipulation Tests *)
   let%test "recursive_vector_transformations" = 
     (eval (Mag(
       Add(
         ScalProd(
           Angle(
             ScalProd(Mag(ConstV [1.0; 1.0]), ConstV [1.0; 0.0]),
             ConstV [0.0; 1.0]
           ),
           ConstV [3.0; 4.0]
         ),
         ScalProd(
           DotProd(
             ScalProd(ConstS 2.0, ConstV [1.0; 0.0]),
             ConstV [0.0; 1.0]
           ),
           ConstV [-1.0; 2.0]
         )
       )
     )) =S (sqrt((3.0 *. Float.pi /. 2.0) ** 2.0 +. (2.0 *. Float.pi) ** 2.0))) 
   
   let%test "complex_conditional_chain" = 
     (eval (Cond(
       ScalProd(
         IsZero(
           DotProd(
             ScalProd(ConstS (-1.0), ConstV [1.0; 0.0]),
             ConstV [1.0; 0.0]
           )
         ),
         Inv(
           IsZero(
             Mag(
               Add(ConstV [0.1; -0.1], ConstV [-0.1; 0.1])
             )
           )
         )
       ),
       Mag(
         Add(
           ScalProd(ConstS 3.0, ConstV [2.0; -1.0]),
           ScalProd(ConstS (-2.0), ConstV [-1.0; 2.0])
         )
       ),
       DotProd(
         ScalProd(ConstS 2.0, ConstV [1.0; 1.0]),
         ConstV [1.0; -1.0]
       )
     )) = S 0.0)
   
   let%test "nested_angle_calculations" = 
     (eval (Angle(
       ScalProd(
         Add(
           Mag(ConstV [3.0; 4.0]),
           DotProd(ConstV [1.0; 1.0], ConstV [1.0; -1.0])
         ),
         ConstV [1.0; 0.0]
       ),
       ScalProd(
         Cond(
           IsZero(ConstV [0.0; 0.0]),
           ConstS 1.0,
           Mag(ConstV [3.0; 4.0])
         ),
         ConstV [0.0; 1.0]
       )
     )) = S (Float.pi /. 2.0))
   
   let%test "complex_type_error_propagation" = 
     (try 
       let _ = type_of (
         Add(
           ScalProd(
             Cond(
               IsZero(
                 DotProd(
                   ConstV [1.0; 0.0],
                   ScalProd(ConstS (-1.0), ConstV [1.0; 0.0])
                 )
               ),
               Mag(ConstV [3.0; 4.0]),
               DotProd(ConstV [1.0; 1.0], ConstV [2.0; 2.0])
             ),
             ConstV [1.0; 2.0]
           ),
           Angle(
             ConstV [1.0; 0.0],
             ConstV [0.0; 1.0]
           )
         )
       ) in false
      with (Wrong _) -> true)
   
   let%test "vector_dimension_mismatch_in_nested_ops" = 
     (try 
       let _ = eval (
         DotProd(
           Add(
             ScalProd(
               Mag(ConstV [1.0; 2.0; 3.0]),
               ConstV [1.0; 0.0]
             ),
             ConstV [2.0; 3.0]
           ),
           ScalProd(
             DotProd(
               ConstV [1.0; 1.0],
               ConstV [1.0; -1.0]
             ),
             ConstV [1.0; 0.0; -1.0]
           )
         )
       ) in false
      with (Wrong _) -> true)
   
      let%test "deep_nested_boolean_arithmetic" = 
      (eval (
        Cond(
          ScalProd(
            IsZero(
              Add(
                ScalProd(ConstS 2.0, ConstV [1.0; -1.0]),
                ConstV [-2.0; 2.0]
              )
            ),
            Inv(
              IsZero(
                DotProd(
                  ConstV [1.0; 0.0],
                  ConstV [0.0; 1.0]
                )
              )
            )
          ),
          ConstS 1.0,
          ConstS (-1.0)
        )
      ) = S (-1.0))
