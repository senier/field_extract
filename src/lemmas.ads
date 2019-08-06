package Lemmas with SPARK_Mode, Ghost
is
   procedure Div_Limit (Value : Long_Integer;
                        I     : Natural;
                        J     : Natural) with
      Pre  => 2 ** I <= Long_Integer'Last and 2 ** J <= Long_Integer'Last and J <= I and Value <= 2 ** I,
      Post => Value / 2 ** J <= 2 ** (I - J),
      Ghost;

   procedure Mult_Limit (Value_1 : Long_Integer;
                         Exp_1   : Natural;
                         Value_2 : Long_Integer;
                         Exp_2   : Natural) with
      Pre  => 2 ** Exp_1 <= Long_Integer'Last
              and 2 ** Exp_2 <= Long_Integer'Last
              and 0 <= Value_2
              and Value_1 <= 2 ** Exp_1
              and Value_2 <= 2 ** Exp_2,
      Post => Value_1 * Value_2 <= 2 ** (Exp_1 + Exp_2),
      Ghost;

   procedure Exp_Mono (Base  : Long_Integer;
                       Exp_1 : Natural;
                       Exp_2  : Natural) with
      Pre  => 0 < Base and Exp_1 <= Exp_2,
      Post => Base ** Exp_1 <= Base ** Exp_2,
      Ghost;

   procedure Exp_Eq (Base  : Long_Integer;
                     Exp_1 : Natural;
                     Exp_2  : Natural) with
      Pre  => 0 < Base and Exp_1 = Exp_2,
      Post => Base ** Exp_1 = Base ** Exp_2,
      Ghost;

   procedure Exp_Mono_Strict (Base  : Long_Integer;
                              Exp_1 : Natural;
                              Exp_2  : Natural) with
      Pre  => Base > 1 and Base ** Exp_1 <= Long_Integer'Last and Exp_1 < Exp_2,
      Post => Base ** Exp_1 < Base ** Exp_2,
      Ghost;

   procedure Mult_Ge_0 (Factor_1 : Long_Integer;
                        Factor_2 : Long_Integer) with
      Pre  => Factor_1 >= 0 and Factor_2 >= 0,
      Post => Factor_1 * Factor_2 >= 0,
      Ghost;

   procedure Base_Eq_Exp_Eq (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2 : Natural) with
      Pre  => Exp_1 = Exp_2,
      Post => Base ** Exp_1 = Base ** Exp_2,
      Ghost;

   procedure Mult_Mono (X : Long_Integer;
                        Y : Long_Integer;
                        Z : Long_Integer) with
      Pre  => 0 <= Z and X <= Y,
      Post => Z * X <= Z * Y,
     Ghost;

   procedure Mult_Mono2 (X : Long_Integer;
                         Y : Long_Integer;
                         Z : Long_Integer) with
      Pre  => 0 < Z and 0 <= X and X <= Y,
      Post => X <= Y * Z,
      Ghost;

   procedure Div_Mono (X : Long_Integer;
                       Y : Long_Integer;
                       Z : Long_Integer) with
      Pre  => 0 < Z and X <= Y,
      Post => X / Z <= Y / Z,
      Ghost;

   procedure Mult_Mono_Strict (X : Long_Integer;
                               Y : Long_Integer;
                               Z : Long_Integer) with
      Pre  => 0 < Z and X < Y,
      Post => X * Z < Y * Z,
      Ghost;

   procedure Mult_Exp_Lt_Exp (X : Long_Integer;
                              J : Natural;
                              Y : Long_Integer;
                              K : Natural) with
     Pre  => 2 ** J < Natural'Last and 2 ** K < Natural'Last and X < 2 ** J and Y < 2 ** K,
     Post => X * 2 ** K + Y < 2 ** (J + K),
     Ghost;

   procedure Exp_Mult (Base  : Long_Integer;
                       Exp_1 : Natural;
                       Exp_2 : Natural) with
     Post => Base ** Exp_1 * Base ** Exp_2 = Base ** (Exp_1 + Exp_2),
     Ghost;

   procedure Exp_Div (Base  : Long_Integer;
                      Exp_1 : Natural;
                      Exp_2 : Natural) with
     Pre  => 0 < Base
             and then Exp_2 <= Exp_1
             and then Base ** (Exp_1 - Exp_2) <= Long_Integer'Last
             and then Base ** Exp_2 <= Long_Integer'Last,
     Post => Base ** Exp_1 / Base ** Exp_2 = Base ** (Exp_1 - Exp_2),
     Ghost;

   procedure Mult_Div_Id (X : Long_Integer;
                          Y : Long_Integer) with
     Pre  => 0 < Y,
     Post => X * Y / Y = X,
     Ghost;

   procedure Plus_Base_Le_Exp (B : Long_Integer;
                               E : Natural) with
     Pre  => 1 < B and 1 < E and B ** E <= Long_Integer'Last,
     Post => B ** (E - 1) + B <= B ** E,
     Ghost;

   procedure Mult_Le_Cancel_Left1 (F1 : Long_Integer;
                                   F2 : Long_Integer) with
     Pre  => 0 <= F1 and 1 <= F2,
     Post => F1 <= F1 * F2,
     Ghost;

end Lemmas;
