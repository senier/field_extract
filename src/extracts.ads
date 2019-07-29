package Extracts
  with
     SPARK_Mode
is
   generic
      type Index_Type   is (<>);
      type Element_Type is (<>);
      type Array_Type   is array (Index_Type range <>) of Element_Type;
      type Offset_Type  is (<>);
      type Value_Type   is (<>);
   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type;

   procedure Lemma_Div_Limit (Value : Long_Integer;
                              I     : Natural;
                              J     : Natural) with
      Pre  => J <= I and Value <= 2 ** I,
      Post => Value / 2 ** J <= 2 ** (I - J),
      Ghost;

   procedure Lemma_Mult_Limit (Value_1 : Long_Integer;
                               Exp_1   : Natural;
                               Value_2 : Long_Integer;
                               Exp_2   : Natural) with
      Pre  => Value_1 <= 2 ** Exp_1 and Value_2 <= 2 ** Exp_2,
      Post => Value_1 * Value_2 <= 2 ** (Exp_1 + Exp_2),
      Ghost;

   procedure Lemma_Exp_Mono (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2  : Natural) with
      Pre  => Exp_1 <= Exp_2,
      Post => Base ** Exp_1 <= Base ** Exp_2,
      Ghost;

   procedure Lemma_Exp_Mono_Strict (Base  : Long_Integer;
                                    Exp_1 : Natural;
                                    Exp_2  : Natural) with
      Pre  => Exp_1 < Exp_2,
      Post => Base ** Exp_1 < Base ** Exp_2,
      Ghost;

   procedure Lemma_Mult_Ge_0 (Factor_1 : Long_Integer;
                              Factor_2 : Long_Integer) with
      Pre  => Factor_1 >= 0 and Factor_2 >= 0,
      Post => Factor_1 * Factor_2 >= 0,
      Ghost;

   procedure Lemma_Base_Eq_Exp_Eq (Base  : Long_Integer;
                                   Exp_1 : Natural;
                                   Exp_2 : Natural) with
      Pre  => Exp_1 = Exp_2,
      Post => Base ** Exp_1 = Base ** Exp_2,
      Ghost;

end Extracts;
