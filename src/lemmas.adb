package body Lemmas with SPARK_Mode
is
   procedure Div_Limit (Value : Long_Integer;
                              I     : Natural;
                              J     : Natural)
   is
   begin
      pragma Assert (Value <= 2 ** I);
      Div_Mono (Value, 2 ** I, 2 ** J);
      pragma Assert (Value / 2 ** J <= 2 ** I / 2 ** J);
      Exp_Div (2, I, J);
      pragma Assert (2 ** I / 2 ** J = 2 ** (I - J));
   end Div_Limit;

   procedure Mult_Limit (Value_1 : Long_Integer;
                               Exp_1   : Natural;
                               Value_2 : Long_Integer;
                               Exp_2   : Natural)
   is
   begin
      Mult_Mono (Value_1, 2 ** Exp_1, Value_2);
      pragma Assert (Value_1 * Value_2 <= 2 ** Exp_1 * Value_2);
      Mult_Mono (Value_2, 2 ** Exp_2, 2 ** Exp_1);
      pragma Assert (2 ** Exp_1 * Value_2 <= 2 ** Exp_1 * 2 ** Exp_2);
      Exp_Mult (2, Exp_1, Exp_2);
      pragma Assert (2 ** Exp_1 * 2 ** Exp_2 = 2 ** (Exp_1 + Exp_2));
      pragma Assert (Value_1 * Value_2 <= 2 ** (Exp_1 + Exp_2));
   end Mult_Limit;

   procedure Exp_Mono (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2  : Natural)
   is
   begin
      null;
   end Exp_Mono;

   procedure Exp_Mono_Strict (Base  : Long_Integer;
                                    Exp_1 : Natural;
                                    Exp_2  : Natural)
   is
   begin
      Exp_Mono (Base, Exp_1 + 1, Exp_2);
      pragma Assert (Base ** (Exp_1 + 1) <= Base ** Exp_2);
      pragma Assert (Base ** Exp_1 * Base <= Base ** Exp_2);
      Mult_Mono_Strict (1, Base, Base ** Exp_1);
      pragma Assert (Base ** Exp_1 < Base ** Exp_1 * Base);
      pragma Assert (Base ** Exp_1 < Base ** Exp_2);
   end Exp_Mono_Strict;

   procedure Mult_Ge_0 (Factor_1 : Long_Integer;
                              Factor_2 : Long_Integer)
   is
   begin
      null;
   end Mult_Ge_0;

   procedure Base_Eq_Exp_Eq (Base  : Long_Integer;
                                   Exp_1 : Natural;
                                   Exp_2 : Natural)
   is
   begin
      null;
   end Base_Eq_Exp_Eq;

   procedure Mult_Exp_Lt_Exp
     (X : Long_Integer; J : Natural; Y : Long_Integer; K : Natural)
   is
   begin
      pragma Assert (X * 2 ** K + Y < X * 2 ** K + 2 ** K);
      pragma Assert (X * 2 ** K + 2 ** K = (X + 1) * 2 ** K);
      pragma Assert (X + 1 <= 2 ** J);
      Mult_Mono (X + 1, 2 ** J, 2 ** K);
      pragma Assert ((X + 1) * 2 ** K <= 2 ** J * 2 ** K);
      pragma Assert (2 ** J * 2 ** K = 2 ** (J + K));
      pragma Assert (X * 2 ** K + Y < 2 ** (J + K));
   end Mult_Exp_Lt_Exp;

   procedure Mult_Mono (X : Long_Integer;
                              Y : Long_Integer;
                              Z : Long_Integer)
   is
   begin
      null;
   end Mult_Mono;

   procedure Mult_Mono_Strict (X : Long_Integer;
                                     Y : Long_Integer;
                                     Z : Long_Integer)
   is
   begin
      null;
   end Mult_Mono_Strict;


   procedure Exp_Mult (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2 : Natural)
   is
   begin
     null;
   end Exp_Mult;

   procedure Exp_Div (Base  : Long_Integer;
                            Exp_1 : Natural;
                            Exp_2 : Natural)
   is
   begin
      pragma Assert (Exp_1 = Exp_1 - Exp_2 + Exp_2);
      pragma Assert (Base ** Exp_1 = Base ** (Exp_1 - Exp_2 + Exp_2));
      Exp_Mult (Base, Exp_1 - Exp_2, Exp_2);
      pragma Assert (Base ** (Exp_1 - Exp_2 + Exp_2) = Base ** (Exp_1 - Exp_2) * Base ** Exp_2);
      pragma Assert (Base ** Exp_1 = Base ** (Exp_1 - Exp_2) * Base ** Exp_2);
      pragma Assert (Base ** Exp_1 / Base ** Exp_2 = Base ** (Exp_1 - Exp_2) * Base ** Exp_2 / Base ** Exp_2);
      Mult_Div_Id (Base ** (Exp_1 - Exp_2), Base ** Exp_2);
      pragma Assert (Base ** (Exp_1 - Exp_2) * Base ** Exp_2 / Base ** Exp_2 = Base ** (Exp_1 - Exp_2));
   end Exp_Div;

   procedure Div_Mono (X : Long_Integer;
                             Y : Long_Integer;
                             Z : Long_Integer)
   is
   begin
      null;
   end Div_Mono;

   procedure Mult_Div_Id (X : Long_Integer;
                                Y : Long_Integer)
   is
   begin
      null;
   end Mult_Div_Id;

   procedure Plus_Base_Le_Exp (B : Long_Integer;
                                     E : Natural)
   is
   begin
      if B ** (E - 1) > Long_Integer'Last - B or else B ** (E - 1) + B > B ** E then
         --  Proof by contradiction
         Exp_Mono_Strict (B, E - 2, E - 1);
         pragma Assert (B > B * (B ** (E - 1) - B ** (E - 2)));
         Mult_Le_Cancel_Left1 (B, B ** (E - 1) - B ** (E - 2));
         pragma Assert (B <= B * (B ** (E - 1) - B ** (E - 2)));
      end if;

   end Plus_Base_Le_Exp;

   procedure Mult_Mono2 (X : Long_Integer;
                               Y : Long_Integer;
                               Z : Long_Integer)
   is
   begin
      null;
   end Mult_Mono2;

   procedure Mult_Le_Cancel_Left1 (F1 : Long_Integer;
                                         F2 : Long_Integer)
   is
   begin
      null;
   end Mult_Le_Cancel_Left1;

   procedure Exp_Eq (Base  : Long_Integer;
                           Exp_1 : Natural;
                           Exp_2  : Natural)
   is
   begin
      null;
   end Exp_Eq;

end Lemmas;
