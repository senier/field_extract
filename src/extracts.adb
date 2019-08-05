with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
  SPARK_Mode
is

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      pragma Assert (Element_Type'Pos (Element_Type'First) = 0);
      pragma Assert (Element_Type'Pos (Element_Type'Last) = 2 ** Element_Type'Size - 1);

      --  Prevent the prover from simplifiying
      function Element_Size return Natural is (Element_Type'Size) with Post => Element_Size'Result = Element_Type'Size;

      function Value_Size return Natural is (Value_Type'Size) with Post => Value_Size'Result = Value_Type'Size;
      function Long_Integer_Size return Natural is (Long_Integer'Size) with Post => Long_Integer_Size'Result = Long_Integer'Size;

      LSB_Offset : constant Long_Integer := Offset_Type'Pos (Offset);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Element_Type'Size;

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Element_Type'Size);

      LSE_Size : constant Long_Integer := Value_Type'Size mod Element_Type'Size;

      --  Index pointing to most significant index
      Most_Significant_Index : constant Long_Integer :=
        (LSB_Offset + Long_Integer (Value_Type'Size) - 1) / Element_Type'Size;

      --  Bits the most significant element (MSE) is shifted right relative to a single element
      MSE_Offset             : constant Natural := Element_Type'Size - LSE_Offset;

      function D (Index : Long_Integer) return Element_Type with
        Pre => Index >= 0 and then Index < Data'Length;

      function D (Index : Long_Integer) return Element_Type
      is
         E : constant Natural := (LSE_Offset + Value_Type'Size + Element_Type'Size - 1) mod Element_Type'Size + 1;
      begin
         pragma Assert (Element_Size = Element_Type'Size);
         Lemma_Exp_Eq (2, Element_Type'Size, Element_Size);
         pragma Assert (2 ** Element_Type'Size = 2 ** Element_Size);
         declare
            Mask : constant Long_Integer := (if Index < Most_Significant_Index then 2 ** Element_Type'Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Index)));
         begin
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

      type Result_Type is mod 2 ** Long_Integer'Size;
      Result : Result_Type := 0;

   begin
      pragma Assert (Value_Type'Size - Value_Type'Size mod Element_Type'Size
                     = Element_Type'Size * (Value_Type'Size / Element_Type'Size));

      for I in Least_Significant_Index .. Most_Significant_Index - 1
      loop
         declare
            D_Current : constant Element_Type := D (I);
            D_Next    : constant Element_Type := D (I + 1);
         begin
            Lemma_Mult_Limit (Element_Type'Pos (D_Next) mod 2 ** LSE_Offset, LSE_Offset, 2 ** MSE_Offset, MSE_Offset);
            Lemma_Mult_Ge_0 (Element_Type'Pos (D_Next) mod 2 ** LSE_Offset, 2 ** MSE_Offset);
            declare
               Current : constant Long_Integer := Element_Type'Pos (D_Current) / 2 ** LSE_Offset;
               Next    : constant Long_Integer := Element_Type'Pos (D_Next) mod 2 ** LSE_Offset * 2 ** MSE_Offset;
            begin
               Result := Result
                         + (Result_Type (Current) + Result_Type (Next))
                           * 2 ** (Element_Type'Size * Natural (I - Least_Significant_Index));
            end;
         end;
      end loop;

      pragma Assert (Result_Type'(2 ** LSE_Offset) > 0);
      Result := Result + 2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index))
        * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);
      return Value_Type'Val (Result mod 2 ** Value_Size);
   end Extract;

   procedure Lemma_Div_Limit (Value : Long_Integer;
                              I     : Natural;
                              J     : Natural)
   is
   begin
      pragma Assert (Value <= 2 ** I);
      Lemma_Div_Mono (Value, 2 ** I, 2 ** J);
      pragma Assert (Value / 2 ** J <= 2 ** I / 2 ** J);
      Lemma_Exp_Div (2, I, J);
      pragma Assert (2 ** I / 2 ** J = 2 ** (I - J));
   end Lemma_Div_Limit;

   procedure Lemma_Mult_Limit (Value_1 : Long_Integer;
                               Exp_1   : Natural;
                               Value_2 : Long_Integer;
                               Exp_2   : Natural)
   is
   begin
      Lemma_Mult_Mono (Value_1, 2 ** Exp_1, Value_2);
      pragma Assert (Value_1 * Value_2 <= 2 ** Exp_1 * Value_2);
      Lemma_Mult_Mono (Value_2, 2 ** Exp_2, 2 ** Exp_1);
      pragma Assert (2 ** Exp_1 * Value_2 <= 2 ** Exp_1 * 2 ** Exp_2);
      Lemma_Exp_Mult (2, Exp_1, Exp_2);
      pragma Assert (2 ** Exp_1 * 2 ** Exp_2 = 2 ** (Exp_1 + Exp_2));
      pragma Assert (Value_1 * Value_2 <= 2 ** (Exp_1 + Exp_2));
   end Lemma_Mult_Limit;

   procedure Lemma_Exp_Mono (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2  : Natural)
   is
   begin
      null;
   end Lemma_Exp_Mono;

   procedure Lemma_Exp_Mono_Strict (Base  : Long_Integer;
                                    Exp_1 : Natural;
                                    Exp_2  : Natural)
   is
   begin
      Lemma_Exp_Mono (Base, Exp_1 + 1, Exp_2);
      pragma Assert (Base ** (Exp_1 + 1) <= Base ** Exp_2);
      pragma Assert (Base ** Exp_1 * Base <= Base ** Exp_2);
      Lemma_Mult_Mono_Strict (1, Base, Base ** Exp_1);
      pragma Assert (Base ** Exp_1 < Base ** Exp_1 * Base);
      pragma Assert (Base ** Exp_1 < Base ** Exp_2);
   end Lemma_Exp_Mono_Strict;

   procedure Lemma_Mult_Ge_0 (Factor_1 : Long_Integer;
                              Factor_2 : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Ge_0;

   procedure Lemma_Base_Eq_Exp_Eq (Base  : Long_Integer;
                                   Exp_1 : Natural;
                                   Exp_2 : Natural)
   is
   begin
      null;
   end Lemma_Base_Eq_Exp_Eq;

   procedure Lemma_Mult_Exp_Lt_Exp
     (X : Long_Integer; J : Natural; Y : Long_Integer; K : Natural)
   is
   begin
      pragma Assert (X * 2 ** K + Y < X * 2 ** K + 2 ** K);
      pragma Assert (X * 2 ** K + 2 ** K = (X + 1) * 2 ** K);
      pragma Assert (X + 1 <= 2 ** J);
      Lemma_Mult_Mono (X + 1, 2 ** J, 2 ** K);
      pragma Assert ((X + 1) * 2 ** K <= 2 ** J * 2 ** K);
      pragma Assert (2 ** J * 2 ** K = 2 ** (J + K));
      pragma Assert (X * 2 ** K + Y < 2 ** (J + K));
   end Lemma_Mult_Exp_Lt_Exp;

   procedure Lemma_Mult_Mono (X : Long_Integer;
                              Y : Long_Integer;
                              Z : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Mono;

   procedure Lemma_Mult_Mono_Strict (X : Long_Integer;
                                     Y : Long_Integer;
                                     Z : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Mono_Strict;


   procedure Lemma_Exp_Mult (Base  : Long_Integer;
                             Exp_1 : Natural;
                             Exp_2 : Natural)
   is
   begin
     null;
   end Lemma_Exp_Mult;

   procedure Lemma_Exp_Div (Base  : Long_Integer;
                            Exp_1 : Natural;
                            Exp_2 : Natural)
   is
   begin
      pragma Assert (Exp_1 = Exp_1 - Exp_2 + Exp_2);
      pragma Assert (Base ** Exp_1 = Base ** (Exp_1 - Exp_2 + Exp_2));
      Lemma_Exp_Mult (Base, Exp_1 - Exp_2, Exp_2);
      pragma Assert (Base ** (Exp_1 - Exp_2 + Exp_2) = Base ** (Exp_1 - Exp_2) * Base ** Exp_2);
      pragma Assert (Base ** Exp_1 = Base ** (Exp_1 - Exp_2) * Base ** Exp_2);
      pragma Assert (Base ** Exp_1 / Base ** Exp_2 = Base ** (Exp_1 - Exp_2) * Base ** Exp_2 / Base ** Exp_2);
      Lemma_Mult_Div_Id (Base ** (Exp_1 - Exp_2), Base ** Exp_2);
      pragma Assert (Base ** (Exp_1 - Exp_2) * Base ** Exp_2 / Base ** Exp_2 = Base ** (Exp_1 - Exp_2));
   end Lemma_Exp_Div;

   procedure Lemma_Div_Mono (X : Long_Integer;
                             Y : Long_Integer;
                             Z : Long_Integer)
   is
   begin
      null;
   end Lemma_Div_Mono;

   procedure Lemma_Mult_Div_Id (X : Long_Integer;
                                Y : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Div_Id;

   procedure Lemma_Plus_Base_Le_Exp (B : Long_Integer;
                                     E : Natural)
   is
   begin
      if B ** (E - 1) > Long_Integer'Last - B or else B ** (E - 1) + B > B ** E then
         --  Proof by contradiction
         Lemma_Exp_Mono_Strict (B, E - 2, E - 1);
         pragma Assert (B > B * (B ** (E - 1) - B ** (E - 2)));
         Lemma_Mult_Le_Cancel_Left1 (B, B ** (E - 1) - B ** (E - 2));
         pragma Assert (B <= B * (B ** (E - 1) - B ** (E - 2)));
      end if;

   end Lemma_Plus_Base_Le_Exp;

   procedure Lemma_Mult_Mono2 (X : Long_Integer;
                               Y : Long_Integer;
                               Z : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Mono2;

   procedure Lemma_Mult_Le_Cancel_Left1 (F1 : Long_Integer;
                                         F2 : Long_Integer)
   is
   begin
      null;
   end Lemma_Mult_Le_Cancel_Left1;

   procedure Lemma_Exp_Eq (Base  : Long_Integer;
                           Exp_1 : Natural;
                           Exp_2  : Natural)
   is
   begin
      null;
   end Lemma_Exp_Eq;

end Extracts;
