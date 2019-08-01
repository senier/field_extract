with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
  SPARK_Mode
is

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      pragma Assert ((Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size < Data'Length
                     and then (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size <= Natural'Size
                     and then 2 ** Natural (((Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size) * Element_Type'Size) <= Long_Integer'Last);
      pragma Assert (Element_Type'Pos (Element_Type'Last) = 2 ** Element_Type'Size - 1);

      --  Prevent the prover from simplifiying
      function Element_Size return Natural is (Element_Type'Size) with Post => Element_Size'Result = Element_Type'Size;

      function Long_Integer_Size return Natural is (Long_Integer'Size) with Post => Long_Integer_Size'Result = Long_Integer'Size;

      LSB_Offset : constant Long_Integer := Offset_Type'Pos (Offset);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Element_Type'Size;

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Element_Type'Size);

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
         Lemma_Exp_Mono (2, E, Element_Type'Size);
         pragma Assert (2 ** E <= 2 ** Element_Type'Size);
         declare
            Mask : constant Long_Integer := (if Index < Most_Significant_Index then 2 ** Element_Type'Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Index)));
         begin
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

      Result : Long_Integer := 0;

   begin
      pragma Assert (Most_Significant_Index - Least_Significant_Index
                     = (LSB_Offset + Long_Integer (Value_Type'Size) - 1) / Element_Type'Size - LSB_Offset / Element_Type'Size);

      for I in Least_Significant_Index .. Most_Significant_Index - 1
      loop
         declare
            D_Current : constant Element_Type := D (I);
            D_Next    : constant Element_Type := D (I + 1);
         begin
            Lemma_Mult_Limit (Element_Type'Pos (D_Next) mod 2 ** LSE_Offset, LSE_Offset, 2 ** MSE_Offset, MSE_Offset);
            Lemma_Mult_Ge_0 (Element_Type'Pos (D_Next) mod 2 ** LSE_Offset, 2 ** MSE_Offset);
            declare
               Current   : constant Long_Integer := Element_Type'Pos (D_Current) / 2 ** LSE_Offset;
               Next      : constant Long_Integer := Element_Type'Pos (D_Next) mod 2 ** LSE_Offset * 2 ** MSE_Offset;
            begin
               pragma Assert (2 ** Element_Type'Size = 2 ** Element_Size);
               Lemma_Div_Limit (Element_Type'Pos (D_Current), Element_Size, LSE_Offset);
               declare
                  Value : constant Long_Integer := Current + Next;
               begin
                  pragma Loop_Invariant (Result >= Value_Type'Pos (Value_Type'First));
                  pragma Loop_Invariant (Value >= 0);
                  pragma Loop_Invariant (Result
                                         + 2 ** (Element_Type'Size * Natural (I - Least_Significant_Index )) * Value
                                         + 2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index)) *
                                           (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset)
                                         <= Value_Type'Pos (Value_Type'Last));
                  Lemma_Mult_Ge_0 (2 ** (Element_Type'Size * Natural (I - Least_Significant_Index)), Value);
                  Lemma_Mult_Ge_0 (2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index)),
                                   Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);
                  Result := Value * 2 ** (Element_Type'Size * Natural (I - Least_Significant_Index)) + Result;
               end;
            end;
         end;
      end loop;

      pragma Assert (Result >= Value_Type'Pos (Value_Type'First));
      pragma Assert (2 ** LSE_Offset <= Natural'Last);
      Lemma_Mult_Ge_0 (2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index)),
                       Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);

      Result := Result + 2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index))
        * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);
      return Value_Type'Val (Result);
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

end Extracts;
