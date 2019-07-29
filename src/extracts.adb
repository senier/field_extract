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
               Lemma_Base_Eq_Exp_Eq (2, Element_Type'Size, Element_Size);
               pragma Assert (2 ** Element_Type'Size = 2 ** Element_Size);
               Lemma_Div_Limit (Element_Type'Pos (D_Current), Element_Size, LSE_Offset);
               declare
                  Value : constant Long_Integer := Current + Next;
               begin
                  Lemma_Mult_Ge_0 (Element_Type'Pos (D_Next) mod 2 ** LSE_Offset, 2 ** MSE_Offset);
                  pragma Loop_Invariant (I + 1 < Data'Length);
                  pragma Loop_Invariant (2 ** Element_Size <= Long_Integer'Last);
                  pragma Loop_Invariant (Result >= Value_Type'Pos (Value_Type'First));
                  pragma Loop_Invariant (Value >= 0);
                  pragma Loop_Invariant (Result + 2 ** (Element_Type'Size * Natural (I - Least_Significant_Index)) * Value
                                         <= Value_Type'Pos (Value_Type'Last));
                  Lemma_Mult_Ge_0 (2 ** (Element_Type'Size * Natural (I - Least_Significant_Index)), Value);
                  Result := Result + (2 ** (Element_Type'Size * Natural (I - Least_Significant_Index)) * Value);
               end;
            end;
         end;
      end loop;

      pragma Assert (Result >= Value_Type'Pos (Value_Type'First));
      pragma Assert (Result <= Value_Type'Pos (Value_Type'Last)
                     - 2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index))
                     * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset));

      Lemma_Mult_Ge_0 (2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index)),
                       Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);

      Result := Result + 2 ** (Element_Type'Size * Natural (Most_Significant_Index - Least_Significant_Index))
        * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);
      return Value_Type'Val (Result);
   end Extract;

   procedure Lemma_Div_Limit (Value : Long_Integer;
                              I     : Natural;
                              J     : Natural) with SPARK_Mode => Off
   is
   begin
      pragma Assert (Value <= 2 ** I);
      pragma Assert (Value / 2 ** J <= 2 ** I / 2 ** J);
      pragma Assert (2 ** I / 2 ** J = 2 ** (I - J));
   end Lemma_Div_Limit;

   procedure Lemma_Mult_Limit (Value_1 : Long_Integer;
                               Exp_1   : Natural;
                               Value_2 : Long_Integer;
                               Exp_2   : Natural)
   is
   begin
      null;
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
      null;
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

end Extracts;
