with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
   SPARK_Mode
is

   procedure Lemma_Exp_Is_Monotonic (Base       : Long_Integer;
                                     Exponent_1 : Natural;
                                     Exponent_2 : Natural)
   with
      Pre  => Exponent_1 <= Exponent_2,
      Post => Base ** Exponent_1 <= Base ** Exponent_2,
      Ghost;

   procedure Lemma_Exp_Is_Monotonic (Base       : Long_Integer;
                                     Exponent_1 : Natural;
                                     Exponent_2 : Natural)
   is null with SPARK_Mode => Off;

   procedure Lemma_Exp_Eq (Base       : Long_Integer;
                           Exponent_1 : Natural;
                           Exponent_2 : Natural)
   with
      Pre  => Exponent_1 = Exponent_2,
      Post => Base ** Exponent_1 = Base ** Exponent_2,
      Ghost;

   procedure Lemma_Exp_Eq (Base       : Long_Integer;
                           Exponent_1 : Natural;
                           Exponent_2 : Natural) is
   begin
      null;
   end Lemma_Exp_Eq;

   procedure Lemma_Lt_Mult_Lt_Lt (Left        : Long_Integer;
                                  Left_Bound  : Long_Integer;
                                  Right       : Long_Integer;
                                  Right_Bound : Long_Integer;
                                  Result      : Long_Integer)
   with
     Pre  => Left <= Left_Bound
             and Right <= Right_Bound
             and Left_Bound * Right_Bound <= Result,
     Post => Left * Right <= Result,
     Ghost;

   procedure Lemma_Lt_Mult_Lt_Lt (Left        : Long_Integer;
                                  Left_Bound  : Long_Integer;
                                  Right       : Long_Integer;
                                  Right_Bound : Long_Integer;
                                  Result      : Long_Integer)
   is
   begin
      null;
   end Lemma_Lt_Mult_Lt_Lt;

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      Element_Size   : constant Natural := Element_Type'Size;
      Value_Size     : constant Natural := Value_Type'Size;

      Right          : constant Long_Integer := Offset_Type'Pos (Offset) / Long_Integer (Element_Size);
      Left           : constant Long_Integer := (Offset_Type'Pos (Offset) + Long_Integer (Value_Size) - 1)
                                                / Long_Integer (Element_Size);
      Element_Offset : constant Natural      := Offset_Type'Pos (Offset) mod Element_Size;
      Left_Offset    : constant Natural      := Element_Size - Element_Offset;
      Result         : Long_Integer          := 0;

      pragma Assert (Element_Size - Offset_Type'Pos (Offset) mod Element_Size <= 63);

      function D (Pos : Long_Integer) return Element_Type with
         Pre => Pos >= 0 and then Index_Type'Pos (Data'First) <= Index_Type'Pos (Data'Last) - Pos;

      function D (Pos : Long_Integer) return Element_Type
      is
         E : constant Natural := (Element_Offset + Value_Size + Element_Size - 1) mod Element_Size + 1;
      begin
         Lemma_Exp_Is_Monotonic (2, E, Element_Size);
         declare
            Mask : constant Long_Integer := (if Pos < Left then 2 ** Element_Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Pos)));
         begin
            pragma Assert (2 ** E <= 2 ** Element_Size);
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

      ES  : Natural := Element_Size;
   begin
      for I in Right .. Left - 1
      loop
         Lemma_Exp_Is_Monotonic (2, Element_Offset, 62);
         Lemma_Exp_Is_Monotonic (2, Left_Offset, 62);

         pragma Assert (2 ** Element_Offset <= 2 ** 62);
         pragma Assert (2 ** Left_Offset <= 2 ** 62);
         pragma Assert (2 ** ES <= Long_Integer'Last);

         pragma Assert (2 ** Left_Offset >= 0);
         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset >= 0));
         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset >= 0));

         pragma Assert (2 ** Element_Offset * 2 ** Left_Offset = 2 ** (Element_Offset + Left_Offset));
         pragma Assert (2 ** (Element_Offset + Left_Offset) = 2 ** (Element_Offset + (Element_Size - Element_Offset)));
         pragma Assert (Element_Offset + (Element_Size - Element_Offset) = Element_Size);
         pragma Assert (2 ** (Element_Offset + (Element_Size - Element_Offset)) = 2 ** ES);
         pragma Assert (2 ** Element_Offset * 2 ** Left_Offset = 2 ** ES);

         pragma Assert (ES <= 62);

         Lemma_Lt_Mult_Lt_Lt (Left        => Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset,
                              Left_Bound  => 2 ** Element_Offset,
                              Right       => 2 ** Left_Offset,
                              Right_Bound => 2 ** Left_Offset,
                              Result      => 2 ** ES);

         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset <= 2 ** ES));
         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset <= Long_Integer'Last));

         declare
            Val : constant Long_Integer :=
              (Element_Type'Pos (D (I)) / 2 ** Element_Offset
               + (Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset)
                  mod 2 ** Element_Size);
         begin
            Result := Result + (2 ** (Element_Size * Natural (I)) * Val);
         end;
      end loop;

      Result := Result + 2 ** (Element_Size * Natural (Left)) * (Element_Type'Pos (D (Left)) / 2 ** Element_Offset);
      return Value_Type'Val (Result);
   end Extract;

end Extracts;
