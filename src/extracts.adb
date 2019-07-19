with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
  SPARK_Mode,
  Annotate => (GNATprove, Terminating)
is

   procedure Lemma_Exp_Is_Monotonic (Base       : Long_Integer;
                                     Exponent_1 : Natural;
                                     Exponent_2 : Natural)
   with
      Pre  => Exponent_1 <= Exponent_2,
      Post => Base ** Exponent_1 <= Base ** Exponent_2,
      Ghost, Annotate => (GNATprove, Terminating);

   procedure Lemma_Exp_Is_Monotonic (Base       : Long_Integer;
                                     Exponent_1 : Natural;
                                     Exponent_2 : Natural)
   is null;

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
   is null;

   procedure Lemma_Mult_Gt_0 (Factor_1 : Long_Integer;
                              Factor_2 : Long_Integer)
   with
       Pre  => Factor_1 >= 0 and Factor_2 >= 0,
       Post => Factor_1 * Factor_2 >= 0,
       Ghost;

   procedure Lemma_Mult_Gt_0 (Factor_1 : Long_Integer;
                              Factor_2 : Long_Integer)
   is null;

   procedure Lemma_Val_Lt_Mod (Value  : Long_Integer;
                               Modulo : Long_Integer)
   with
       Pre  => Modulo > 0,
       Post => Value mod Modulo < Modulo,
       Ghost;

   procedure Lemma_Val_Lt_Mod (Value  : Long_Integer;
                               Modulo : Long_Integer)
   is null;


   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      pragma Assert (Element_Type'Size < 63);
--        pragma Assert (Data'Length > (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size);
--        pragma Assert (Element_Type'Size * Natural ((Offset_Type'Pos (Offset) + Long_Integer (Value_Type'Size) - 1) / Long_Integer (Element_Type'Size)) <= Natural'Last);
--        pragma Assert (2 ** (Element_Type'Size * Natural ((Offset_Type'Pos (Offset) + Long_Integer (Value_Type'Size) - 1)
--                                          / Long_Integer (Element_Type'Size)))
--                                          * 2 ** Element_Type'Size <= Long_Integer'Last);

      Right          : constant Long_Integer := Offset_Type'Pos (Offset) / Long_Integer (Element_Type'Size);
      Left           : constant Long_Integer := (Offset_Type'Pos (Offset) + Long_Integer (Value_Type'Size) - 1)
                                                / Long_Integer (Element_Type'Size);
      Element_Offset : constant Natural      := Offset_Type'Pos (Offset) mod Element_Type'Size;
      Left_Offset    : constant Natural      := Element_Type'Size - Element_Offset;
      Result         : Long_Integer          := 0;

      function Element_Type_Size return Natural is (Element_Type'Size)
        with Post => (Element_Type_Size'Result = Element_Type'Size);

      pragma Assert (Element_Type'Size - Offset_Type'Pos (Offset) mod Element_Type'Size <= 63);

      function D (Pos : Long_Integer) return Element_Type with
         Pre => Pos >= 0 and then Index_Type'Pos (Data'First) <= Index_Type'Pos (Data'Last) - Pos;

      function D (Pos : Long_Integer) return Element_Type
      is
         E : constant Natural := (Element_Offset + Value_Type'Size + Element_Type'Size - 1) mod Element_Type'Size + 1;
      begin
         Lemma_Exp_Is_Monotonic (2, E, Element_Type'Size);
         pragma Assert (2 ** E <= 2 ** Element_Type'Size);
         declare
            Mask : constant Long_Integer := (if Pos < Left then 2 ** Element_Type'Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Pos)));
         begin
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

   begin
      pragma Assert (Element_Type'Size * Natural (Left) <= Natural'Last);
      --  pragma Assert (2 ** (Element_Type'Size * Natural (Left)) * 2 ** Element_Type'Size <= Long_Integer'Last);

      for I in Right .. Left - 1
      loop
         Lemma_Exp_Is_Monotonic (2, Element_Offset, 62);
         Lemma_Exp_Is_Monotonic (2, Left_Offset, 62);

         pragma Assert (2 ** Element_Offset <= 2 ** 62);
         pragma Assert (2 ** Element_Type'Size <= Long_Integer'Last);
         pragma Assert (2 ** Left_Offset >= 0);
         pragma Assert (2 ** Left_Offset <= Long_Integer'Last);
         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset >= 0));

         Lemma_Mult_Gt_0 (2 ** Left_Offset, (Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset));
         pragma Assert ((Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset >= 0));

         pragma Assert (2 ** Element_Offset * 2 ** (Element_Type'Size - Element_Offset)
                        = 2 ** (Element_Offset + (Element_Type'Size - Element_Offset)));
         pragma Assert (Element_Offset <= Element_Type'Size);
         pragma Assert (2 ** (Element_Offset + (Element_Type_Size - Element_Offset)) = 2 ** Element_Type_Size);
         pragma Assert (2 ** Element_Offset * 2 ** (Element_Type'Size - Element_Offset) <= 2 ** Element_Type_Size);

         Lemma_Lt_Mult_Lt_Lt (Left        => Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset,
                              Left_Bound  => 2 ** Element_Offset,
                              Right       => 2 ** Left_Offset,
                              Right_Bound => 2 ** Left_Offset,
                              Result      => 2 ** Element_Type_Size);
         declare
            Right : constant Long_Integer := Element_Type'Pos (D (I)) / 2 ** Element_Offset;
            Left  : constant Long_Integer := Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset;
            pragma Assert (Right + Left <= Long_Integer'Last);
            Val : constant Long_Integer := (Left + Right) mod 2 ** Element_Type'Size;
         begin
            Lemma_Val_Lt_Mod (Element_Type'Pos (D (I)) / 2 ** Element_Offset, 2 ** Element_Type'Size);
            pragma Assert (2 ** (Element_Type'Size * Natural (I)) * 2 ** Element_Type'Size <= Long_Integer'Last);
            Lemma_Lt_Mult_Lt_Lt (Left        => 2 ** (Element_Type'Size * Natural (I)),
                                 Left_Bound  => 2 ** (Element_Type'Size * Natural (I)),
                                 Right       => Val,
                                 Right_Bound => 2 ** Element_Type'Size,
                                 Result      => Long_Integer'Last);

            pragma Assert (2 ** (Element_Type_Size * Natural (I)) * Val <= Long_Integer'Last);
            Result := Result + (2 ** (Element_Type_Size * Natural (I)) * Val);
         end;
      end loop;

      Result := Result + 2 ** (Element_Type_Size * Natural (Left)) * (Element_Type'Pos (D (Left)) / 2 ** Element_Offset);
      pragma Assert (Result <= Value_Type'Pos (Value_Type'Last));
      pragma Assert (Result >= Value_Type'Pos (Value_Type'First));
      return Value_Type'Val (Result);
   end Extract;

end Extracts;
