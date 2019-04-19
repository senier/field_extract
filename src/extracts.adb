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
      Ghost, Import;

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      Right          : constant Long_Integer := Offset_Type'Pos (Offset) / Element_Type'Size;
      Left           : constant Long_Integer := (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size;
      Element_Offset : constant Natural      := Offset_Type'Pos (Offset) mod Element_Type'Size;
      Result         : Long_Integer          := 0;

      pragma Assert (Element_Type'Size - Offset_Type'Pos (Offset) mod Element_Type'Size <= 63);

      function D (Pos : Long_Integer) return Element_Type with
         Pre => Pos >= 0 and then Index_Type'Pos (Data'First) <= Index_Type'Pos (Data'Last) - Pos;

      function D (Pos : Long_Integer) return Element_Type
      is
         E : constant Natural := (Element_Offset + Value_Type'Size + Element_Type'Size - 1) mod Element_Type'Size + 1;
      begin
         Lemma_Exp_Is_Monotonic (2, E, Element_Type'Size);
         declare
            Mask : constant Long_Integer := (if Pos < Left then 2 ** Element_Type'Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Pos)));
         begin
            pragma Assert (2 ** E <= 2 ** Element_Type'Size);
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

   begin
      for I in Right .. Left - 1
      loop
         pragma Loop_Invariant (Index_Type'Pos (Data'First) <= Index_Type'Pos (Data'Last) - I);
         Lemma_Exp_Is_Monotonic (2, Element_Type'Size - Element_Offset, 62);
         pragma Assert (2 ** (Element_Type'Size - Element_Offset) <= 2 ** 62);
         declare
            Val : constant Long_Integer :=
              (Element_Type'Pos (D (I)) / 2 ** Element_Offset
               + (Element_Type'Pos (D (I + 1)) * 2 ** (Element_Type'Size - Element_Offset))
                  mod 2 ** Element_Type'Size);
         begin
            Result := Result + (2 ** Natural (Element_Type'Size * I) * Val);
         end;
      end loop;

      Result := Result + 2 ** Natural (Element_Type'Size * Left) * (Element_Type'Pos (D (Left)) / 2 ** Element_Offset);
      return Value_Type'Val (Result);
   end Extract;

end Extracts;
