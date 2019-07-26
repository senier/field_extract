with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
  SPARK_Mode
is

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      LSB_Offset : constant Long_Integer := Offset_Type'Pos (Offset);

      --  Index pointing to least significant element
      Least_Significant_Index : constant Long_Integer := LSB_Offset / Element_Type'Size;

      --  Bits the least significant element (LSE) is shifted left relative to a single element
      LSE_Offset : constant Natural := Natural (LSB_Offset mod Element_Type'Size);

      --  Index pointing to most significant index
      Most_Significant_Index : constant Long_Integer :=
         (LSB_Offset + Long_Integer (Value_Type'Size) - 1) / Element_Type'Size;

      --  Bits the most significant element (MSE) is shifted right relative to a single element
      MSE_Offset : constant Natural := Element_Type'Size - LSE_Offset;

      function D (Index : Long_Integer) return Element_Type with
         Pre => Index >= 0 and then Index < Data'Length;

      function D (Index : Long_Integer) return Element_Type
      is
         E : constant Natural := (LSE_Offset + Value_Type'Size + Element_Type'Size - 1) mod Element_Type'Size + 1;
      begin
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
            Current : constant Long_Integer := Element_Type'Pos (D (I)) / 2 ** LSE_Offset;
            Next    : constant Long_Integer := Element_Type'Pos (D (I + 1)) mod 2 ** LSE_Offset * 2 ** MSE_Offset;
            Value   : constant Long_Integer := (Current + Next) mod 2 ** Element_Type'Size;
         begin
            Result := Result + (2 ** (Element_Type'Size * Natural (I)) * Value);
         end;
      end loop;
      Result := Result + 2 ** (Element_Type'Size * Natural (Most_Significant_Index))
                           * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** LSE_Offset);
      return Value_Type'Val (Result);
   end Extract;

end Extracts;
