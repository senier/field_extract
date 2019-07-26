with Ada.Text_IO; use Ada.Text_IO;

package body Extracts with
  SPARK_Mode
is

   function Extract (Data   : Array_Type;
                     Offset : Offset_Type) return Value_Type
   is
      Least_Significant_Index : constant Long_Integer :=
         Offset_Type'Pos (Offset) / Long_Integer (Element_Type'Size);
      Most_Significant_Index  : constant Long_Integer :=
         (Offset_Type'Pos (Offset) + Long_Integer (Value_Type'Size) - 1) / Long_Integer (Element_Type'Size);

      Element_Offset : constant Natural      := Offset_Type'Pos (Offset) mod Element_Type'Size;
      Left_Offset    : constant Natural      := Element_Type'Size - Element_Offset;
      Result         : Long_Integer          := 0;

      function Element_Type_Size return Natural is (Element_Type'Size)
        with Post => (Element_Type_Size'Result = Element_Type'Size);

      function D (Pos : Long_Integer) return Element_Type with
         Pre => Pos >= 0 and then Pos < Data'Length;

      function D (Pos : Long_Integer) return Element_Type
      is
         E : constant Natural := (Element_Offset + Value_Type'Size + Element_Type'Size - 1) mod Element_Type'Size + 1;
      begin
         declare
            Mask : constant Long_Integer := (if Pos < Most_Significant_Index then 2 ** Element_Type'Size else 2 ** E);
            Val  : constant Element_Type := Data (Index_Type'Val ((Index_Type'Pos (Data'Last) - Pos)));
         begin
            return Element_Type'Val (Element_Type'Pos (Val) mod Mask);
         end;
      end D;

   begin
      for I in Least_Significant_Index .. Most_Significant_Index - 1
      loop
         declare
            Current : constant Long_Integer := Element_Type'Pos (D (I)) / 2 ** Element_Offset;
            Next    : constant Long_Integer := Element_Type'Pos (D (I + 1)) mod 2 ** Element_Offset * 2 ** Left_Offset;
            Value   : constant Long_Integer := (Current + Next) mod 2 ** Element_Type'Size;
         begin
            Result := Result + (2 ** (Element_Type_Size * Natural (I)) * Value);
         end;
      end loop;
      Result := Result + 2 ** (Element_Type_Size * Natural (Most_Significant_Index))
                           * (Element_Type'Pos (D (Most_Significant_Index)) / 2 ** Element_Offset);
      return Value_Type'Val (Result);
   end Extract;

end Extracts;
