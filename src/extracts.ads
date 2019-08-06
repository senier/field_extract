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
                     Offset : Offset_Type) return Value_Type with
     Pre => (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size < Data'Length
            and then (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size <= Natural'Size
            and then Natural (((Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size) * Element_Type'Size) < Long_Integer'Size - 1
            and then 2 ** (Element_Type'Size - Natural (Offset_Type'Pos (Offset) mod Element_Type'Size)) <= Long_Integer'Last;
end Extracts;
