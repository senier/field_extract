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
                     Offset : Offset_Type) return Value_Type
   with
      Pre => Element_Type'Size < 63
             and Data'Length > (Offset_Type'Pos (Offset) + Value_Type'Size - 1) / Element_Type'Size;

end Extracts;
