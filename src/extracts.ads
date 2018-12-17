package Extracts
with
   SPARK_Mode
is
   type Byte is mod 2**8;

   subtype Index_Type is Natural range Natural'First .. Natural'Last - 1;
   type Byte_Array is array (Index_Type range <>) of Byte;

   generic
      type Value_Type is mod <>;
   function Extract_U (Data   : Byte_Array;
                       Offset : Natural) return Value_Type
   with
      Pre => Offset < 8 and then Data'Length >= (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size;

   generic
      type Value_Type is range <>;
   function Extract_I (Data   : Byte_Array;
                       Offset : Natural) return Value_Type
   with
      Pre => Offset < 8 and then Data'Length >= (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size;

end Extracts;
