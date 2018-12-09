package Extract
with
   SPARK_Mode
is
   type Byte is mod 2**8;

   subtype Index_Type is Natural range Natural'First .. Natural'Last - 1;
   type Byte_Array is array (Index_Type range <>) of Byte;

   generic
      type Value_Type is mod <>;
   function Extract (Data   : Byte_Array;
                     Offset : Natural) return Value_Type
   with
      Pre => Data'Length > (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size - 1 and Offset < 8;

end Extract;
