package Extract
with
   SPARK_Mode
is
   type Value_Type is mod 2**13;

   type Byte is mod 2**8;
   type Byte_Array is array (Natural range <>) of Byte;

   function Extract (Data   : Byte_Array;
                     Offset : Natural) return Value_Type
   with
      Pre => Offset < 8;
end Extract;
