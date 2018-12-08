package Extract
is
   type U13 is mod 2**13;

   type Byte is mod 2**8;
   type Byte_Array is array (Natural range <>) of Byte;

   function Extract (Data   : Byte_Array;
                     Offset : Natural) return U13;
end Extract;
