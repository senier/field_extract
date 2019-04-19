with Extracts;

package Extract
with
   SPARK_Mode
is

   type Byte is mod 2**8;

   subtype Index_Type is Natural range Natural'First .. Natural'Last - 1;
   type Byte_Array is array (Index_Type range <>) of Byte;

   type U1 is mod 2**1 with Size => 1;
   type U2 is mod 2**2 with Size => 2;
   type U7 is mod 2**7 with Size => 7;
   type U13 is mod 2**13 with Size => 13;
   type U57 is mod 2**57 with Size => 57;

   type I1 is range 0 .. 2**1 - 1 with Size => 1;
   type I2 is range 0 .. 2**2 - 1 with Size => 2;
   type I7 is range 0 .. 2**7 - 1 with Size => 7;
   type I13 is range 0 .. 2**13 - 1 with Size => 13;
   type I57 is range 0 .. 2**57 - 1 with Size => 57;

end Extract;
