with Extracts;

package Extract
with
   SPARK_Mode
is

   type U1 is mod 2**1 with Size => 1;
   type U2 is mod 2**2 with Size => 2;
   type U7 is mod 2**7 with Size => 7;
   type U13 is mod 2**13 with Size => 13;
   type U57 is mod 2**57 with Size => 57;
   subtype Byte_Array is Extracts.Byte_Array;

   function Extract_57 is new Extracts.Extract (U57);
   function Extract_13 is new Extracts.Extract (U13);
   function Extract_7 is new Extracts.Extract (U7);
   function Extract_2 is new Extracts.Extract (U2);
   function Extract_1 is new Extracts.Extract (U1);

end Extract;
