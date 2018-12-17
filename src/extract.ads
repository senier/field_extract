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

   type I1 is range 0 .. 2**1 - 1 with Size => 1;
   type I2 is range 0 .. 2**2 - 1 with Size => 2;
   type I7 is range 0 .. 2**7 - 1 with Size => 7;
   type I13 is range 0 .. 2**13 - 1 with Size => 13;
   type I57 is range 0 .. 2**57 - 1 with Size => 57;

   subtype Byte_Array is Extracts.Byte_Array;

   function Extract_U57 is new Extracts.Extract_U (U57);
   function Extract_U13 is new Extracts.Extract_U (U13);
   function Extract_U7 is new Extracts.Extract_U (U7);
   function Extract_U2 is new Extracts.Extract_U (U2);
   function Extract_U1 is new Extracts.Extract_U (U1);

   function Extract_I57 is new Extracts.Extract_I (I57);
   function Extract_I13 is new Extracts.Extract_I (I13);
   function Extract_I7 is new Extracts.Extract_I (I7);
   function Extract_I2 is new Extracts.Extract_I (I2);
   function Extract_I1 is new Extracts.Extract_I (I1);

end Extract;
