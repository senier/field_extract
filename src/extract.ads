with Extracts;

package Extract
with
   SPARK_Mode
is

   type U13 is mod 2**13;
   subtype Byte_Array is Extracts.Byte_Array;

   function Extract_13 is new Extracts.Extract (U13);

end Extract;
