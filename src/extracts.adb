package body Extracts
with
   SPARK_Mode
is

   -------------
   -- Extract --
   -------------

   function Extract
     (Data   : Byte_Array;
      Offset : Natural) return Value_Type
   is
      Next   : Value_Type;
      Result : Value_Type := 0;
      Last   : constant Natural := (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size - 1;

   begin
      for I in Natural range 0 .. Last
      loop
         Next := (if I < Last
                  then Value_Type (Data (Data'Last - I - 1) and (2**Offset - 1) and (2**Value_Type'Size - 1)) * 2**(Byte'Size - Offset)
                  else 0);
         Result := Result
           + 2**(Byte'Size * I)
             * (Value_Type (Data (Data'Last - I) / 2**Offset and (2**Value_Type'Size - 1)) + Next);
      end loop;
      return Result;
   end Extract;

end Extracts;
