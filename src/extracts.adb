package body Extracts
with
   SPARK_Mode
is

   ---------------
   -- Extract_U --
   ---------------

   function Extract_U
     (Data   : Byte_Array;
      Offset : Natural) return Value_Type
   is
      Next   : Value_Type;
      Result : Value_Type := 0;
      Last   : constant Natural := (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size - 1;

   begin
      --  We need two guard expressions to make the compiler happy in case
      --  this generic function gets instantiated with a modular type with
      --  size 1 (i.e. mod 2**1). In this case, the frontend aborts with a
      --  instantiation error, as it assumes that 2**k exceed Value_Type and
      --  a Constraint_Error could be raised. In fact, this cannot happen for
      --  the first expression, as Last = 0 and thus I < Last is staticlaly
      --  false for Value_Type'Size < 8. The second expression is safe, as
      --  I (and thus the exponent) is 0.

      for I in Natural range 0 .. Last
      loop
         Next := (if I < Last
                  then Value_Type (Data (Data'Last - I - 1)
                     and (2**Offset - 1)
                     and (2**Value_Type'Size - 1)) *
                     --  Guard #1, see comment above
                     (if Value_Type'Last > 1 then 2**(Byte'Size - Offset) else 0)
                  else 0);
         Result := Result
           --  Guard #2, see comment above
           + (if Value_Type'Last > 1 then 2**(Byte'Size * I) else 1)
            * (Value_Type (Data (Data'Last - I) / 2**Offset and (2**Value_Type'Size - 1)) + Next);
      end loop;
      return Result;
   end Extract_U;

   ---------------
   -- Extract_I --
   ---------------

   function Extract_I
     (Data   : Byte_Array;
      Offset : Natural) return Value_Type
   is
      Next   : Value_Type;
      Result : Value_Type := 0;
      Last   : constant Natural := (Offset + Value_Type'Size + Byte'Size - 1) / Byte'Size - 1;

   begin
      --  We need two guard expressions to make the compiler happy in case
      --  this generic function gets instantiated with a modular type with
      --  size 1 (i.e. mod 2**1). In this case, the frontend aborts with a
      --  instantiation error, as it assumes that 2**k exceed Value_Type and
      --  a Constraint_Error could be raised. In fact, this cannot happen for
      --  the first expression, as Last = 0 and thus I < Last is staticlaly
      --  false for Value_Type'Size < 8. The second expression is safe, as
      --  I (and thus the exponent) is 0.

      for I in Natural range 0 .. Last
      loop
         Next := (if I < Last
                  then Value_Type (Data (Data'Last - I - 1)
                     and (2**Offset - 1)
                     and (2**Value_Type'Size - 1)) *
                     --  Guard #1, see comment above
                     (if Value_Type'Last > 1 then 2**(Byte'Size - Offset) else 0)
                  else 0);
         Result := Result
           --  Guard #2, see comment above
           + (if Value_Type'Last > 1 then 2**(Byte'Size * I) else 1)
            * (Value_Type (Data (Data'Last - I) / 2**Offset and (2**Value_Type'Size - 1)) + Next);
      end loop;
      return Result;
   end Extract_I;

end Extracts;
