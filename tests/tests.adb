with IO;
with Extract;
with Extracts;

procedure Tests
   with SPARK_Mode
is
   use IO;
   use Extract;

   procedure Assert (Condition : Boolean;
                     Message   : String;
                     Error     : String)
   with
      Pre => Error'Length < 1000 and Message'Last < Natural'Last - 3 and Message'Length < 1000;

   procedure Assert (Condition : Boolean;
                     Message   : String;
                     Error     : String)
   is
   begin
      Put_Line ((if Condition then "OK: " else "ERROR: ") & Message & (if Condition then "" else " - " & Error));
   end Assert;

   generic
      type Expected_Type is (<>);
   procedure Check (Message  : String;
                    Data     : Byte_Array;
                    Offset   : Natural;
                    Expected : Expected_Type)
   with
      Pre => Message'Last < Natural'Last - 3
             and then Message'Length < 1000
             and then Data'Length > (Natural'Pos (Offset) + Expected_Type'Size - 1) / Byte'Size
             and then (Natural'Pos (Offset) + Expected_Type'Size - 1) / Byte'Size < Data'Length
             and then (Natural'Pos (Offset) + Expected_Type'Size - 1) / Byte'Size <= Natural'Size
             and then ((Natural'Pos (Offset) + Expected_Type'Size - 1) / Byte'Size) * Byte'Size < Long_Integer'Size - 1
             and then 2 ** (Byte'Size - Natural (Natural'Pos (Offset) mod Byte'Size)) <= Long_Integer'Last;

   procedure Check (Message  : String;
                    Data     : Byte_Array;
                    Offset   : Natural;
                    Expected : Expected_Type)
   is
      Result : Expected_Type;
      function Extract is new Extracts.Extract (Index_Type   => Index_Type,
                                                Element_Type => Byte,
                                                Array_Type   => Byte_Array,
                                                Offset_Type  => Natural,
                                                Value_Type   => Expected_Type);
   begin
      Result := Extract (Data, Offset);
      Assert (Condition => Result = Expected,
              Message   => Message,
              Error     => "Invalid result");
   end Check;

   procedure Check_U57 is new Check (U57);
   procedure Check_U13 is new Check (U13);
   procedure Check_U7  is new Check (U7);
   procedure Check_U2  is new Check (U2);
   procedure Check_U1  is new Check (U1);
   procedure Check_I13 is new Check (I13);
   procedure Check_I7  is new Check (I7);
   procedure Check_I57 is new Check (I57);
   procedure Check_I2  is new Check (I2);
   procedure Check_I1  is new Check (I1);

   Data   : Byte_Array (1..3) := (16#de#, 16#ad#, 16#be#);
   Data2  : Byte_Array (1..2) := (16#ff#, 16#ff#);
   Data64 : Byte_Array (1..8) := (16#de#, 16#ad#, 16#be#, 16#ef#, 16#ca#, 16#fe#, 16#ba#, 16#be#);

begin
   Put_Line ("Running tests...");

   Check_U13 ("Extract U13, 3 bytes, Off 0", Data, 0, 16#0dbe#);
   Check_U13 ("Extract U13, 3 bytes, Off 1", Data, 1, 16#16df#);
   Check_U13 ("Extract U13, 3 bytes, Off 2", Data, 2, 16#0b6f#);
   Check_U13 ("Extract U13, 3 bytes, Off 3", Data, 3, 16#15b7#);
   Check_U13 ("Extract U13, 3 bytes, Off 7", Data, 7, 16#1d5b#);
   Check_U13 ("Extract U13, 8 bytes, Off 11", Data64, 11, 16#1fd7#);

   Check_U7 ("Extract U7, 3 bytes, Off 0", Data, 0, 16#3e#);
   Check_U7 ("Extract U7, 3 bytes, Off 1", Data, 1, 16#5f#);
   Check_U7 ("Extract U7, 3 bytes, Off 2", Data, 2, 16#6f#);
   Check_U7 ("Extract U7, 3 bytes, Off 7", Data, 7, 16#5b#);

   Check_U57 ("Extract U57, 8 bytes, Off 0", Data64, 0, 16#adbeefcafebabe#);
   Check_U57 ("Extract U57, 8 bytes, Off 3", Data64, 3, 16#1d5b7ddf95fd757#);
   Check_U57 ("Extract U57, 8 bytes, Off 7", Data64, 7, 16#1bd5b7ddf95fd75#);

   Check_U2 ("Extract U2, 3 bytes, Off 23", Data (1..1), 0, 2);
   Check_U2 ("Extract U2, 3 bytes, Off 22", Data (1..1), 6, 3);
   Check_U2 ("Extract U2, 3 bytes, Off 16", Data (1..2), 7, 1);
   Check_U2 ("Extract U2, 3 bytes, Off 17", Data (3..3), 0, 2);
   Check_U2 ("Extract U2, 3 bytes, Off 18", Data (3..3), 1, 3);
   Check_U2 ("Extract U2, 3 bytes, Off  1", Data (3..3), 1, 3);
   Check_U2 ("Extract U2, 3 bytes, Off  0", Data (3..3), 0, 2);
   Check_U2 ("Extract U2, 3 bytes, Off 16", Data2, 7, 3);

   Check_U1 ("Extract U1, 3 bytes, Off 23", Data (3..3), 7, 1);
   Check_U1 ("Extract U1, 3 bytes, Off 17", Data (3..3), 1, 1);
   Check_U1 ("Extract U1, 3 bytes, Off 16", Data (3..3), 0, 0);
   Check_U1 ("Extract U1, 3 bytes, Off 15", Data (2..2), 7, 1);
   Check_U1 ("Extract U1, 3 bytes, Off 14", Data (2..2), 6, 0);
   Check_U1 ("Extract U1, 3 bytes, Off 13", Data (2..2), 5, 1);
   Check_U1 ("Extract U1, 3 bytes, Off 12", Data (2..2), 4, 0);
   Check_U1 ("Extract U1, 3 bytes, Off 11", Data (2..2), 3, 1);
   Check_U1 ("Extract U1, 3 bytes, Off  1", Data (1..1), 1, 1);
   Check_U1 ("Extract U1, 3 bytes, Off  0", Data (1..1), 0, 0);

   Check_I13 ("Extract I13, 3 bytes, Off 0", Data, 0, 16#0dbe#);
   Check_I13 ("Extract I13, 3 bytes, Off 1", Data, 1, 16#16df#);
   Check_I13 ("Extract I13, 3 bytes, Off 2", Data, 2, 16#0b6f#);
   Check_I13 ("Extract I13, 3 bytes, Off 3", Data, 3, 16#15b7#);
   Check_I13 ("Extract I13, 3 bytes, Off 7", Data, 7, 16#1d5b#);

   Check_I7 ("Extract I7, 3 bytes, Off 0", Data, 0, 16#3e#);
   Check_I7 ("Extract I7, 3 bytes, Off 1", Data, 1, 16#5f#);
   Check_I7 ("Extract I7, 3 bytes, Off 2", Data, 2, 16#6f#);
   Check_I7 ("Extract I7, 3 bytes, Off 7", Data, 7, 16#5b#);

   Check_I57 ("Extract I57, 8 bytes, Off 0", Data64, 0, 16#adbeefcafebabe#);
   Check_I57 ("Extract I57, 8 bytes, Off 3", Data64, 3, 16#1d5b7ddf95fd757#);
   Check_I57 ("Extract I57, 8 bytes, Off 7", Data64, 7, 16#1bd5b7ddf95fd75#);

   Check_I2 ("Extract I2, 3 bytes, Off 23", Data (1..1), 0, 2);
   Check_I2 ("Extract I2, 3 bytes, Off 22", Data (1..1), 6, 3);
   Check_I2 ("Extract I2, 3 bytes, Off 16", Data (1..2), 7, 1);
   Check_I2 ("Extract I2, 3 bytes, Off 17", Data (3..3), 0, 2);
   Check_I2 ("Extract I2, 3 bytes, Off 18", Data (3..3), 1, 3);
   Check_I2 ("Extract I2, 3 bytes, Off  1", Data (3..3), 1, 3);
   Check_I2 ("Extract I2, 3 bytes, Off  0", Data (3..3), 0, 2);

   Check_I1 ("Extract I1, 3 bytes, Off 23", Data (3..3), 7, 1);
   Check_I1 ("Extract I1, 3 bytes, Off 17", Data (3..3), 1, 1);
   Check_I1 ("Extract I1, 3 bytes, Off 16", Data (3..3), 0, 0);
   Check_I1 ("Extract I1, 3 bytes, Off 15", Data (2..2), 7, 1);
   Check_I1 ("Extract I1, 3 bytes, Off 14", Data (2..2), 6, 0);
   Check_I1 ("Extract I1, 3 bytes, Off 13", Data (2..2), 5, 1);
   Check_I1 ("Extract I1, 3 bytes, Off 12", Data (2..2), 4, 0);
   Check_I1 ("Extract I1, 3 bytes, Off 11", Data (2..2), 3, 1);
   Check_I1 ("Extract I1, 3 bytes, Off  1", Data (1..1), 1, 1);
   Check_I1 ("Extract I1, 3 bytes, Off  0", Data (1..1), 0, 0);

end Tests;
