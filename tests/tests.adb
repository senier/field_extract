with IO;
with Extract;

procedure Tests
with
   SPARK_Mode
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
      Put (Message & ": ");
      if not Condition
      then
         Put_Line ("ERROR, " & Error);
      else
         Put_Line ("OK");
      end if;
   end Assert;

   procedure Check_57 (Message  : String;
                       Data     : Byte_Array;
                       Offset   : Natural;
                       Expected : U57)
   is
      Result : U57;
   begin
      Result := Extract_57 (Data, Offset);
      Assert (Result = Expected, Message, "expected " & Expected'Img & ", got " & Result'Img);
   end Check_57;

   procedure Check_13 (Message  : String;
                       Data     : Byte_Array;
                       Offset   : Natural;
                       Expected : U13)
   is
      Result : U13;
   begin
      Result := Extract_13 (Data, Offset);
      Assert (Result = Expected, Message, "expected " & Expected'Img & ", got " & Result'Img);
   end Check_13;

   procedure Check_7 (Message  : String;
                      Data     : Byte_Array;
                      Offset   : Natural;
                      Expected : U7)
   is
      Result : U7;
   begin
      Result := Extract_7 (Data, Offset);
      Assert (Result = Expected, Message, "expected " & Expected'Img & ", got " & Result'Img);
   end Check_7;

   Data   : Byte_Array (1..3) := (16#de#, 16#ad#, 16#be#);
   Data64 : Byte_Array (1..8) := (16#de#, 16#ad#, 16#be#, 16#ef#, 16#ca#, 16#fe#, 16#ba#, 16#be#);

begin
   Put_Line ("Running tests...");

   Check_13 ("Extract U13, 3 bytes, Off 0", Data, 0, 16#0dbe#);
   Check_13 ("Extract U13, 3 bytes, Off 1", Data, 1, 16#16df#);
   Check_13 ("Extract U13, 3 bytes, Off 2", Data, 2, 16#0b6f#);
   Check_13 ("Extract U13, 3 bytes, Off 3", Data, 3, 16#15b7#);
   Check_13 ("Extract U13, 3 bytes, Off 7", Data, 7, 16#1d5b#);

   Check_7 ("Extract U7, 3 bytes, Off 0", Data, 0, 16#3e#);
   Check_7 ("Extract U7, 3 bytes, Off 1", Data, 1, 16#5f#);
   Check_7 ("Extract U7, 3 bytes, Off 2", Data, 2, 16#6f#);
   Check_7 ("Extract U7, 3 bytes, Off 7", Data, 7, 16#5b#);

   Check_57 ("Extract U57, 8 bytes, Off 0", Data64, 0, 16#adbeefcafebabe#);
   Check_57 ("Extract U57, 8 bytes, Off 3", Data64, 3, 16#1d5b7ddf95fd757#);
   Check_57 ("Extract U57, 8 bytes, Off 7", Data64, 7, 16#1bd5b7ddf95fd75#);

end Tests;
