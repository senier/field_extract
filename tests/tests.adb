with Ada.Text_IO;
with Extract;

procedure Tests
is
   use Ada.Text_IO;
   use Extract;

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

   Data : Byte_Array (1..3) := (16#de#, 16#ad#, 16#be#);

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

end Tests;
