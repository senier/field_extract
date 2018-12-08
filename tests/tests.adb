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

   procedure Check (Message  : String;
                    Data     : Byte_Array;
                    Offset   : Natural;
                    Expected : U13)
   is
      Result : U13;
   begin
      Result := Extract.Extract (Data, Offset);
      Assert (Result = Expected, Message, "expected " & Expected'Img & ", got " & Result'Img);
   end Check;

   Data : Byte_Array (1..3) := (16#de#, 16#ad#, 16#be#);

begin
   Put_Line ("Running tests...");

   Check ("Extract", Data, 0, 16#0dbe#);
   Check ("Extract", Data, 1, 16#16df#);
   Check ("Extract", Data, 2, 16#0b6f#);
   Check ("Extract", Data, 7, 16#1d5b#);
end Tests;
