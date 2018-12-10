with Ada.Text_IO;

package body IO
with
   SPARK_Mode => Off
is
   procedure Put (Message : String) renames Ada.Text_IO.Put;
   procedure Put_Line (Message : String) renames Ada.Text_IO.Put_Line;
end IO;
