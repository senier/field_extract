package IO
with
   SPARK_Mode,
   Abstract_State => (State),
   Initializes => State
is
   procedure Put (Message : String)
   with
      Global => (In_Out => State);

   procedure Put_Line (Message : String)
   with
      Global => (In_Out => State);
end IO;
