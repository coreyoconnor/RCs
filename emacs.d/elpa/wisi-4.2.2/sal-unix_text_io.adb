--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

package body SAL.Unix_Text_IO is

   use Ada.Streams.Stream_IO;

   procedure Create
     (File : in out File_Type;
      Mode : in     File_Mode := Out_File;
      Name : in     String    := "";
      Form : in     String    := "")
   is begin
      Create
        (File.Stream,
         (case Mode is
          when In_File => Ada.Streams.Stream_IO.In_File,
          when Out_File => Ada.Streams.Stream_IO.Out_File,
          when Append_File => Ada.Streams.Stream_IO.Append_File),
        Name, Form);
   end Create;

   procedure Close (File : in out File_Type)
   is begin
      Close (File.Stream);
   end Close;

   procedure Put (File : in File_Type; Item : in Character)
   is begin
      Write (File.Stream, (1 => Ada.Streams.Stream_Element (Character'Pos (Item))));
   end Put;

   procedure Put (File : in File_Type; Item : in String)
   is
      use Ada.Streams;
      Stream_Item : constant Stream_Element_Array :=
        (for I in Stream_Element_Offset (Item'First) .. Stream_Element_Offset (Item'Last) =>
         Stream_Element (Character'Pos (Item (Integer (I)))));
   begin
      Write (File.Stream, Stream_Item);
   end Put;

   procedure Put_Line (File : in File_Type; Item : in String)
   is begin
      Put (File, Item); New_Line (File);
   end Put_Line;

   procedure New_Line (File : in File_Type)
   is begin
      Write (File.Stream, (1 => Ada.Streams.Stream_Element (Character'Pos (ASCII.LF))));
   end New_Line;

end SAL.Unix_Text_IO;
