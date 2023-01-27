--  Abstract :
--
--  Replacement for subset of Ada.Text_IO, using Unix line endings
--  on all platforms.
--
--  For very large files, this is significantly faster than Text_IO
--  output followed by dos2unix.
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

private with Ada.Streams.Stream_IO;
package SAL.Unix_Text_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   procedure Create
     (File : in out File_Type;
      Mode : in     File_Mode := Out_File;
      Name : in     String    := "";
      Form : in     String    := "");

   procedure Close (File : in out File_Type);

   procedure Put (File : in File_Type; Item : in Character);
   procedure Put (File : in File_Type; Item : in String);

   procedure Put_Line (File : in File_Type; Item : in String);

   procedure New_Line (File : in File_Type);

private
   type File_Type is limited record
      Stream : Ada.Streams.Stream_IO.File_Type;
   end record;

end SAL.Unix_Text_IO;
