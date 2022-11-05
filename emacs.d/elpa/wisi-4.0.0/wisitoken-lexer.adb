--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with GNAT.Strings;
package body WisiToken.Lexer is

   function Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      ID_Image : constant String := WisiToken.Image (Item.ID, Descriptor);
   begin
      if Item.Char_Region = Null_Buffer_Region then
         return "(" & ID_Image & ")";

      else
         return "(" & ID_Image & ", " & Image (Item.Char_Region) &
           (if Item.ID = Descriptor.New_Line_ID
            then ", " & Image (Item.Line_Region)
            else "") & ")";
      end if;
   end Image;

   function Full_Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      return "(" & Image (Item.ID, Descriptor) & ", " &
        Image (Item.Byte_Region) & ", " &
        Image (Item.Char_Region) & ", " &
        Image (Item.Line_Region) & ")";
   end Full_Image;

   procedure Shift
     (Token       : in out Lexer.Token;
      Shift_Bytes : in     Base_Buffer_Pos;
      Shift_Chars : in     Base_Buffer_Pos;
      Shift_Lines : in     Base_Line_Number_Type)
   is begin
      Token.Byte_Region := @ + Shift_Bytes;
      Token.Char_Region := @ + Shift_Chars;
      Token.Line_Region := @ + Shift_Lines;
   end Shift;

   function To_String (Item : in Recover_Characters) return String
   is begin
      for I in Item'Range loop
         if Item (I) = ASCII.NUL then
            return Item (Item'First .. I - 1);
         end if;
      end loop;
      return Item;
   end To_String;

   function Column (Token : in Lexer.Token; Line_Begin_Char_Pos : in Buffer_Pos) return Ada.Text_IO.Count
   is begin
      if Token.Line_Region.First = 1 then
         return Ada.Text_IO.Count (Token.Char_Region.First);

      elsif Line_Begin_Char_Pos = Invalid_Buffer_Pos then
         return 0;

      else
         return Ada.Text_IO.Count (Token.Char_Region.First - Line_Begin_Char_Pos);
      end if;
   end Column;

   procedure Begin_Pos
     (Object     : in     Source;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   is begin
      Begin_Byte := Object.Buffer_Nominal_First_Byte;
      Begin_Char := Object.Buffer_Nominal_First_Char;
      Begin_Line := Object.Line_Nominal_First;
   end Begin_Pos;

   function Has_Source (Lexer : access constant Instance) return Boolean
   is begin
      return Has_Source (Lexer.Source);
   end Has_Source;

   function Buffer_Text (Lexer : in Instance; Byte_Region : in WisiToken.Buffer_Region) return String
   is
      First : constant Integer := Integer
        (Byte_Region.First - Lexer.Source.Buffer_Nominal_First_Byte + Buffer_Pos'First);
      Last  : constant Integer := Integer
        (Byte_Region.Last - Lexer.Source.Buffer_Nominal_First_Byte + Buffer_Pos'First);
   begin
      return String (Buffer (Lexer.Source) (First .. Last));
   end Buffer_Text;

   function Buffer_Region_Byte (Lexer : in Instance) return WisiToken.Buffer_Region
   is begin
      return Buffer_Region_Byte (Lexer.Source);
   end Buffer_Region_Byte;

   function File_Name (Lexer : in Instance) return String
   is begin
      return File_Name (Lexer.Source);
   end File_Name;

   procedure Begin_Pos
     (Lexer      : in     Instance;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   is begin
      Begin_Pos (Lexer.Source, Begin_Byte, Begin_Char, Begin_Line);
   end Begin_Pos;

   function Line_At_Byte_Pos
     (Lexer    : in Instance;
      Token    : in WisiToken.Lexer.Token;
      Byte_Pos : in Buffer_Pos)
     return Line_Number_Type
   is begin
      return Line_At_Byte_Pos
        (Instance'Class (Lexer), Token.Byte_Region, Byte_Pos, First_Line => Token.Line_Region.First);
   end Line_At_Byte_Pos;

   function Find_New_Line
     (Lexer  : in Instance;
      Region : in Buffer_Region)
     return Base_Buffer_Pos
   is begin
      return Find_New_Line (Lexer.Source, Region);
   end Find_New_Line;

   function Contains_New_Line
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos
   is begin
      return Contains_New_Line (Lexer.Source, Byte_Region, First);
   end Contains_New_Line;

   function Contains_New_Line
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos
   is begin
      if Instance'Class (Lexer).Can_Contain_New_Line (ID) then
         return Contains_New_Line (Lexer.Source, Byte_Region, First);
      else
         return Invalid_Buffer_Pos;
      end if;
   end Contains_New_Line;

   function New_Line_Count
     (Lexer : in Instance;
      Item  : in Token_Arrays.Vector)
     return Base_Line_Number_Type
   is
      Result : Base_Line_Number_Type := 0;
      Pos    : Base_Buffer_Pos;
   begin
      for Token of Item loop
         Pos := Lexer.Contains_New_Line (Token.ID, Token.Byte_Region, First => True);
         if Pos /= Invalid_Buffer_Pos then
            Result := @ + New_Line_Count (Lexer.Source, Token.Byte_Region);
         end if;
      end loop;
      return Result;
   end New_Line_Count;

   function Find_New_Line
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos)
     return Buffer_Pos
   is begin
      for I in To_Buffer_Index (Source, Start) .. Source.Buffer'Last loop
         if Source.Buffer (I) = ASCII.LF then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return From_Buffer_Index (Source, Source.Buffer'Last);
   end Find_New_Line;

   function Find_String_Or_New_Line
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos;
      Item   : in String)
     return Buffer_Pos
   is begin
      for I in To_Buffer_Index (Source, Start) .. Source.Buffer'Last loop
         if Source.Buffer (I) = ASCII.LF or
           ((I + Item'Length <= Source.Buffer'Last) and then
              Source.Buffer (I .. I + Item'Length - 1) = Item)
         then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return From_Buffer_Index (Source, Source.Buffer'Last); --  Implicit new_line at EOI
   end Find_String_Or_New_Line;

   function Find_String
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos;
      Item   : in String)
     return Buffer_Pos
   is begin
      for I in To_Buffer_Index (Source, Start) .. Source.Buffer'Last loop
         if (I + Item'Length <= Source.Buffer'Last) and then
           Source.Buffer (I .. I + Item'Length - 1) = Item
         then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return From_Buffer_Index (Source, Source.Buffer'Last); --  Implicit delimiter at EOI
   end Find_String;

   function Find_New_Line
     (Source : in WisiToken.Lexer.Source;
      Region : in Buffer_Region)
     return Base_Buffer_Pos
   is begin
      for I in To_Buffer_Index (Source, Region.First) .. To_Buffer_Index (Source, Region.Last) loop
         if Source.Buffer (I) = ASCII.LF then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return Invalid_Buffer_Pos;
   end Find_New_Line;

   function Find_String_Or_New_Line
     (Source : in WisiToken.Lexer.Source;
      Region : in Buffer_Region;
      Item   : in String)
     return Base_Buffer_Pos
   is
      Index_Last : constant Integer := To_Buffer_Index (Source, Region.Last);
   begin
      for I in To_Buffer_Index (Source, Region.First) .. Index_Last loop
         if Source.Buffer (I) = ASCII.LF or
           ((I + Item'Length - 1 <= Index_Last) and then
              Source.Buffer (I .. I + Item'Length - 1) = Item)
         then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return Invalid_Buffer_Pos;
   end Find_String_Or_New_Line;

   function Find_String
     (Source : in WisiToken.Lexer.Source;
      Region : in Buffer_Region;
      Item   : in String)
     return Base_Buffer_Pos
   is
      Index_Last : constant Integer := To_Buffer_Index (Source, Region.Last);
   begin
      for I in To_Buffer_Index (Source, Region.First) .. Index_Last loop
         if (I + Item'Length <= Index_Last) and then
           Source.Buffer (I .. I + Item'Length - 1) = Item
         then
            return From_Buffer_Index (Source, I);
         end if;
      end loop;
      return Invalid_Buffer_Pos;
   end Find_String;

   function Line_Begin_Char_Pos
     (Source : in WisiToken.Lexer.Source;
      Token  : in WisiToken.Lexer.Token;
      Line   : in Line_Number_Type)
     return Base_Buffer_Pos
   is
      Found_Line : Base_Line_Number_Type := Token.Line_Region.First;
   begin
      for I in To_Buffer_Index (Source, Token.Byte_Region.First) ..
        To_Buffer_Index (Source, Token.Byte_Region.Last)
      loop
         if Source.Buffer (I) = ASCII.LF then
            Found_Line := @ + 1;
            if Found_Line = Line then
               return Base_Buffer_Pos (I) + 1;
            end if;
         end if;
      end loop;
      return Invalid_Buffer_Pos;
   end Line_Begin_Char_Pos;

   function Get_Byte
     (Source : in WisiToken.Lexer.Source;
      Pos    : in Integer)
     return Character
   is begin
      return
        (case Source.Label is
         when String_Label => Source.Buffer (Pos),
         when File_Label => GNATCOLL.Mmap.Data (Source.Region)(Pos));
   end Get_Byte;

   function Line_At_Byte_Pos
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in WisiToken.Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return Line_Number_Type
   is
      Index_Pos : constant Integer := To_Buffer_Index (Source, Byte_Pos);
      Found_Line : Base_Line_Number_Type := First_Line;
   begin
      for I in To_Buffer_Index (Source, Byte_Region.First) ..
        To_Buffer_Index (Source, Byte_Region.Last)
      loop
         if Get_Byte (Source, I) = ASCII.LF then
            Found_Line := @ + 1;
         end if;

         if I = Index_Pos then
            return Found_Line;
         end if;
      end loop;
      raise SAL.Programmer_Error; -- precondition false.
   end Line_At_Byte_Pos;

   function Contains_New_Line
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos
   is
      First_Index : constant Integer := To_Buffer_Index (Source, Byte_Region.First);
   begin
      if First then
         for I in First_Index .. To_Buffer_Index (Source, Byte_Region.Last) loop
            if Get_Byte (Source, I) = ASCII.LF then
               return Byte_Region.First + Base_Buffer_Pos (I - First_Index);
            end if;
         end loop;
      else
         for I in reverse First_Index .. To_Buffer_Index (Source, Byte_Region.Last) loop
            if Get_Byte (Source, I) = ASCII.LF then
               return Byte_Region.First + Base_Buffer_Pos (I - First_Index);
            end if;
         end loop;
      end if;
      return Invalid_Buffer_Pos;
   end Contains_New_Line;

   function New_Line_Count
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in Buffer_Region)
     return Base_Line_Number_Type
   is begin
      return Count : Base_Line_Number_Type := 0 do
         for I in To_Buffer_Index (Source, Byte_Region.First) ..
           To_Buffer_Index (Source, Byte_Region.Last)
         loop
            if Get_Byte (Source, I) = ASCII.LF then
               Count := @ + 1;
            end if;
         end loop;
      end return;
   end New_Line_Count;

   procedure Finalize (Object : in out Source)
   is begin
      case Object.Label is
      when String_Label =>
         if not Object.User_Buffer then
            Ada.Strings.Unbounded.Free (Object.Buffer);
         end if;

      when File_Label =>
         GNATCOLL.Mmap.Free (Object.Region);
         GNATCOLL.Mmap.Close (Object.File);
      end case;

      Object.Buffer_Nominal_First_Byte := Buffer_Pos'First;
      Object.Buffer_Nominal_First_Char := Buffer_Pos'First;
      Object.Line_Nominal_First        := Line_Number_Type'First;
   end Finalize;

   function Has_Source (Object : in Source) return Boolean
   is
      use all type Ada.Strings.Unbounded.String_Access;
   begin
      case Object.Label is
      when String_Label =>
         return Object.Buffer /= null;

      when File_Label =>
         --  Mmap doesn't provice "Is_Open".
         return Object.Buffer_Nominal_First_Byte /= Invalid_Buffer_Pos;
      end case;
   end Has_Source;

   function Buffer_Region_Byte (Object : in Source) return Buffer_Region
   is begin
      case Object.Label is
      when String_Label =>
         return (Base_Buffer_Pos (Object.Buffer'First), Base_Buffer_Pos (Object.Buffer'Last));

      when File_Label =>
         return (Object.Buffer_Nominal_First_Byte,
                 Object.Buffer_Nominal_First_Byte + Base_Buffer_Pos (Object.Buffer_Last));
      end case;
   end Buffer_Region_Byte;

   function Buffer (Source : in Lexer.Source) return GNATCOLL.Mmap.Str_Access
   is
      use GNATCOLL.Mmap;
   begin
      case Source.Label is
      when String_Label =>
         return Short.To_Str_Access (GNAT.Strings.String_Access (Source.Buffer));

      when File_Label =>
         return Data (Source.Region);
      end case;

   end Buffer;

   function File_Name (Source : in Lexer.Source) return String
   is begin
      return -Source.File_Name;
   end File_Name;

   function To_Char_Pos (Source : in Lexer.Source; Lexer_Char_Pos : in Integer) return Base_Buffer_Pos
   is begin
      return Base_Buffer_Pos (Lexer_Char_Pos) + Source.Buffer_Nominal_First_Char - Buffer_Pos'First;
   end To_Char_Pos;

end WisiToken.Lexer;
