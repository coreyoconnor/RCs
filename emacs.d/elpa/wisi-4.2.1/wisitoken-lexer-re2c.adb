--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package body WisiToken.Lexer.re2c is

   overriding procedure Finalize (Object : in out Instance)
   is
      use all type System.Address;
   begin
      if Object.Lexer /= System.Null_Address then
         Free_Lexer (Object.Lexer);
         Object.Lexer := System.Null_Address;
      end if;

      Finalize (Object.Source);
   end Finalize;

   type Instance_Access is access Instance; --  silence compiler warning

   function New_Lexer
     (Trace      : in WisiToken.Trace_Access;
      Descriptor  : in WisiToken.Descriptor_Access_Constant)
     return Handle
   is
      Result : constant Instance_Access := new Instance;
   begin
      Result.Trace      := Trace;
      Result.Descriptor := Descriptor;
      return Handle (Result);
   end New_Lexer;

   overriding procedure Set_Verbosity
     (Lexer     : in Instance;
      Verbosity : in Integer)
   is
      use all type System.Address;
   begin
      --  Allow calling this before the lexer is configured, for
      --  run_emacs_common_parse.adb.
      if Lexer.Lexer /= System.Null_Address then
         Set_Verbosity (Lexer.Lexer, Interfaces.C.int (Verbosity));
      end if;
   end Set_Verbosity;

   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label                     => String_Label,
         File_Name                 => +"",
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer                    => new String'(Input),
         Buffer_Last               => Input'Last,
         User_Buffer               => False);

      Lexer.Lexer := New_Lexer
        (Buffer => Lexer.Source.Buffer.all'Address,
         Length => Interfaces.C.size_t (Input'Length));

      Reset (Lexer);
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      Input_Last : in     Integer;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is
      function Short_File_Name return Ada.Strings.Unbounded.Unbounded_String
      is
         use Ada.Strings.Unbounded;
      begin
         if Length (File_Name) = 0 then
            return +"";
         else
            return +Ada.Directories.Simple_Name (-File_Name);
         end if;
      exception
      when Ada.IO_Exceptions.Name_Error =>
         --  Probably an editor temp buffer name.,,
         return File_Name;
      end Short_File_Name;
   begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label                     => String_Label,
         File_Name                 => Short_File_Name,
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer                    => Input,
         Buffer_Last               => Input_Last,
         User_Buffer               => True);

      Lexer.Lexer := New_Lexer
        (Buffer => Lexer.Source.Buffer.all'Address,
         Length => Interfaces.C.size_t (Input_Last - Input'First + 1));

      Reset (Lexer);
   end Reset_With_String_Access;

   overriding procedure Reset_With_File
     (Lexer          : in out Instance;
      File_Name      : in     String;
      Begin_Byte_Pos : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte_Pos   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char     : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line     : in     Line_Number_Type := Line_Number_Type'First)
   is
      use GNATCOLL.Mmap;
      Length : Base_Buffer_Pos; -- 0 in empty file
   begin
      Finalize (Lexer);

      --  We assume the file is in UTF-8 encoding
      Lexer.Source :=
        (File_Label, +Ada.Directories.Simple_Name (File_Name),
         Buffer_Nominal_First_Byte => Buffer_Pos'First, -- overwritten below,
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Line_Number_Type'First, -- overwritten below
         File                      => Open_Read (File_Name),
         Region                    => Invalid_Mapped_Region,
         Buffer_Last               => 1);

      if Begin_Byte_Pos = Invalid_Buffer_Pos then
         Lexer.Source.Region := Read (Lexer.Source.File);
         Length              := Base_Buffer_Pos (Last (Lexer.Source.Region));
      else
         Length := End_Byte_Pos - Begin_Byte_Pos + 1;

         Lexer.Source.Buffer_Nominal_First_Byte := Begin_Byte_Pos;
         Lexer.Source.Line_Nominal_First        := Begin_Line;

         Lexer.Source.Region := Read
           (Lexer.Source.File,
            Offset => File_Size (Begin_Byte_Pos - 1), -- Offset is 0 indexed, Begin_Byte_Pos is 1 indexed
            Length => File_Size (Length));
      end if;

      Lexer.Source.Buffer_Last := Last (Lexer.Source.Region);

      Lexer.Lexer := New_Lexer
        (Buffer => Data (Lexer.Source.Region).all'Address,
         Length => Interfaces.C.size_t (Length));

      Reset (Lexer);
   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Instance)
   is begin
      Reset_Lexer (Lexer.Lexer);
   end Reset;

   overriding function Find_Next
     (Lexer : in out Instance;
      Token :    out WisiToken.Lexer.Token)
     return Natural
   is
      use Interfaces.C;

      ID            : Token_ID;
      Byte_Position : Natural;
      Byte_Length   : Natural;
      Char_Position : Natural;
      Char_Length   : Natural;
      Line_Start    : Line_Number_Type;
      Line_Length   : Base_Line_Number_Type;
      Error_Count   : Integer := 0;

      procedure Build_Token
      is begin
         Token :=
           (ID => ID,

            Byte_Region =>
              (if ID = Lexer.Descriptor.EOI_ID and then Byte_Position = 0 then
                 --  EOI in empty buffer
                 (Lexer.Source.Buffer_Nominal_First_Byte,
                  Lexer.Source.Buffer_Nominal_First_Byte - 1)
               else
                 (Base_Buffer_Pos (Byte_Position) + Lexer.Source.Buffer_Nominal_First_Byte - Buffer_Pos'First,
                  Base_Buffer_Pos (Byte_Position + Byte_Length - 1) +
                    Lexer.Source.Buffer_Nominal_First_Byte - Buffer_Pos'First)),

            Line_Region =>
              (First    => Line_Start + Lexer.Source.Line_Nominal_First - Line_Number_Type'First,
               Last     => Line_Start + Line_Length + Lexer.Source.Line_Nominal_First - Line_Number_Type'First),

            Char_Region =>
              (if ID = Lexer.Descriptor.EOI_ID and then Byte_Position = Integer (Base_Buffer_Pos'First)
               then
                 --  EOI in empty buffer
                 (Lexer.Source.Buffer_Nominal_First_Byte,
                  Lexer.Source.Buffer_Nominal_First_Byte - 1)
               else
                 (To_Char_Pos (Lexer.Source, Char_Position),
                  To_Char_Pos (Lexer.Source, Char_Position + Char_Length - 1))));
      end Build_Token;

   begin
      loop
         declare
            Status : constant int := Next_Token
              (Lexer.Lexer, ID,
               Byte_Position => Interfaces.C.size_t (Byte_Position),
               Byte_Length   => Interfaces.C.size_t (Byte_Length),
               Char_Position => Interfaces.C.size_t (Char_Position),
               Char_Length   => Interfaces.C.size_t (Char_Length),
               Line_Start    => Interfaces.C.int (Line_Start),
               Line_Length   => Interfaces.C.int (Line_Length));
         begin
            case Status is
            when 0 =>
               Build_Token;
               return Error_Count;

            when 1 =>
               --  Unrecognized character from lexer. Handle missing quotes by
               --  inserting a virtual quote at the existing quote, and telling the
               --  lexer to skip the char.
               Error_Count := @ + 1;
               declare
                  Buffer : constant GNATCOLL.Mmap.Str_Access := WisiToken.Lexer.Buffer (Lexer.Source);
               begin
                  if Trace_Lexer > Outline then
                     --  We don't have a visible Trace object here.
                     Ada.Text_IO.Put_Line ("lexer error char " & Buffer (Byte_Position));
                  end if;

                  if Lexer.Descriptor.String_1_ID /= Invalid_Token_ID and Buffer (Byte_Position) = ''' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after '.
                     Lexer.Errors.Append
                       ((To_Char_Pos (Lexer.Source, Char_Position),
                         (1 => ''', others => ASCII.NUL)));

                     ID := Lexer.Descriptor.String_1_ID;
                     Build_Token;
                     return Error_Count;

                  elsif Lexer.Descriptor.String_2_ID /= Invalid_Token_ID and Buffer (Byte_Position) = '"' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after ".
                     Lexer.Errors.Append
                       ((Char_Pos     => To_Char_Pos (Lexer.Source, Char_Position),
                         Recover_Char =>  (1 => '"', others => ASCII.NUL)));

                     ID := Lexer.Descriptor.String_2_ID;
                     Build_Token;
                     return Error_Count;

                  else
                     --  Just skip the character; call Next_Token again.
                     Lexer.Errors.Append
                       ((To_Char_Pos (Lexer.Source, Char_Position), (others => ASCII.NUL)));
                  end if;
               end;

            when others =>
               raise Fatal_Error with " lexer returned unrecognized status code" & int'Image (Status);
            end case;
         end;
      end loop;
   end Find_Next;

   overriding procedure Set_Position
     (Lexer         : in out Instance;
      Byte_Position : in     Buffer_Pos;
      Char_Position : in     Buffer_Pos;
      Line          : in     Line_Number_Type)
   is begin
      --  FIXME: respect partial parse lexer.source.*_Nominal_first_*. only needed if doing incremental after partial.
      Set_Position
        (Lexer.Lexer,
         Byte_Position => Interfaces.C.size_t (Byte_Position),
         Char_Position => Interfaces.C.size_t (Char_Position),
         Line          => Interfaces.C.int (Line));
   end Set_Position;

   overriding
   function Is_Block_Delimited
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return Is_Block_Delimited (ID);
   end Is_Block_Delimited;

   overriding
   function Same_Block_Delimiters
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return Same_Block_Delimiters (ID);
   end Same_Block_Delimiters;

   overriding
   function Escape_Delimiter_Doubled
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return Escape_Delimiter_Doubled (ID);
   end Escape_Delimiter_Doubled;

   overriding
   function Start_Delimiter_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer
   is begin
      return Start_Delimiter_Length (ID);
   end Start_Delimiter_Length;

   overriding
   function End_Delimiter_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer
   is begin
      return End_Delimiter_Length (ID);
   end End_Delimiter_Length;

   overriding
   function New_Line_Is_End_Delimiter
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return New_Line_Is_End_Delimiter (ID);
   end New_Line_Is_End_Delimiter;

   overriding
   function Find_End_Delimiter
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Token_Start : in Buffer_Pos)
     return Buffer_Pos
   is begin
      return Find_End_Delimiter (Lexer.Source, ID, Token_Start);
   end Find_End_Delimiter;

   overriding
   function Contains_End_Delimiter
     (Lexer         : in Instance;
      ID            : in Token_ID;
      Region : in Buffer_Region)
     return Base_Buffer_Pos
   is begin
      return Contains_End_Delimiter (Lexer.Source, ID, Region);
   end Contains_End_Delimiter;

   overriding
   function Find_Scan_End
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Byte_Region : in Buffer_Region;
      Inserted    : in Boolean;
      Start       : in Boolean)
     return Buffer_Pos
   is begin
      return Find_Scan_End (Lexer.Source, ID, Byte_Region, Inserted, Start);
   end Find_Scan_End;

   overriding
   function Line_Begin_Char_Pos
     (Lexer : in Instance;
      Token : in WisiToken.Lexer.Token;
      Line  : in Line_Number_Type)
     return Base_Buffer_Pos
   is begin
      if Token.ID = Lexer.Descriptor.New_Line_ID then
         return Token.Char_Region.First + 1;
      else
         return Line_Begin_Char_Pos (Lexer.Source, Token, Line);
      end if;
   end Line_Begin_Char_Pos;

   overriding
   function Line_At_Byte_Pos
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return Line_Number_Type
   is begin
      return Line_At_Byte_Pos (Lexer.Source, Byte_Region, Byte_Pos, First_Line);
   end Line_At_Byte_Pos;

   overriding
   function Can_Contain_New_Line
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return Can_Contain_New_Line (ID);
   end Can_Contain_New_Line;

   overriding
   function Terminated_By_New_Line
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is begin
      return Terminated_By_New_Line (ID);
   end Terminated_By_New_Line;

end WisiToken.Lexer.re2c;
