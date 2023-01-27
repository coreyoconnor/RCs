--  Abstract :
--
--  An abstract lexer interface.
--
--  Copyright (C) 2014 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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

pragma Warnings (Off, "license of withed unit ""GNATCOLL.Mmap"" may be inconsistent");

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package WisiToken.Lexer is

   type Token is record
      --  Information provided by the lexer.

      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Index into the Lexer buffer for the token text.

      Char_Region : Buffer_Region := Null_Buffer_Region;
      --  Character position, useful for finding the token location in Emacs
      --  buffers.

      Line_Region : WisiToken.Line_Region := Null_Line_Region;
      --  SOI, EOI tokens have 0 length for Byte_Region and Char_Region, and
      --  0 Line_Length_Count for Line_Region.
      --
      --  SOI.Byte_Region.First = first byte of first character in text
      --  SOI.Char_Region.First = first character in text
      --  SOI.Line_Region.First = first line in text,
      --
      --  SOI may not be Buffer_Pos'First and Line_Number_Type'First if parsing part of a file.
      --
      --  EOI.Byte_Region.First = Byte position of EOI character; if not
      --  actually present, one after the last byte in the text.
      --  EOI.Char_Region.First = Character position of EOI character.
      --  EOI.Line_Region.First = last line in file (after final new_line).
   end record;

   function Column (Token : in Lexer.Token; Line_Begin_Char_Pos : in Buffer_Pos) return Ada.Text_IO.Count;

   function Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  ID, Char_Region; Line_Region if New_Line

   function Full_Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  All fields.

   Invalid_Token : constant Token := (others => <>);

   procedure Shift
     (Token       : in out Lexer.Token;
      Shift_Bytes : in     Base_Buffer_Pos;
      Shift_Chars : in     Base_Buffer_Pos;
      Shift_Lines : in     Base_Line_Number_Type);
   --  Add Shift_* to corresponding regions.

   package Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Index_Type, Token, Default_Element => (others => <>));

   function Image is new Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Trimmed_Image, Image);
   function Full_Image is new Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Trimmed_Image, Full_Image);

   subtype Recover_Characters is String (1 .. 4);

   type Error is record
      Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      --  Character at that position is not recognized as part of a token.

      Recover_Char : Recover_Characters := (others => ASCII.NUL);
      --  If the error was corrected, the character (in UTF-8 encoding) that
      --  was inserted; unused trailing bytes set to ASCII.NUL. Otherwise,
      --  all ASCII.Nul.
   end record;

   function To_String (Item : in Recover_Characters) return String;
   --  Item must be Recover_Char from an error; delete the trailing NULs.

   package Error_Lists is new Ada.Containers.Doubly_Linked_Lists (Error);

   type Source_Labels is (String_Label, File_Label);

   type Source (Label : Source_Labels := Source_Labels'First) is private;

   type Instance
   is abstract new Ada.Finalization.Limited_Controlled with record
      Trace      : WisiToken.Trace_Access;
      Descriptor : WisiToken.Descriptor_Access_Constant;
      Errors     : Error_Lists.List;
      Source     : Lexer.Source;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function Has_Source (Lexer : access constant Instance) return Boolean;
   --  True if one of Reset_* has been called; lexer has source to process.

   procedure Set_Verbosity
     (Lexer     : in Instance;
      Verbosity : in Integer)
   is null;

   procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from Input.

   procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      Input_Last : in     Integer;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from Input (Input'First
   --  .. Input_Last). Input'First is Begin_Byte. File_Name is used for
   --  error messages.

   procedure Reset_With_File
     (Lexer      : in out Instance;
      File_Name  : in     String;
      Begin_Byte : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from File_Name. If
   --  Begin_Pos, End_Pos /= Invalid_Buffer_Pos, only parse that portion
   --  of the file.
   --
   --  Raises Ada.IO_Exceptions.Name_Error if File_Name cannot be opened.

   procedure Reset (Lexer : in out Instance) is abstract
   with Pre'Class => Lexer.Has_Source,
     Post'Class => Lexer.Has_Source;
   --  Reset Lexer, read from previous source.

   procedure Discard_Rest_Of_Input (Lexer : in out Instance) is abstract;
   --  If reading input from a stream, abort reading (or force it to
   --  complete); Find_Next will not be called before another Reset.

   function Buffer_Region_Byte (Lexer : in Instance) return Buffer_Region;

   function Buffer_Text (Lexer : in Instance; Byte_Region : in Buffer_Region) return String;
   --  Return text from internal buffer, given region in byte position.

   procedure Set_Position
     (Lexer         : in out Instance;
      Byte_Position : in     Buffer_Pos;
      Char_Position : in     Buffer_Pos;
      Line          : in     Line_Number_Type)
     is abstract;
   --  Set the current position in the source buffer; Find_Next will
   --  start there. Prev_Token_ID should be Descriptor.New_Line_ID or
   --  Invalid_Token_ID; it is used for First.

   function Find_Next
     (Lexer : in out Instance;
      Token :    out WisiToken.Lexer.Token)
     return Natural is abstract;
   --  Set Token to the next token from the input stream, return number
   --  of lexer errors encountered.
   --
   --  For each lexer error, adds an entry to Lexer.Errors. Unrecognized
   --  characters are skipped; missing quotes are inserted at the found
   --  quote. If the recovery inserted a missing quote, it is the last
   --  entry in Errors, the returned token is an empty string literal.
   --
   --  Token.Char_Region, Token.Byte_Region are the character and byte
   --  position of the start and end of Token, in the internal buffer,
   --  1-indexed. Char_Region and Byte_Region differ when text is UTF-8
   --  or other multi-byte encoding, and when line endings are two byte.
   --
   --  Token.Line_Region is the line number at the start and end of the token.
   --  If the underlying text feeder does not support the notion of
   --  'line', this is Null_Line_Region.
   --
   --  test_incremental.adb Lexer_Errors_05 has multiple lexer errors on
   --  one token.

   function File_Name (Lexer : in Instance) return String;
   --  Return input file name; empty string if there is no file.

   procedure Begin_Pos
     (Lexer      : in     Instance;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   with Pre => Lexer.Has_Source;
   --  Return values from Reset*.

   function Is_Block_Delimited
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is abstract;
   --  True if ID is a token that spans a region of text defined by
   --  delimiters; a string, a comment, or some similar delimited text.
   --
   --  Incremental parse uses this and the following related functions to
   --  determine how much text is affected by an edit that inserts or
   --  deletes a delimiter.

   function Same_Block_Delimiters
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is abstract;
   --  True if Is_Block_Delimited (ID) and the start and end delimiters
   --  are the same (typically true for strings, false for comments).

   function Escape_Delimiter_Doubled
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is abstract
   with Pre'Class => Lexer.Is_Block_Delimited (ID) and Lexer.Same_Block_Delimiters (ID);
   --  True if a delimiter embedded in the token is escaped by doubling
   --  it (like Ada strings).

   function Start_Delimiter_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer
   is abstract
   with Pre'Class => Lexer.Is_Block_Delimited (ID);
   --  Return length in bytes of the characters in the start delimiter character sequence.

   function End_Delimiter_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer
   is abstract
   with Pre'Class => Lexer.Is_Block_Delimited (ID);
   --  Return length in bytes of the characters in the end delimiter character sequence.

   function New_Line_Is_End_Delimiter
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is abstract
   with Pre'Class => Is_Block_Delimited (Lexer, ID);

   function Find_End_Delimiter
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Token_Start : in Buffer_Pos)
     return Buffer_Pos
   is abstract
   with Pre'Class => Is_Block_Delimited (Lexer, ID);
   --  Given the byte position of a start delimiter, return the byte
   --  position of the corresponding end delimiter.
   --
   --  If no end delimiter is found, returns EOI position.

   function Contains_End_Delimiter
     (Lexer  : in Instance;
      ID     : in Token_ID;
      Region : in Buffer_Region)
     return Base_Buffer_Pos
   is abstract
   with Pre'Class => Is_Block_Delimited (Lexer, ID);
   --  If Region contains an end delimiter for ID, return the buffer
   --  position of the start of that delimiter. Otherwise return
   --  Invalid_Buffer_Pos. Does not check for matching or nested start.

   function Find_Scan_End
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Byte_Region : in Buffer_Region;
      Inserted    : in Boolean;
      Start       : in Boolean)
     return Buffer_Pos
   is abstract
   with Pre'Class => Is_Block_Delimited (Lexer, ID);
   --  If Inserted, a delimiter for ID (if Start, a start delimiter, else
   --  an end delimeter) was inserted at Byte_Region.First, and
   --  Byte_Region.Last is the previous end of the token, shifted to
   --  match the current edited text. .
   --
   --  If not Inserted, a delimeter was deleted. If Start, Byte_Region is
   --  where in the current text to start searching for a start delmiter
   --  (nominally the old start position). If not Start,
   --
   --  Text was either hidden in the new token, or exposed as code;
   --  return the end of the buffer region that must be scanned by the
   --  lexer.

   function Find_New_Line
     (Lexer  : in Instance;
      Region : in Buffer_Region)
     return Base_Buffer_Pos;
   --  Returns Invalid_Bufer_Pos if not found in Region.

   function Line_Begin_Char_Pos
     (Lexer : in Instance;
      Token : in WisiToken.Lexer.Token;
      Line  : in Line_Number_Type)
     return Base_Buffer_Pos
   is abstract
   with Pre'Class => Token.Line_Region.First <=  Line - 1 and Token.Line_Region.Last >= Line;
   --  Return first char position on Line; Invalid_Buffer_Pos if Token
   --  does not contain the new-line that starts Line.

   function Line_At_Byte_Pos
     (Lexer       : in Instance;
      Byte_Region : in WisiToken.Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return Line_Number_Type
   is abstract
   with Pre'Class => Contains (Byte_Region, Byte_Pos);
   --  Return line that contains Byte_Pos. If Byte_Pos is on a New_Line,
   --  result is the line that the character ends. First_Line must be the
   --  line number at Byte_Region.First.

   function Line_At_Byte_Pos
     (Lexer    : in Instance;
      Token    : in WisiToken.Lexer.Token;
      Byte_Pos : in Buffer_Pos)
     return Line_Number_Type
   with Pre => Contains (Token.Byte_Region, Byte_Pos);
   --  Return line that contains Byte_Pos. If Byte_Pos is on a New_Line,
   --  result is the line that the character ends.

   function Can_Contain_New_Line
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean is abstract;

   function Contains_New_Line
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos;
   --  Returns the first or last new_line in Byte_Region;
   --  Invalid_Buffer_Pos if none.

   function Contains_New_Line
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos;
   --  If ID cannot contain a new_line, returns Invalid_Buffer_Pos.
   --  Otherwise, finds the first or last new_line in Byte_Region (which
   --  must be the token byte_region); Invalid_Buffer_Pos if none.

   function New_Line_Count
     (Lexer : in Instance;
      Item  : in Token_Arrays.Vector)
     return Base_Line_Number_Type;

   function Terminated_By_New_Line
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean is abstract;
   --  True for tokens that can be terminated by new-line.
   --
   --  Returns false for comment-one-line; terminating that by a new_line
   --  is actually an error.

   function Buffer_Region_Byte (Object : in Source) return Buffer_Region;

   function Find_New_Line
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos)
     return Buffer_Pos;

   function Find_New_Line
     (Source : in WisiToken.Lexer.Source;
      Region : in Buffer_Region)
     return Base_Buffer_Pos;
   --  Returns Invalid_Bufer_Pos if not found in Region.

   function Find_String_Or_New_Line
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos;
      Item   : in String)
     return Buffer_Pos;
   --  Returns last byte in Source if not found, for an implicit New_Line
   --  at EOI.

   function Find_String
     (Source : in WisiToken.Lexer.Source;
      Start  : in Buffer_Pos;
      Item   : in String)
     return Buffer_Pos;
   --  Returns last byte in Source if not found, for an implicit delimiter
   --  at EOI.

   function Find_String_Or_New_Line
     (Source : in WisiToken.Lexer.Source;
      Region : in Buffer_Region;
      Item   : in String)
     return Base_Buffer_Pos;
   --  Returns Invalid_Bufer_Pos if not found in Region.

   function Find_String
     (Source : in WisiToken.Lexer.Source;
      Region  : in Buffer_Region;
      Item   : in String)
     return Base_Buffer_Pos;
   --  Returns Invalid_Bufer_Pos if not found in Region.

   function Line_Begin_Char_Pos
     (Source : in WisiToken.Lexer.Source;
      Token  : in WisiToken.Lexer.Token;
      Line   : in Line_Number_Type)
     return Base_Buffer_Pos;
   --  Implement Line_Begin_Char_Pos (Lexer ...)

   function Line_At_Byte_Pos
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in WisiToken.Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return Line_Number_Type;
   --  Implement Line_At_Byte_Pos (Lexer ...)

   function Contains_New_Line
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in Buffer_Region;
      First       : in Boolean)
     return Base_Buffer_Pos;

   function New_Line_Count
     (Source      : in WisiToken.Lexer.Source;
      Byte_Region : in Buffer_Region)
     return Base_Line_Number_Type;

private

   type Source (Label : Source_Labels := Source_Labels'First) is record
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Not saved in Mapped_File, may be empty for String_Label

      Buffer_Nominal_First_Byte : Buffer_Pos       := Buffer_Pos'First;
      Buffer_Nominal_First_Char : Buffer_Pos       := Buffer_Pos'First;
      Line_Nominal_First        : Line_Number_Type := Line_Number_Type'First;
      Buffer_Last               : Natural          := 0; -- allow empty input string

      case Label is
      when String_Label =>
         Buffer      : Ada.Strings.Unbounded.String_Access;
         User_Buffer : Boolean := False;
         --  If User_Buffer is True, user provided buffer and will deallocate
         --  it. Otherwise we must deallocate it.

         --  Buffer_Nominal_First, Line_Nominal_First are 1.

      when File_Label =>

         --  The input is memory mapped from the following, which must be closed:
         File        : GNATCOLL.Mmap.Mapped_File;
         Region      : GNATCOLL.Mmap.Mapped_Region;
         --  Region always has first character at offset 0.

         --  Buffer_Nominal_First is Begin_Pos. Line_Nominal_First is
         --  Begin_Line.
      end case;
   end record;

   procedure Finalize (Object : in out Source);

   function Has_Source (Object : in Source) return Boolean;
   --  True if one of Reset_* has been called; lexer has source to process.

   function Buffer (Source : in Lexer.Source) return GNATCOLL.Mmap.Str_Access;
   --  If Source_Label is String_label, actual bounds are
   --  Source.Buffer'First, 'Last. Otherwise, The bounds on the result
   --  are not present; 'First, 'Last are not reliable. actual bounds are
   --  1 .. Source.Buffer_Last. Indexing is reliable.

   function To_Buffer_Index (Source : in WisiToken.Lexer.Source; Byte_Pos : in Base_Buffer_Pos) return Integer
   is (Integer (Byte_Pos - Source.Buffer_Nominal_First_Byte) +
                  (case Source.Label is
                   when String_Label => Source.Buffer'First,
                   when File_Label => Integer (Buffer_Pos'First)));

   function From_Buffer_Index (Source : in WisiToken.Lexer.Source; Index : in Integer) return Base_Buffer_Pos
   is (Base_Buffer_Pos (Index) + Source.Buffer_Nominal_First_Byte -
         (case Source.Label is
          when String_Label => Base_Buffer_Pos (Source.Buffer'First),
          when File_Label => Buffer_Pos'First));

   function File_Name (Source : in Lexer.Source) return String;

   function To_Char_Pos (Source : in Lexer.Source; Lexer_Char_Pos : in Integer) return Base_Buffer_Pos;

   procedure Begin_Pos
     (Object     : in     Source;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type);

end WisiToken.Lexer;
