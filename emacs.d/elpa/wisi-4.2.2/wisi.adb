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

with Ada.Directories;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with SAL;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with WisiToken.Lexer;
package body Wisi is
   use WisiToken;

   Chars_Per_Int : constant Integer := Integer'Width;

   ----------
   --  body subprograms bodies, alphabetical

   function Image (Indent : in Indent_Type) return String
   is
      Prefix : constant String := "(" & Trimmed_Image (Indent.Controlling_Token_Line) & ": " &
        Indent_Label'Image (Indent.Label);
   begin
      case Indent.Label is
      when Not_Set =>
         return Prefix & ")";

      when Int =>
         return Prefix & Integer'Image (Indent.Int_Indent) & ")";

      when Anchored =>
         return Prefix & "," & Indent.Anchor_Line'Image & "," & Indent.Anchor_Delta'Image & ")";

      end case;
   end Image;

   procedure Indent_Apply_Anchored
     (Delta_Indent : in     Simple_Delta_Type;
      Indent       : in out Indent_Type)
   with Pre => Delta_Indent.Label = Anchored
   is begin
      case Indent.Label is
      when Not_Set =>
         Indent :=
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchor_Line, Delta_Indent.Anchored_Delta);

      when Int =>
         Indent :=
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchor_Line,
            Delta_Indent.Anchored_Delta + Indent.Int_Indent);

      when Anchored =>
         --  Already anchored, as in nested parens.
         null;
      end case;
   end Indent_Apply_Anchored;

   procedure Indent_Apply_Int
     (Indent                 : in out Indent_Type;
      Offset                 : in     Integer;
      Controlling_Token_Line : in     Base_Line_Number_Type)
   is begin
      --  Add an Int indent to Indent
      case Indent.Label is
      when Not_Set =>
         Indent := (Int, Controlling_Token_Line, Offset);

      when Int =>
         if Controlling_Token_Line = Invalid_Line_Number or
           Indent.Controlling_Token_Line = Invalid_Line_Number or
           Controlling_Token_Line /= Indent.Controlling_Token_Line
         then
            Indent.Controlling_Token_Line := Controlling_Token_Line;
            Indent.Int_Indent := Indent.Int_Indent + Offset;
         end if;

      when Anchored =>
         null;
      end case;
   end Indent_Apply_Int;

   procedure Indent_Line
     (Data              : in out Parse_Data_Type;
      Line              : in     Line_Number_Type;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Indenting_Comment_Label;
      Trace             : in     WisiToken.Trace_Access)
   is
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent : Indent_Type := Data.Indents (Line);
   begin
      case Delta_Indent.Label is
      when Simple =>
         case Delta_Indent.Simple_Delta.Label is
         when None =>
            null;

         when Int =>
            Indent_Apply_Int
              (Indent, Delta_Indent.Simple_Delta.Int_Delta, Delta_Indent.Simple_Delta.Controlling_Token_Line);

         when Anchored =>
            Indent_Apply_Anchored (Delta_Indent.Simple_Delta, Indent);
         end case;

      when Hanging =>
         declare
            procedure Apply_Delta_1
            is begin
               case Delta_Indent.Hanging_Delta_1.Label is
               when None =>
                  null;
               when Int =>
                  Indent_Apply_Int
                    (Indent, Delta_Indent.Hanging_Delta_1.Int_Delta,
                     Delta_Indent.Hanging_Delta_1.Controlling_Token_Line);
               when Anchored =>
                  Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_1, Indent);
               end case;
            end Apply_Delta_1;

            procedure Apply_Delta_2
            is begin
               case Delta_Indent.Hanging_Delta_2.Label is
               when None =>
                  null;
               when Int =>
                  Indent_Apply_Int
                    (Indent, Delta_Indent.Hanging_Delta_2.Int_Delta,
                     Delta_Indent.Hanging_Delta_2.Controlling_Token_Line);
               when Anchored =>
                  Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_2, Indent);
               end case;
            end Apply_Delta_2;

         begin
            case Indenting_Comment is
            when None =>
               if Line = Delta_Indent.Hanging_First_Line then
                  Apply_Delta_1;
               else
                  Apply_Delta_2;
               end if;

            when Leading =>
               Apply_Delta_1;

            when Trailing =>
               Apply_Delta_2;
            end case;
         end;
      end case;
      if Trace_Action > Extra then
         Trace.Put_Line ("indent_line: " & Line_Number_Type'Image (Line) & " => " & Image (Indent));
      end if;

      Data.Indents.Replace_Element (Line, Indent);
   end Indent_Line;

   function Paren_In_Anchor_Line
     (Data         : in out Parse_Data_Type'Class;
      Tree         : in     WisiToken.Syntax_Trees.Tree;
      Anchor_Token : in     Syntax_Trees.Valid_Node_Access;
      Offset       : in     Integer)
     return Integer
   --  If there is a left_paren in Anchor_Token.Line_Region.First
   --  containing Anchor_Token, return offset of that paren from first
   --  char in line + Offset. Else return Offset.
   is
      Left_Paren_ID  : Token_ID renames Data.Left_Paren_ID;
      Right_Paren_ID : Token_ID renames Data.Right_Paren_ID;

      Begin_Token : constant Syntax_Trees.Valid_Node_Access := Tree.Line_Begin_Token
        (Tree.Line_Region (Anchor_Token, Trailing_Non_Grammar => True).First);

      I : Syntax_Trees.Node_Access := Tree.First_Terminal (Anchor_Token);

      Paren_Count    : Integer    := 0;
      Paren_Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      Text_Begin_Pos : Buffer_Pos := Invalid_Buffer_Pos;
   begin
      loop
         declare
            Tok_Char_Region : constant Buffer_Region := Tree.Char_Region (I, Trailing_Non_Grammar => False);
         begin
            if Tree.ID (I) = Left_Paren_ID then
               Paren_Count := Paren_Count + 1;
               if Paren_Count = 1 then
                  Paren_Char_Pos := Tok_Char_Region.First;
               end if;

            elsif Tree.ID (I) = Right_Paren_ID then
               Paren_Count := Paren_Count - 1;

            end if;

            if I = Begin_Token then
               Text_Begin_Pos := Tok_Char_Region.First;
               exit;
            end if;
         end;

         I := Tree.Prev_Terminal (I);
      end loop;

      if Paren_Char_Pos /= Invalid_Buffer_Pos and Text_Begin_Pos /= Invalid_Buffer_Pos then
         return 1 + Offset + Integer (Paren_Char_Pos - Text_Begin_Pos);
      else
         return Offset;
      end if;
   end Paren_In_Anchor_Line;

   procedure Put (Cache : in Navigate_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 11 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");

      procedure Append (Item : in Nil_Buffer_Pos)
      is begin
         if Item.Set then
            Append (Line, Buffer_Pos'Image (Item.Item));
         else
            Append (Line, " -1");
         end if;
      end Append;
   begin
      Append (Line, Navigate_Cache_Code);
      Append (Line, Buffer_Pos'Image (Cache.Pos));
      Append (Line, Token_ID'Image (Cache.Statement_ID));
      Append (Line, Token_ID'Image (Cache.ID));
      Append (Line, Integer'Image (Cache.Length));
      Append (Line, Integer'Image (Navigate_Class_Type'Pos (Cache.Class)));
      Append (Cache.Containing_Pos);
      Append (Cache.Prev_Pos);
      Append (Cache.Next_Pos);
      Append (Cache.End_Pos);
      Append (Line, ']');
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Put (Cache : in WisiToken.Buffer_Region)
   is begin
      Ada.Text_IO.Put_Line
        ("[" & Name_Property_Code & Buffer_Pos'Image (Cache.First) & Buffer_Pos'Image (Cache.Last) & "]");
   end Put;

   procedure Put (Cache : in Face_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 4 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");
   begin
      if Cache.Face.Set then
         Append (Line, Face_Property_Code);
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.First));
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.Last));
         Append (Line, Integer'Image (Cache.Face.Item));
         Append (Line, ']');
         Ada.Text_IO.Put_Line (To_String (Line));
      end if;
   end Put;

   procedure Put
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type;
      Item : in Indent_Type)
   is begin
      --  All Anchors must be resolved at this point, but not all lines have
      --  an indent computed. A negative indent is an error in either the
      --  grammar indent rules or the algorithms in this package.
      case Item.Label is
      when Not_Set =>
         --  Especially with partial parse, we have no idea what this indent should be.
         null;

      when Int =>
         declare
            --  We can easily get negative indents when there are syntax errors.
            Ind : constant Integer := Integer'Max (0, Item.Int_Indent);
            Line_Begin_Char_Pos : Base_Buffer_Pos :=
              (if Line = Line_Number_Type'First
               then Buffer_Pos'First
               else Invalid_Buffer_Pos);
            Node : constant Syntax_Trees.Node_Access :=
              (if Line = Line_Number_Type'First
               then Syntax_Trees.Invalid_Node_Access
               else Tree.Find_New_Line (Line, Line_Begin_Char_Pos));
            pragma Unreferenced (Node);
         begin
            if Debug_Mode then
               if Ind > 100 then
                  --  This is better than hanging Emacs by returning a huge bogus indent.
                  raise SAL.Programmer_Error with "indent > 100";
               elsif Line_Begin_Char_Pos = Invalid_Buffer_Pos then
                  raise SAL.Programmer_Error with "Line_Begin_Char_Pos = Invalid_Buffer_Pos, line" & Line'Image;
               end if;
            end if;
            Ada.Text_IO.Put_Line
              --  elisp doesn't need line number, but it is very helpful for debugging
              ('[' & Indent_Code & Line'Image & Line_Begin_Char_Pos'Image & Ind'Image & ']');
         end;

      when Anchored =>
         raise SAL.Programmer_Error with "Indent item has non-int label: " & Indent_Label'Image (Item.Label);
      end case;
   end Put;

   procedure Put
     (Item      : in Parse.Recover_Op_Nodes_Arrays.Vector;
      Error_Pos : in Buffer_Pos;
      Tree      : in Syntax_Trees.Tree)
   is
      use Ada.Strings.Unbounded;
      use WisiToken.Parse;
      use WisiToken.Parse.Recover_Op_Nodes_Arrays;
      use all type Ada.Containers.Count_Type;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      --  Output is a sequence of edit regions; each is:
      --  [error-pos edit-pos [inserted token-ids] [deleted token-ids] deleted-region]

      type State_Label is
        (None,     -- not started yet
         Inserted, -- edit-pos, some insert ids appended
         Deleted); -- some delete ids appended

      State : State_Label := None;
      --  State of the current edit region.

      Last_Edit_Pos  : Buffer_Pos       := Invalid_Buffer_Pos;
      Line           : Unbounded_String := To_Unbounded_String ("[");
      Deleted_Region : Buffer_Region    := Null_Buffer_Region;
      Last_Deleted   : Recover_Op_Nodes :=
        (Op               => Delete,
         Input_Node_Index => Syntax_Trees.Invalid_Node_Index,
         Del_ID           => Invalid_Token_ID,
         Del_Index        => Syntax_Trees.Sequential_Index'Last,
         Del_Node         => Syntax_Trees.Invalid_Node_Access);

      procedure Start_Edit_Region (Error_Pos, Edit_Pos : in Buffer_Pos)
      is begin
         Append (Line, "[");
         Append (Line, Trimmed_Image (Error_Pos));
         Append (Line, Edit_Pos'Image);
         Append (Line, "[");
      end Start_Edit_Region;

      procedure Terminate_Edit_Region
      is begin
         case State is
         when None =>
            null;
         when Inserted =>
            Append (Line, "][]" & Image (Deleted_Region) & "]");
         when Deleted =>
            --  Emacs (cdr (region)) is after last char to be deleted.
            Append
              (Line, "]" & "(" & Trimmed_Image (Integer (Deleted_Region.First)) & " ." &
                 Buffer_Pos'Image (Deleted_Region.Last + 1) & ")" & "]");
         end case;
         Deleted_Region := Null_Buffer_Region;
      end Terminate_Edit_Region;
   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line ("recover: " & WisiToken.Parse.Image (Item, Tree));
      end if;

      if Item.Length = 0 or not Tree.Parents_Set then
         --  Parents not set due to failed recover.
         return;
      end if;

      Append (Line, Recover_Code);
      for I in Item.First_Index .. Item.Last_Index loop
         declare
            use WisiToken.Syntax_Trees;

            Op : constant Recover_Op_Nodes := Element (Item, I);

            Edit_Pos_Node : constant Node_Access :=
              --  Can be Invalid_Node_Access when recover fails.
              (case Op.Op is
               when Insert =>
                 (if Op.Ins_Node = Invalid_Node_Access
                  then Invalid_Node_Access
                  else Tree.First_Source_Terminal (Op.Ins_Node, Trailing_Non_Grammar => True, Following => True)),

               when Delete => Op.Del_Node);

            Edit_Pos : constant Buffer_Pos :=
              (if Edit_Pos_Node = Invalid_Node_Access
               then Invalid_Buffer_Pos
               else Tree.Char_Region (Edit_Pos_Node, Trailing_Non_Grammar => True).First);
         begin
            if Last_Edit_Pos = Invalid_Buffer_Pos then
               Last_Edit_Pos := Edit_Pos;

            elsif Edit_Pos /= Last_Edit_Pos then
               Terminate_Edit_Region;
               State         := None;
               Last_Edit_Pos := Edit_Pos;
            end if;

            case Op.Op is
            when Insert =>
               case State is
               when None =>
                  Start_Edit_Region (Error_Pos, Edit_Pos);

               when Inserted =>
                  null;

               when Deleted =>
                  Terminate_Edit_Region;
                  Start_Edit_Region (Error_Pos, Edit_Pos);

               end case;
               Append (Line, Token_ID'Image (Op.Ins_ID));
               State := Inserted;

            when Delete =>
               Deleted_Region := Deleted_Region and Tree.Char_Region (Op.Del_Node, Trailing_Non_Grammar => False);
               declare
                  Skip : Boolean := False;
               begin
                  case State is
                  when None =>
                     Start_Edit_Region (Error_Pos, Edit_Pos);
                     Append (Line, "][");

                  when Inserted =>
                     Append (Line, "][");

                  when Deleted =>
                     if Tree.Lexer.Escape_Delimiter_Doubled (Last_Deleted.Del_ID) and then
                       ((Last_Deleted.Del_ID = Descriptor.String_1_ID and
                           Op.Del_ID = Descriptor.String_1_ID) or
                          (Last_Deleted.Del_ID = Descriptor.String_2_ID and
                             Op.Del_ID = Descriptor.String_2_ID))
                     then
                        declare
                           Tok_1_Char_Region : constant Buffer_Region := Tree.Char_Region
                             (Last_Deleted.Del_Node, Trailing_Non_Grammar => False);
                           Tok_2_Char_Region : constant Buffer_Region := Tree.Char_Region
                             (Op.Del_Node, Trailing_Non_Grammar => False);
                        begin
                           if Tok_1_Char_Region.Last + 1 = Tok_2_Char_Region.First then
                              --  Buffer text was '"""', lexer repair changed it to '""""'. The
                              --  repaired text looks like a single string with an embedded quote.
                              --  But here, it is two STRING_LITERAL tokens. Don't send the second
                              --  delete to elisp. See test/ada_mode-recover_string_quote_1.adb
                              Skip := True;
                           end if;
                        end;
                     end if;
                  end case;
                  State := Deleted;

                  if not Skip then
                     Append (Line, Token_ID'Image (Op.Del_ID));
                  end if;
               end;
               Last_Deleted := Op;
            end case;
         end;
      end loop;

      case State is
      when None =>
         null;
      when Inserted | Deleted =>
         Terminate_Edit_Region;
      end case;
      Append (Line, "]");
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Resolve_Anchors
     (Data : in out Parse_Data_Type;
      Tree : in     Syntax_Trees.Tree)
   is
      Begin_Indent : Integer renames Data.Begin_Indent;
   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.New_Line;
         Tree.Lexer.Trace.Put_Line ("Begin_Indent: " & Integer'Image (Data.Begin_Indent));
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Tree.Lexer.Trace.Put_Line (Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
         Tree.Lexer.Trace.Put_Line ("resolve anchors");
      end if;

      for Line in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         declare
            Indent : constant Indent_Type := Data.Indents (Line);
         begin
            case Indent.Label is
            when Not_Set =>
               --  We get here in partial_parse when there is no action to set indent
               --  for the first few lines; they are comments or low-level statements
               --  or declarations. ada_mode-recover_partial_28.adb
               Data.Indents.Replace_Element (Line, (Int, Data.Action_Region_Lines.First, Data.Begin_Indent));

            when Int =>
               Data.Indents.Replace_Element
                 (Line, (Int, Data.Action_Region_Lines.First, Indent.Int_Indent + Begin_Indent));

            when Anchored =>
               declare
                  Anchor_Line_Indent : Indent_Type renames Data.Indents (Indent.Anchor_Line);
               begin
                  case Anchor_Line_Indent.Label is
                  when Not_Set | Anchored =>
                     raise SAL.Programmer_Error with
                       "indent line" & Line'Image &
                       " uses anchor line" & Indent.Anchor_Line'Image &
                       " which has non-int anchor";

                  when Int =>
                     Data.Indents.Replace_Element
                       (Line,
                        (Int, Data.Action_Region_Lines.First, Anchor_Line_Indent.Int_Indent + Indent.Anchor_Delta));
                  end case;
               end;

            end case;
         end;
      end loop;

      if Trace_Action > Outline then
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            if I in Data.Action_Region_Lines.First .. Data.Action_Region_Lines.Last then
               Tree.Lexer.Trace.Put_Line (Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
            end if;
         end loop;
      end if;
   end Resolve_Anchors;

   procedure Set_End
     (Data           : in out Parse_Data_Type;
      Containing_Pos : in     Buffer_Pos;
      End_Pos        : in     Buffer_Pos;
      Trace          : in     WisiToken.Trace_Access)
   is
      use Navigate_Cursor_Lists;
      I            : Cursor := Data.End_Positions.First;
      Delete_Cache : Boolean;
      Temp         : Cursor;
   begin
      loop
         exit when not Has_Element (I);
         declare
            Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Element (I));
         begin
            if Cache.Pos in Containing_Pos .. End_Pos then
               Cache.End_Pos := (True, End_Pos);
               if Trace_Action > Detail then
                  Trace.Put_Line ("   " & Cache.Pos'Image & " end to " & Cache.End_Pos.Item'Image);
               end if;
               Delete_Cache := True;
            else
               Delete_Cache := False;
            end if;
         end;
         if Delete_Cache then
            Temp := Next (I);
            Delete (Data.End_Positions, I);

            I := Temp;
         else
            Next (I);
         end if;

      end loop;
   end Set_End;

   function To_Delta (Indent : in Indent_Type) return Delta_Type
   is begin
      return
        (Label                  => Simple,
         Simple_Delta           =>
           (case Indent.Label is
            when Not_Set  => (None, Invalid_Line_Number),
            when Int      => (Int, Indent.Controlling_Token_Line, Indent.Int_Indent),
            when Anchored => (Anchored, Indent.Controlling_Token_Line, Indent.Anchor_Line, Indent.Anchor_Delta)));
   end To_Delta;

   ----------
   --  public subprograms (declaration order)

   procedure Skip
     (Source : in     String;
      Last   : in out Integer;
      Char   : in     Character)
   is begin
      loop
         if Last = Source'Last then
            raise Protocol_Error with "at" & Last'Image & ": expecting '" & Char & "' found EOI";

         elsif Source (Last + 1) = ' ' then
            Last := Last + 1;
            exit when Char = ' ';

         elsif Source (Last + 1) = Char then
            Last := Last + 1;
            exit;
         else
            raise Protocol_Error with
              "at" & Last'Image & ": expecting '" & Char & "' found '" & Source (Last + 1) & "'";
         end if;
      end loop;
   end Skip;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      --  First we find the starting '"'; typically at Last + 1, but we
      --  allow for other cases.
      First : constant Integer := Index
        (Source  => Source,
         Pattern => """",
         From    => Last + 1);

      --  We must handle an arbitrary sequence of '\', and find the
      --  terminating '"'; so we search for either.
      Set  : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("\""");

      Temp : Integer := First + 1;
   begin
      Find_End :
      loop
         Last := Index
           (Source => Source,
            Set    => Set,
            From   => Temp);

         exit Find_End when Last = 0;

         case Source (Last) is
         when '\' =>
            declare
               subtype Mod_2_Result is Integer range 0 .. 1;
               Escape : Integer := 1;
            begin
               loop
                  exit Find_End when Source'Last < Last + Escape;

                  exit when Source (Last + Escape) /= '\';
                  Escape := @ + 1;
               end loop;
               Last := @ + Escape - 1;

               case Mod_2_Result'(Escape mod 2) is
               when 0 =>
                  --  Even number of '\'; next char is not escaped.
                  null;

               when 1 =>
                  --  Odd number of '\'; next char is escaped.
                  Last := @ + 1;

               end case;
            end;

         when '"' =>
            exit Find_End;
         when others =>
            raise SAL.Programmer_Error;
         end case;
         Temp := Last + 1;
      end loop Find_End;

      if First = 0 or Last = 0 then
         raise Protocol_Error with "at" & Last'Image & ": no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

   function Get_Enum
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 1;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First + 1); -- Skip a leading space if present.

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;
      return Source (First .. Last);
   end Get_Enum;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 1;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First + 1); -- Skip a leading space if present.

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when others =>
      raise Protocol_Error with "at" & First'Image & ": bad integer '" & Source (First .. Last) & "'";
   end Get_Integer;

   function To_Unix_Line_Endings (Source : in out String) return Integer
   --  Return count of line endings converted.
   is
      Read              : Integer := Source'First;
      Write             : Integer := Source'First - 1;
      Line_Ending_Count : Integer := 0;
   begin
      loop
         exit when Read > Source'Last;
         if Source (Read) = ASCII.CR and (Read < Source'Last and then Source (Read + 1) = ASCII.LF) then
            Write             := @ + 1;
            Source (Write)    := ASCII.LF;
            Read              := @ + 2;
            Line_Ending_Count := @ + 1;
         else
            Write          := @ + 1;
            Source (Write) := Source (Read);
            Read           := @ + 1;
         end if;
      end loop;
      return Line_Ending_Count;
   end To_Unix_Line_Endings;

   procedure To_Unix_Line_Endings
     (Source           : in     Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer)
   is
      Line_End_Count : constant Integer := To_Unix_Line_Endings (Source (Source'First .. Source_Byte_Last));
   begin
      Source_Byte_Last := @ - Line_End_Count;
      Source_Char_Last := @ - Line_End_Count;
   end To_Unix_Line_Endings;

   function Image_Action (Action : in Syntax_Trees.Post_Parse_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image_Action;

   procedure Reset_Post_Parse
     (Data                : in out Parse_Data_Type;
      Tree                : in     WisiToken.Syntax_Trees.Tree'Class;
      Post_Parse_Action   : in     Post_Parse_Action_Type;
      Action_Region_Bytes : in     WisiToken.Buffer_Region;
      Action_Region_Chars : in     WisiToken.Buffer_Region;
      Begin_Indent        : in     Integer)
   is begin
      if Tree.Root = Syntax_Trees.Invalid_Node_Access then
         raise Parse_Error with "previous parse failed; can't execute post_parse action.";
      end if;

      Data.Post_Parse_Action   := Post_Parse_Action;
      Data.Action_Region_Bytes := Action_Region_Bytes;
      Data.Action_Region_Chars := Action_Region_Chars;
      Data.Begin_Indent := Begin_Indent;

      case Post_Parse_Action is
      when Navigate =>
         Data.Navigate_Caches.Clear;
         Data.End_Positions.Clear;
         Data.Name_Caches.Clear;

      when Face =>
         Data.Face_Caches.Clear;

      when Indent =>
         Data.Action_Region_Lines :=
           (First => Tree.Line_At_Byte_Pos (Action_Region_Bytes.First),
            Last  => Tree.Line_At_Byte_Pos (Action_Region_Bytes.Last));

         --  We need more lines in Indents than in Action_Region, for nonterms
         --  that extend outside the action region.
         declare
            Tree_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
              (Tree.Root, Trailing_Non_Grammar => True);
         begin
            Data.Indents.Set_First_Last
              (First => Tree_Line_Region.First,
               Last  => Tree_Line_Region.Last);
         end;

         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Data.Indents.Replace_Element (I, (Not_Set, Invalid_Line_Number));
         end loop;
      end case;

      if Data.Augmented_Cache_Version = Cache_Version'Last then
         Tree.Free_Augmented;
         Data.Augmented_Cache_Version := Cache_Version'First + 1;
      else
         Data.Augmented_Cache_Version := @ + 1;
      end if;
   end Reset_Post_Parse;

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type
   is begin
      return Data.Post_Parse_Action;
   end Post_Parse_Action;

   function Action_Region_Bytes (Data : in Parse_Data_Type) return WisiToken.Buffer_Region
   is begin
      return Data.Action_Region_Bytes;
   end Action_Region_Bytes;

   overriding
   function Copy_Augmented
     (User_Data : in Parse_Data_Type;
      Augmented : in Syntax_Trees.Augmented_Class_Access)
     return Syntax_Trees.Augmented_Class_Access
   is
      Old_Aug : constant Augmented_Access := Augmented_Access (Augmented);
      New_Aug : constant Augmented_Access := new Wisi.Augmented'(Old_Aug.all);
   begin
      return Syntax_Trees.Augmented_Class_Access (New_Aug);
   end Copy_Augmented;

   overriding
   procedure Initialize_Actions
     (Data : in out Parse_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class)
   is begin
      --  Parsing is complete, with error recover insert/delete tokens in
      --  the parse tree. Insert_Token, Delete_Token have been called;

      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line ("action_region_bytes: " & Image (Data.Action_Region_Bytes));
         Tree.Lexer.Trace.Put_Line ("action_region_lines: " & Image (Data.Action_Region_Lines));
      end if;
   end Initialize_Actions;

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   --  Set data that allows using Inserted_Token when computing indent.
   is
      use Syntax_Trees;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Inserted_Before : constant Valid_Node_Access := Tree.Next_Terminal (Inserted_Token);

      First_Token : constant Node_Access := Tree.Line_Begin_Token
        (Tree.Line_Region (Inserted_Before, Trailing_Non_Grammar => True).First);

      Insert_Location : WisiToken.Insert_Location := Before_Next;
   begin
      if First_Token = Inserted_Token then
         declare
            use all type Ada.Containers.Count_Type;
            use all type SAL.Base_Peek_Type;

            --  See ada_mode-interactive_02.adb, "Typing ..."; three tests.
            --
            --  When typing new code, we want a new blank line to be indented as
            --  if the code was there already. To accomplish that, we put the
            --  inserted tokens at the end of the line before the Before token;
            --  that will be after the non-grammar on the previous terminal.
            --
            --  Compare to test/ada_mode-recover_20.adb. There we are not typing
            --  new code, but there is a blank line; the right paren is placed at
            --  the end of the blank line, causing the comment to be indented.
            --
            --  Also test/ada_mode-interactive_05.adb Proc_2; error recover
            --  inserts "null;" before "end"; we want it on the blank line. So
            --  Insert_After has to see the next source_terminal, which may not be
            --  Inserted_Before.

            Next_Source_Terminal : constant Valid_Node_Access :=
              (if Tree.Label (Inserted_Before) = Syntax_Trees.Source_Terminal
               then Inserted_Before
               else Tree.Next_Source_Terminal (Inserted_Before, Trailing_Non_Grammar => False));

            Prev_Terminal : constant Valid_Node_Access := Tree.Prev_Terminal (Inserted_Token);
            --  Tree.SOI if Inserted_Token is inserted before first grammar token

            Prev_Non_Grammar  : Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Prev_Terminal);
            Token_Non_Grammar : Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Inserted_Token);

            --  Prev_Non_Grammar must have at least one New_Line, since First
            --  (Inserted_Token) is True. The whitespace after the New_Line is not
            --  given a token, but comments are.
            --
            --  If the first two tokens in Prev_Non_Grammar are both New_Lines,
            --  there is a blank line after the code line (and before any
            --  comments); assume that is the edit point; see
            --  test/ada_mode-interactive_2.adb "A := B \n+C;"
            Insert_Line      : Base_Line_Number_Type := Invalid_Line_Number;
            Blank_Line_Index : SAL.Base_Peek_Type    := 0; -- new_line ending blank line
            Comment_Index    : SAL.Base_Peek_Type    := 0; -- first comment not on code line

            procedure Check_Non_Grammar
            --  Set Insert_Line, Blank_Line_Index, Comment_Index
            is
               I : SAL.Base_Peek_Type := Prev_Non_Grammar.First_Index;
            begin
               loop
                  exit when I > Prev_Non_Grammar.Last_Index;

                  if Comment_Index = 0 and
                    I > Prev_Non_Grammar.First_Index and
                    Prev_Non_Grammar (I).ID /= Descriptor.New_Line_ID
                  then
                     --  Exclude comment on same line as code. test/ads_mode-recover_13.adb
                     Insert_Line   := Prev_Non_Grammar (I).Line_Region.First;
                     Comment_Index := I;
                  end if;

                  if (Blank_Line_Index = 0 and
                        I < Prev_Non_Grammar.Last_Index) and then
                    (Tree.Lexer.Terminated_By_New_Line (Prev_Non_Grammar (I).ID) and
                       Prev_Non_Grammar (I + 1).ID = Descriptor.New_Line_ID)
                  then
                     Insert_Line      := Prev_Non_Grammar (I + 1).Line_Region.First;
                     Blank_Line_Index := I + 1;
                  end if;

                  exit when Blank_Line_Index /= 0 and Comment_Index /= 0;

                  I := I + 1;
               end loop;
            end Check_Non_Grammar;
         begin
            Check_Non_Grammar;

            Insert_Location := Parse_Data_Type'Class (Data).Insert_After
              (Tree,
               Insert_Token        => Inserted_Token,
               Insert_Before_Token => Next_Source_Terminal,
               Comment_Present     => Comment_Index > 0,
               Blank_Line_Present  => Blank_Line_Index > 0);

            pragma Assert (Prev_Non_Grammar.Length > 0); --  else First would be false in condition above.

            case Insert_Location is
            when Between =>
               --  Insert on blank line or comment line
               --
               --  test/ada_mode-interactive_2.adb Function_Access_2,
               --  ada_mode-recover_17.adb missing 'end if' at end.
               --  Indent for new code line extending previous code.
               declare
                  New_Non_Grammar : WisiToken.Lexer.Token_Arrays.Vector;
                  Start_Index : constant SAL.Peek_Type :=
                    (if Blank_Line_Index > 0 then Blank_Line_Index else Comment_Index);
               begin
                  for I in Start_Index .. Prev_Non_Grammar.Last_Index loop
                     New_Non_Grammar.Append (Prev_Non_Grammar (I));
                  end loop;
                  New_Non_Grammar.Append (Token_Non_Grammar);

                  Token_Non_Grammar := New_Non_Grammar;

                  Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Start_Index - 1);

                  Tree.Set_Insert_Location (Inserted_Token, Between);

                  if Trace_Action > WisiToken.Outline then
                     Tree.Lexer.Trace.Put_Line
                       ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True, Non_Grammar => True) &
                          " on line" & Insert_Line'Image & "; move some non_grammar from " &
                          Tree.Image (Prev_Terminal, Node_Numbers => True, Non_Grammar => True));
                  end if;
               end;

            when After_Prev =>
               if Prev_Non_Grammar (Prev_Non_Grammar.First_Index).ID = Tree.Lexer.Descriptor.SOI_ID then
                  --  Don't move SOI non_grammar

                  for I in Prev_Non_Grammar.First_Index + 1 .. Prev_Non_Grammar.Last_Index loop
                     Token_Non_Grammar.Append (Prev_Non_Grammar (I));
                  end loop;
                  Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Prev_Non_Grammar.First_Index);

               else
                  Token_Non_Grammar := Prev_Non_Grammar;

                  Prev_Non_Grammar := WisiToken.Lexer.Token_Arrays.Empty_Vector;
               end if;

               Tree.Set_Insert_Location (Inserted_Token, After_Prev);

               if Trace_Action > WisiToken.Outline then
                  Tree.Lexer.Trace.Put_Line
                    ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True, Non_Grammar => True) &
                       " after " & Tree.Image (Prev_Terminal, Node_Numbers => True, Non_Grammar => True));
               end if;

            when Before_Next =>
               null;
            end case;
         end;
      end if;

      if Insert_Location = Before_Next and Trace_Action > WisiToken.Outline then
         Tree.Lexer.Trace.Put_Line
           ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True, Non_Grammar => True) &
              " before " & Tree.Image (Inserted_Before, Node_Numbers => True, Non_Grammar => True));
      end if;
   end Insert_Token;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Statement_Param_Array)
   is
      use all type SAL.Base_Peek_Type;

      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      First_Item         : Boolean        := True;
      Start_Set          : Boolean        := False;
      Override_Start_Set : Boolean        := False;
      Containing_Pos     : Nil_Buffer_Pos := Nil;
   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line ("Statement_Action " & Tree.Image (Nonterm, Children => True));
      end if;

      for Pair of Params loop
         if Pair.Index > Tree.Child_Count (Nonterm) then
            raise Fatal_Error with Tree.Error_Message
              (Nonterm,
               "wisi-statement-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) &
                 " token index" & SAL.Peek_Type'Image (Pair.Index) &
                 " not in tokens range (1 .." & Tree.Child_Count (Nonterm)'Image & "); bad grammar action.");

         elsif Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, Pair.Index), Trailing_Non_Grammar => False),
            Data.Action_Region_Chars)
         then
            declare
               use all type Syntax_Trees.Node_Label;
               Token  : constant Syntax_Trees.Node_Access :=
                 (if Pair.Class = Statement_End and then
                    Tree.Label (Tree.Child (Nonterm, Pair.Index)) = Syntax_Trees.Nonterm
                  then Tree.Last_Terminal (Tree.Child (Nonterm, Pair.Index))
                  else Tree.Child (Nonterm, Pair.Index));

               Cache_Pos : constant Buffer_Pos         := Tree.Char_Region (Token, Trailing_Non_Grammar => False).First;
               Cursor    : Navigate_Cache_Trees.Cursor := Navigate_Cache_Trees.Find
                 (Data.Navigate_Caches.Iterate, Cache_Pos,
                  Direction => Navigate_Cache_Trees.Unknown);
            begin
               if Navigate_Cache_Trees.Has_Element (Cursor) then
                  declare
                     Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                  begin
                     if Pair.Class in Statement_Start | Statement_Override then
                        if Start_Set then
                           Cache.Class := Motion;
                        else
                           Cache.Class := Statement_Start;
                           Start_Set   := True;
                        end if;
                     elsif Override_Start_Set then
                        Cache.Class := Statement_Start;
                        Start_Set   := True;
                     else
                        Cache.Class := Pair.Class;
                     end if;
                     Cache.Statement_ID   := Tree.ID (Nonterm);
                     Cache.Containing_Pos := Containing_Pos;
                     if Trace_Action > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("   " & Cache.Pos'Image & " nonterm to " & Image (Cache.Statement_ID, Descriptor) &
                             " containing to" & Image (Cache.Containing_Pos));
                     end if;
                  end;
               else
                  Cursor := Data.Navigate_Caches.Insert
                    ((Pos            => Cache_Pos,
                      Statement_ID   => Tree.ID (Nonterm),
                      ID             => Tree.ID (Token),
                      Length         => Length (Tree.Char_Region (Token, Trailing_Non_Grammar => False)),
                      Class          =>
                        (if Override_Start_Set then Statement_Start
                         else
                           (case Pair.Class is
                            when Statement_Start | Statement_Override =>
                              (if Start_Set then Motion else Statement_Start),
                            when others => Pair.Class)),
                      Containing_Pos => Containing_Pos,
                      others         => Nil));

                  if Trace_Action > Detail then
                     declare
                        Cache : Navigate_Cache_Type renames Data.Navigate_Caches.Constant_Ref (Cursor);
                     begin
                        Tree.Lexer.Trace.Put_Line
                          ("   " & Cache.Pos'Image & " create " & Image (Cache.ID, Descriptor) &
                             ", containing to " & Image (Data.Navigate_Caches.Constant_Ref (Cursor).Containing_Pos));
                     end;
                  end if;
               end if;

               Data.End_Positions.Append (Cursor);

               if First_Item then
                  First_Item := False;
                  if Override_Start_Set or Pair.Class in Statement_Start | Statement_Override then
                     Override_Start_Set := False;
                     Containing_Pos     := (True, Tree.Char_Region (Token, Trailing_Non_Grammar => False).First);

                     --  Set containing on all contained caches
                     declare
                        use Navigate_Cache_Trees;
                        Iterator : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;

                        Nonterm_Char_Region : constant Buffer_Region := Tree.Char_Region
                          (Nonterm, Trailing_Non_Grammar => False);

                        Cursor : Navigate_Cache_Trees.Cursor :=
                          (if Length (Nonterm_Char_Region) = 0
                           then No_Element
                           else Find_In_Range
                             (Iterator, Ascending, Nonterm_Char_Region.First + 1, -- don't set containing on start
                              Nonterm_Char_Region.Last));
                     begin
                        loop
                           exit when not Has_Element (Cursor);
                           declare
                              Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                           begin
                              if not Cache.Containing_Pos.Set then
                                 Cache.Containing_Pos := Containing_Pos;
                                 if Trace_Action > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ("   " & Cache.Pos'Image & " containing to " & Image
                                         (Data.Navigate_Caches.Constant_Ref (Cursor).Containing_Pos));
                                 end if;
                              end if;
                              exit when Nonterm_Char_Region.Last < Cache.Pos + 1;
                           end;
                           Cursor := Iterator.Next (Cursor);
                        end loop;
                     end;
                  end if;
               end if;

               if Pair.Class = Statement_End and Containing_Pos.Set then
                  Set_End (Data, Containing_Pos.Item, Cache_Pos, Tree.Lexer.Trace);
               end if;
            end;

         else
            --  Token.Char_Region is empty or outside action_region
            if First_Item and Pair.Class = Statement_Start then
               Override_Start_Set := True;
            end if;
         end if;
      end loop;
   end Statement_Action;

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Name    : in     WisiToken.Positive_Index_Type)
   is
      use all type SAL.Base_Peek_Type;
      use all type Syntax_Trees.Node_Label;
   begin
      if Name > Tree.Child_Count (Nonterm) then
         raise Grammar_Error with Tree.Error_Message
           (Nonterm,
            "wisi-name-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) & " name (" &
              Trimmed_Image (Name) & ") not in child range (1 .." &
              Tree.Child_Count (Nonterm)'Image & "); bad grammar action.");
      end if;

      if Length (Tree.Char_Region (Tree.Child (Nonterm, Name), Trailing_Non_Grammar => False)) = 0 then
         --  Token is virtual; it does not appear in the actual buffer, so we
         --  can't set a text property on it.
         return;
      elsif not Overlaps
        (Tree.Char_Region (Tree.Child (Nonterm, Name), Trailing_Non_Grammar => False), Data.Action_Region_Chars)
      then
         return;
      end if;

      pragma Assert (Tree.Label (Tree.Child (Nonterm, Name)) in Source_Terminal | Syntax_Trees.Nonterm);

      declare
         use Name_Cache_Trees;
         Name_Char_Region : constant Buffer_Region := Tree.Char_Region
           (Tree.Child (Nonterm, Name), Trailing_Non_Grammar => False);
         Cursor     : constant Name_Cache_Trees.Cursor := Find
           (Data.Name_Caches.Iterate, Name_Char_Region.First,
            Direction => Name_Cache_Trees.Unknown);
      begin
         if Has_Element (Cursor) then
            raise Fatal_Error with Tree.Error_Message
              (Tree.Child (Nonterm, Name), Tree.Image
                 (Node         => Tree.Child (Nonterm, Name),
                  Node_Numbers => Trace_Action > Extra,
                  RHS_Index    => Trace_Action > Extra)
                 & ": wisi-name-action: name set twice.");
         else
            if Trace_Action > Detail then
               Tree.Lexer.Trace.Put_Line
                 ("Name_Action " & Tree.Image
                    (Nonterm,
                     Node_Numbers    => Trace_Action > Extra,
                     RHS_Index       => Trace_Action > Extra) & " " & Tree.Image
                       (Tree.Child (Nonterm, Name),
                        Node_Numbers => Trace_Action > Extra,
                        RHS_Index    => Trace_Action > Extra));
            end if;

            if Name_Char_Region /= Null_Buffer_Region then
               Data.Name_Caches.Insert (Name_Char_Region);
            end if;
         end if;
      end;
   end Name_Action;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Motion_Param_Array)
   is
      use Navigate_Cache_Trees;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Iter           : constant Iterator := Data.Navigate_Caches.Iterate;
      Prev_Cache_Cur : Cursor;
   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line
           ("Motion_Action " & Image (Tree.ID (Nonterm), Descriptor) & " " &
              Image (Tree.Byte_Region (Nonterm, Trailing_Non_Grammar => False)));
      end if;
      for Param of Params loop
         if Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False),
            Data.Action_Region_Chars)
         then
            declare
               use all type Syntax_Trees.Node_Label;
               Token     : constant Syntax_Trees.Valid_Node_Access := Tree.Child (Nonterm, Param.Index);
               Region    : constant Buffer_Region := Tree.Char_Region (Token, Trailing_Non_Grammar => False);
               Cache_Cur : Cursor;
               Skip      : Boolean;
               Done      : Boolean := False;
            begin
               loop
                  Skip := False;

                  case Tree.Label (Tree.Child (Nonterm, Param.Index)) is
                  when Source_Terminal =>
                     Cache_Cur := Find (Iter, Region.First);
                     Done      := True;

                  when Virtual_Terminal | Virtual_Identifier =>
                     Skip := True;
                     Done := True;

                  when Syntax_Trees.Nonterm =>
                     if Param.ID = Invalid_Token_ID then
                        Cache_Cur := Find (Iter, Region.First);
                        Done      := True;

                     else
                        Skip := True;

                        if not Has_Element (Cache_Cur) then
                           Cache_Cur := Find_In_Range (Iter, Ascending, Region.First, Region.Last);
                        end if;

                        loop
                           exit when not Has_Element (Cache_Cur);
                           if Data.Navigate_Caches (Cache_Cur).Pos > Region.Last then
                              Cache_Cur := No_Element;
                              exit;

                           elsif Data.Navigate_Caches (Cache_Cur).ID = Param.ID and
                             not Data.Navigate_Caches (Cache_Cur).Prev_Pos.Set
                           then
                              Skip := False;
                              exit;
                           end if;

                           Cache_Cur := Next (Iter, Cache_Cur);
                        end loop;
                     end if;
                  end case;

                  if not Skip then
                     if not Has_Element (Cache_Cur) then
                        raise Fatal_Error with Tree.Error_Message
                          (Tree.Child (Nonterm, Param.Index),
                           Message   => "wisi-motion-action: token " &
                             WisiToken.Image (Tree.ID (Token), Descriptor) &
                             " has no cache; add to statement-action for " &
                             Trimmed_Image (Tree.Production_ID (Nonterm)) & ".");
                     end if;

                     if Has_Element (Prev_Cache_Cur) then
                        declare
                           Cache      : Navigate_Cache_Type renames Data.Navigate_Caches (Cache_Cur);
                           Prev_Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Prev_Cache_Cur);
                        begin
                           if Cache.Prev_Pos.Set then
                              if Trace_Action > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("   " & Cache.Pos'Image & " prev already at " & Cache.Prev_Pos.Item'Image);
                              end if;
                           else
                              Cache.Prev_Pos := (True, Prev_Cache.Pos);
                              if Trace_Action > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("   " & Cache.Pos'Image & " prev to " & Cache.Prev_Pos.Item'Image);
                              end if;
                           end if;

                           if Prev_Cache.Next_Pos.Set then
                              if Trace_Action > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("   " & Prev_Cache.Pos'Image & " next already at " &
                                      Prev_Cache.Next_Pos.Item'Image);
                              end if;
                           else
                              Prev_Cache.Next_Pos := (True, Cache.Pos);
                              if Trace_Action > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("   " & Prev_Cache.Pos'Image & " next to " & Prev_Cache.Next_Pos.Item'Image);
                              end if;
                           end if;
                        end;
                     end if;

                     if Data.Navigate_Caches (Cache_Cur).Next_Pos.Set then
                        --  Set Cache_Cur to end of Cache_Cur.Next chain.
                        --  Handles 'elsif ... then' in if_statement.
                        loop
                           Cache_Cur := Find (Iter, Data.Navigate_Caches (Cache_Cur).Next_Pos.Item);
                           exit when not Data.Navigate_Caches (Cache_Cur).Next_Pos.Set;
                        end loop;
                     end if;
                     Prev_Cache_Cur := Cache_Cur;
                     Cache_Cur := Iter.Next (Cache_Cur);
                  end if;
                  exit when Done or not Has_Element (Cache_Cur);
               end loop;
            end;
         end if;
      end loop;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array)
   is
      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False),
            Data.Action_Region_Chars)
         then
            if Trace_Action > Outline then
               Tree.Lexer.Trace.Put_Line
                 ("face_apply_action: " & Tree.Image
                    (Tree.Child (Nonterm, Param.Index), Node_Numbers => True) &
                    " " & Param.Prefix_Face'Image & " " & Param.Suffix_Face'Image);
            end if;

            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region
                 (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False);
            begin
               Cache_Cur := Find (Iter, Token_Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                        --  Check for suffix
                        Suffix_Cur := Next (Iter, Cache_Cur);
                        if Has_Element (Suffix_Cur) then
                           declare
                              Suf_Cache : Face_Cache_Type renames Data.Face_Caches (Suffix_Cur);
                           begin
                              if Suffix = Suf_Cache.Class and
                                Contains (Token_Char_Region, Suf_Cache.Char_Region.First)
                              then
                                 Suf_Cache.Face := (True, Param.Suffix_Face);
                              end if;
                           end;
                        end if;

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
               else
                  Data.Face_Caches.Insert ((Token_Char_Region, Suffix, (True, Param.Suffix_Face)));
               end if;
            end;
         end if;
      end loop;
   end Face_Apply_Action;

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False),
            Data.Action_Region_Chars)
         then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region
                 (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False);
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token_Char_Region.First, Token_Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token_Char_Region.Last;
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
                  Cache_Cur := Next (Iter, Cache_Cur);
               end loop;
            end;
         end if;
      end loop;
   end Face_Apply_List_Action;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Mark_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False),
            Data.Action_Region_Chars)
         then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region
                 (Tree.Child (Nonterm, Param.Index), Trailing_Non_Grammar => False);
            begin
               Cache_Cur := Find (Iter, Token_Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Char_Region.Last + 1, Token_Char_Region.Last);
                     To_Delete : Buffer_Pos_Lists.List;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Data.Face_Caches (Other_Cur).Char_Region.First > Token_Char_Region.Last;
                        To_Delete.Append (Data.Face_Caches (Other_Cur).Char_Region.First);
                        Other_Cur := Next (Iter, Other_Cur);
                     end loop;

                     Cache.Class            := Param.Class;
                     Cache.Char_Region.Last := Token_Char_Region.Last;

                     for Face of To_Delete loop
                        Data.Face_Caches.Delete (Face);
                     end loop;
                  end;
               else
                  Data.Face_Caches.Insert ((Token_Char_Region, Param.Class, (Set => False)));
               end if;
            end;
         end if;
      end loop;
   end Face_Mark_Action;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Remove_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for I of Params loop
         if Overlaps
           (Tree.Char_Region (Tree.Child (Nonterm, I), Trailing_Non_Grammar => False), Data.Action_Region_Chars)
         then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region
                 (Tree.Child (Nonterm, I), Trailing_Non_Grammar => False);
               To_Delete : Buffer_Pos_Lists.List;
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token_Char_Region.First, Token_Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token_Char_Region.Last;
                  To_Delete.Append (Data.Face_Caches (Cache_Cur).Char_Region.First);
                  Cache_Cur := Next (Iter, Cache_Cur);
               end loop;
               for Face of To_Delete loop
                  Data.Face_Caches.Delete (Face);
               end loop;
            end;
         end if;
      end loop;
   end Face_Remove_Action;

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Item);
      end return;
   end "+";

   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function Image (Item : in Simple_Indent_Param) return String
   is begin
      return "(" & Simple_Indent_Param_Label'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Block | Int => ", " & Trimmed_Image (Item.Int_Delta),
         when Simple_Param_Anchored => Positive_Index_Type'Image (Item.Anchored_Index) & "," &
           Integer'Image (Item.Anchored_Delta),
         when Language => "<language_function>") & ")";
   end Image;

   function Add_Simple_Indent_Param (Left, Right : in Simple_Indent_Param) return Simple_Indent_Param
   is begin
      case Left.Label is
      when None =>
         return Right;

      when Block =>
         case Right.Label is
         when None =>
            return Left;

         when Block | Int =>
            return (Block, Left.Int_Delta + Right.Int_Delta);

         when Anchored_0 =>
            return (Anchored_0, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Anchored_1 =>
            return (Anchored_1, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Int =>
         case Right.Label is
         when None =>
            return Left;

         when Block =>
            return (Block, Left.Int_Delta + Right.Int_Delta);

         when Int =>
            return (Int, Left.Int_Delta + Right.Int_Delta);

         when Anchored_0 =>
            return (Anchored_0, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Anchored_1 =>
            return (Anchored_1, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Simple_Param_Anchored =>
         case Right.Label is
         when None =>
            return Left;

         when Block | Int =>
            case Simple_Param_Anchored'(Left.Label) is
            when Anchored_0 =>
               return (Anchored_0, Left.Anchored_Index, Left.Anchored_Delta + Right.Int_Delta);
            when Anchored_1 =>
               return (Anchored_1, Left.Anchored_Index, Left.Anchored_Delta + Right.Int_Delta);
            end case;

         when Simple_Param_Anchored | Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Language =>
         raise Grammar_Error with "adding incompatible indent params";
      end case;
   end Add_Simple_Indent_Param;

   function Image (Item : in Indent_Param) return String
   is begin
      return "(" & Indent_Param_Label'Image (Item.Label) & ", " &
        (case Item.Label is
         when Simple => Image (Item.Param),
         when Hanging_Label =>
            Image (Item.Hanging_Delta_1) & ", "  & Image (Item.Hanging_Delta_2))
        & ")";
   end Image;

   function Image (Item : in Indent_Pair) return String
   is begin
      return "(" & Image (Item.Code_Delta) &
        (if Item.Comment_Present
         then ", " & Image (Item.Comment_Delta)
         else "") & ")";
   end Image;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Indent_Param_Array)
   is
      use all type SAL.Base_Peek_Type;

      function In_Line_Region (Node : in Syntax_Trees.Valid_Node_Access) return Boolean
      is
         Node_Region : constant Line_Region := Tree.Line_Region (Node, Trailing_Non_Grammar => True);
      begin
         return Node_Region.First > Node_Region.Last -- null region is always in active region
           or else
           (Contains (Data.Action_Region_Lines, Node_Region.First) or
              Contains (Data.Action_Region_Lines, Node_Region.Last) or
              (Node_Region.First < Data.Action_Region_Lines.First and
                 Node_Region.Last > Data.Action_Region_Lines.Last));
      end In_Line_Region;

   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line
           ("indent_action_0 " & Tree.Image
              (Nonterm, RHS_Index => True, Node_Numbers => True, Augmented => True, Line_Numbers => True));
      end if;

      for I in 1 .. Tree.Child_Count (Nonterm) loop
         if not (Tree.SOI = Tree.Child (Nonterm, I) or Tree.EOI = Tree.Child (Nonterm, I)) and then
           --  We see these in a partial parse.

           (I in Params'Range and then
              --  In some translated EBNF, not every token has an indent param.
              In_Line_Region (Tree.Child (Nonterm, I)))
         then
            declare
               Child : constant Syntax_Trees.Valid_Node_Access := Tree.Child (Nonterm, I);

               Indenting : constant Wisi.Indenting := Compute_Indenting (Data, Tree, Child);

               Code_Delta : constant Delta_Type :=
                 (if Indenting.Code = Null_Line_Region
                  then Null_Delta
                  else Indent_Compute_Delta
                    (Data, Tree, Nonterm, Params (I).Code_Delta, Child, Indenting_Comment => False));

               Controlling_Token : Syntax_Trees.Node_Access;
               Comment_Param     : Indent_Param;
               Comment_Param_Set : Boolean := False;
               Comment_Delta     : Delta_Type;
            begin
               if Trace_Action > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("...    code " & Tree.Image
                       (Child,
                        Node_Numbers => Trace_Action > Extra,
                        Line_Numbers => True) & ": " &
                       Image (Params (I).Code_Delta));
               end if;

               if Code_Delta /= Null_Delta then
                  Indent_Token_1
                    (Data, Tree,
                     Line_Region       => Indenting.Code,
                     Delta_Indent      => Code_Delta,
                     Indenting_Comment => None);
               end if;

               if Indenting.Comment /= Null_Line_Region then
                  if Params (I).Comment_Present then
                     Comment_Param     := Params (I).Comment_Delta;
                     Controlling_Token := Child;
                     Comment_Param_Set := True;

                  elsif I < Tree.Child_Count (Nonterm) then
                     Comment_Param     := Params (I + 1).Code_Delta;
                     Controlling_Token := Tree.Child (Nonterm, I + 1);
                     Comment_Param_Set := True;
                  end if;

                  if Comment_Param_Set then
                     if Trace_Action > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("... comment " & Tree.Image
                             (Controlling_Token,
                              Node_Numbers => Trace_Action > Extra,
                              Line_Numbers => True) & ": " &
                             Image (Comment_Param));
                     end if;

                     Comment_Delta := Indent_Compute_Delta
                       (Data, Tree, Nonterm, Comment_Param,
                        Indenting_Token   => Controlling_Token,
                        Indenting_Comment => True);

                     if Comment_Delta /= Null_Delta then
                        Indent_Token_1
                          (Data, Tree,
                           Line_Region       => Indenting.Comment,
                           Delta_Indent      => Comment_Delta,
                           Controlling_Delta =>
                             (if Params (I).Comment_Present
                              then Null_Delta --  test_select.adb accept E2

                              else To_Delta --  ada_mode-conditional_expressions.adb case expression for K.
                                (Data.Indents
                                   (Tree.Line_Region (Controlling_Token, Trailing_Non_Grammar => True).First))),
                           Indenting_Comment => (if Params (I).Comment_Present then Trailing else Leading));
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Indent_Action_0;

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Token   : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Label             : in     Hanging_Label)
     return Delta_Type
   is
      Indenting_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
        (Indenting_Token, Trailing_Non_Grammar => True);

      Indenting : constant Wisi.Indenting := Compute_Indenting (Data, Tree, Indenting_Token);

      function Compute_Hanging_2 return Simple_Indent_Param
      is begin
         --  WORKAROUND: GNAT Commmunity 2020 gives a bogus compile error when
         --  we try to inline this with an if_expression.
         if Indenting_Line_Region.First = Indenting.Code.First then
            return Add_Simple_Indent_Param (Delta_1, Delta_2);
         else
            return Delta_2;
         end if;
      end Compute_Hanging_2;

   begin
      return Result : Delta_Type :=
        (Hanging,
         Hanging_First_Line  => Indenting_Line_Region.First,
         Hanging_Delta_1     => Indent_Compute_Delta
           (Data, Tree, Nonterm,
            (Simple,
             (if Indenting_Line_Region.First = Indenting.Code.First
              then Delta_1
              else (Label => None))),
            Indenting_Token, Indenting_Comment).Simple_Delta,
         Hanging_Delta_2     =>
           Indent_Compute_Delta
             (Data, Tree, Nonterm,
              (Simple,
               (case Label is
                when Hanging_0 => Delta_2,
                when Hanging_1 =>
                  (if Indenting_Line_Region.First = Indenting.Code.First
                   then Delta_2 else Delta_1),
                when Hanging_2 =>
                   Compute_Hanging_2)),
              Indenting_Token, Indenting_Comment)
             .Simple_Delta)
      do
         --  Controlling_Token_Line for Delta_2 is the first non-comment
         --  line indented by Delta_2.
         if Label = Hanging_1 and
           Indenting_Line_Region.First /= Indenting.Code.First
         then
            --  Only using Delta_1
            null;
         else
            for Line in Indenting.Code.First +
              (if Indenting_Line_Region.First = Indenting.Code.First then 1 else 0)
              .. Indenting.Code.Last
            loop
               if Tree.Line_Begin_Token (Line) /= Syntax_Trees.Invalid_Node_Access then
                  Result.Hanging_Delta_2.Controlling_Token_Line := Line;
                  exit;
               end if;
            end loop;
         end if;
      end return;
   end Indent_Hanging_1;

   package Node_Access_Address is new System.Address_To_Access_Conversions (WisiToken.Syntax_Trees.Node);

   function Address_Image (Item : in WisiToken.Syntax_Trees.Valid_Node_Access) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Int : constant System.Storage_Elements.Integer_Address := System.Storage_Elements.To_Integer
        (Node_Access_Address.To_Address (Node_Access_Address.Object_Pointer (Item)));
   begin
      return """" & Trim (Int'Image, Both) & """";
   end Address_Image;

   function To_Node_Access (Item : in String) return WisiToken.Syntax_Trees.Valid_Node_Access
   is
      Int : constant System.Address := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address'Value (Item));
   begin
      return WisiToken.Syntax_Trees.Valid_Node_Access (Node_Access_Address.To_Pointer (Int));
   end To_Node_Access;

   procedure Query_Tree
     (Data  : in Parse_Data_Access_Constant;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Query : in Wisi.Query)
   is
      --  See wisi-parse-common.el wisi-parse-tree query for definition of
      --  queries.
      use Syntax_Trees;
   begin
      if Trace_Action > Outline then
         Tree.Lexer.Trace.Put_Line ("post-parse tree:");
         Tree.Print_Tree
           (Non_Grammar  => True,
            Line_Numbers => True);
      end if;
      case Query.Label is
      when Point_Query =>
         declare
            --  IMPROVEME: if Query.Char_Point is in the whitespace between a
            --  token A non_grammar and the next grammar token B, this will return
            --  the next grammar token, which is not right; wisi-parse-tree says
            --  it should return A. Let's see if that's actually a problem. This
            --  is better then setting After False; then it would return
            --  Invalid_Node_Access.
            Terminal : constant Node_Access := Tree.Find_Char_Pos
              (Query.Char_Point,
               Trailing_Non_Grammar => True,
               After                => True);
         begin
            if Terminal = Invalid_Node_Access then
               Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Query.Label)'Image & " nil]");
               return;
            else
               declare
                  Result   : Node_Access :=
                    (case Point_Query'(Query.Label) is
                     when Node => Terminal,
                     when Containing_Statement => Tree.Find_Ancestor (Terminal, To_Array (Data.Statement_IDs)),
                     when Ancestor => Tree.Find_Ancestor (Terminal, To_Array (Query.IDs)));
                  Char_Region : Buffer_Region;
               begin
                  case Point_Query'(Query.Label) is
                  when Node | Ancestor =>
                     if Result = Invalid_Node_Access then
                        --  Node is tree.root, or ancestor not found.
                        Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Query.Label)'Image & " nil]");
                        return;
                     end if;

                     Char_Region := Tree.Char_Region (Result, Trailing_Non_Grammar => False);

                  when Containing_Statement =>
                     loop
                        if Result = Invalid_Node_Access then
                           --  ancestor not found.
                           Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Query.Label)'Image & " nil]");
                           return;
                        end if;

                        Char_Region := Tree.Char_Region (Result, Trailing_Non_Grammar => False);

                        if Query.Char_Point = Char_Region.First and
                          Tree.ID (Result) /= Tree.Lexer.Descriptor.Accept_ID
                        then
                           --  Find container of this statement
                           Result := Tree.Find_Ancestor (Result, To_Array (Data.Statement_IDs));
                        else
                           exit;
                        end if;
                     end loop;
                  end case;

                  Ada.Text_IO.Put_Line
                    ("[" & Query_Tree_Code &
                       Query_Label'Pos (Query.Label)'Image & " " &
                       Address_Image (Result) & " " &
                       Tree.ID (Result)'Image & " " &
                       Char_Region.First'Image & " " &
                       Buffer_Pos'Image (Char_Region.Last + 1) & -- Emacs region end convention.
                       "]");
               end;
            end if;
         end;

      when Parent | Child =>
         declare
            Result   : constant Node_Access :=
              (if Query.Label = Parent
               then Tree.Parent (Query.Node, Query.N)
               else Tree.Child (Query.Node, Positive_Index_Type (Query.N)));
            Char_Region : constant Buffer_Region :=
              (if Result = Invalid_Node_Access
               then Null_Buffer_Region
               else Tree.Char_Region (Result, Trailing_Non_Grammar => False));
         begin
            if Result = Invalid_Node_Access then
               raise Parse_Error with "previous parse failed; can't execute post_parse action.";
            end if;

            Ada.Text_IO.Put_Line
              ("[" & Query_Tree_Code &
                 Query_Label'Pos (Query.Label)'Image & " " &
                 Address_Image (Result) & " " &
                 Tree.ID (Result)'Image & " " &
                 Char_Region.First'Image & " " &
                 Char_Region.Last'Image & "]");
         end;

      when Print =>
         Tree.Print_Tree (Line_Numbers => True, Non_Grammar => True);
         if Tree.Editable then
            WisiToken.Parse.Put_Errors (Tree);
         else
            declare
               Stream : Stream_ID := Tree.First_Parse_Stream;
            begin
               loop
                  exit when Stream = Invalid_Stream_ID;
                  WisiToken.Parse.Put_Errors (Tree, Stream);
                  Tree.Next_Parse_Stream (Stream);
               end loop;
            end;
         end if;

      when Dump =>
         if Tree.Parents_Set then
            declare
               use Ada.Directories;
               File_Name : constant String := -Query.File_Name;
               Normalized_Tree : WisiToken.Syntax_Trees.Tree;
            begin
               if Exists (File_Name) then
                  Delete_File (File_Name);
               end if;

               WisiToken.Syntax_Trees.Copy_Tree
                 (Source      => Tree,
                  Destination => Normalized_Tree,
                  User_Data   => Syntax_Trees.User_Data_Access_Constant (Data));
               Normalized_Tree.Put_Tree (-Query.File_Name);
            end;
         end if;
      end case;
   end Query_Tree;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String)
   is
      pragma Unreferenced (Data);
   begin
      Ada.Text_IO.Put_Line ("[" & Language_Action_Code & Content & "]");
   end Put_Language_Action;

   procedure Put (Data : in out Parse_Data_Type; Parser : in Parse.Base_Parser'Class)
   is
      Tree : WisiToken.Syntax_Trees.Tree renames Parser.Tree;
   begin
      --  +1 to match Emacs region
      Ada.Text_IO.Put_Line
        ('[' & End_Code & Buffer_Pos'Image (Tree.Char_Region (Tree.EOI, Trailing_Non_Grammar => False).Last + 1) & ']');

      --  Caches are populated by Execute_Actions.
      case Data.Post_Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            if Contains (Data.Action_Region_Chars, Cache.Pos) then
               Put (Cache);
            end if;
         end loop;
         Data.Navigate_Caches.Clear;

         for Cache of Data.Name_Caches loop
            if Contains (Outer => Data.Action_Region_Chars, Inner => Cache) then
               Put (Cache);
            end if;
         end loop;
         Data.Name_Caches.Clear;

      when Face =>
         for Cache of Data.Face_Caches loop
            if Overlaps (Cache.Char_Region, Data.Action_Region_Chars) then
               Put (Cache);
            end if;
         end loop;
         Data.Face_Caches.Clear;

      when Indent =>
         Resolve_Anchors (Data, Tree);
         for Line in Data.Action_Region_Lines.First .. Data.Action_Region_Lines.Last loop
            Put (Tree, Line, Data.Indents (Line));
         end loop;
         Data.Indents.Clear;
      end case;
   end Put;

   procedure Put (Item : in WisiToken.Parse.Lexer_Error)
   is begin
      Ada.Text_IO.Put_Line
        ('[' & Lexer_Error_Code & Buffer_Pos'Image (Item.Error.Char_Pos) &
           " ""lexer error" &
           (if Item.Error.Recover_Char (1) = ASCII.NUL
            then """"
            elsif Item.Error.Recover_Char (1) = '"'
            then """ ?\"""
            else """ ?" & Item.Error.Recover_Char (1)) &
           "]");
      if Item.Error.Recover_Char (2) /= ASCII.NUL then
         raise SAL.Programmer_Error with "lexer error with non-ascii or multiple repair char";
      end if;
   end Put;

   procedure Put_Errors (Tree : in Syntax_Trees.Tree)
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Text_IO;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      function Safe_Pos (Token : in Syntax_Trees.Node_Access) return Buffer_Pos
      is begin
         if Token = Syntax_Trees.Invalid_Node_Access then
            return Buffer_Pos'First;
         else
            declare
               Result : constant Buffer_Region := Tree.Char_Region (Token, Trailing_Non_Grammar => False);
            begin
               if Result = Null_Buffer_Region then
                  return Buffer_Pos'First;
               else
                  return Result.First;
               end if;
            end;
         end if;
      end Safe_Pos;

      procedure Handle_Error
        (Err        : in Syntax_Trees.Error_Data'Class;
         Error_Node : in Syntax_Trees.Valid_Node_Access)
      is
         Error_Pos : constant Buffer_Pos := Tree.Char_Region (Error_Node, Trailing_Non_Grammar => False).First;

         procedure Put_Recover (Item : in WisiToken.Parse.Recover_Op_Nodes_Arrays.Vector)
         is begin
            Put (Item, Error_Pos, Tree);
            if Trace_Action > Outline  or WisiToken.Debug_Mode then
               Tree.Lexer.Trace.Put_Line ("recover: " & Parse.Image (Item, Tree));
            end if;
         end Put_Recover;

      begin
         if Err in WisiToken.Parse.Lexer_Error then
            Put (WisiToken.Parse.Lexer_Error (Err));

         elsif Err in WisiToken.Parse.Parse_Error then
            declare
               Item : WisiToken.Parse.Parse_Error renames WisiToken.Parse.Parse_Error (Err);
            begin
               Put_Line
                 ('[' & Parser_Error_Code & Base_Buffer_Pos'Image (Safe_Pos (Error_Node)) &
                    " ""syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                    ", found '" & Image (Tree.ID (Error_Node), Descriptor) & "'""]");
               Put_Recover (Item.Recover_Ops);
            end;

         elsif Err in WisiToken.Parse.In_Parse_Action_Error then
            declare
               use all type WisiToken.Syntax_Trees.In_Parse_Actions.Status_Label;
               Item : WisiToken.Parse.In_Parse_Action_Error renames WisiToken.Parse.In_Parse_Action_Error
                 (Err);
            begin
               Put_Line
                 ('[' & In_Parse_Action_Error_Code & Integer'Image
                    (Syntax_Trees.In_Parse_Actions.Status_Label'Pos (Item.Status.Label)) &
                    (case Item.Status.Label is
                     when WisiToken.Syntax_Trees.In_Parse_Actions.Ok => "",
                     when WisiToken.Syntax_Trees.In_Parse_Actions.Error =>
                        Safe_Pos (Tree.Child (Error_Node, Item.Status.Begin_Name))'Image &
                          Safe_Pos (Tree.Child (Error_Node, Item.Status.End_Name))'Image & " """ &
                          (case WisiToken.Syntax_Trees.In_Parse_Actions.Error'(Item.Status.Label) is
                           when Missing_Name_Error => "missing",
                           when Extra_Name_Error => "extra",
                           when Match_Names_Error => "match") &
                          " name error""]"));
               Put_Recover (Item.Recover_Ops);
            end;

         elsif Err in WisiToken.Parse.Error_Message then
            --  FIXME: convert moved In_Parse_Action_Error to In_Parse_Action_Error_Code?
            declare
               Item : WisiToken.Parse.Error_Message renames WisiToken.Parse.Error_Message (Err);
            begin
               Put_Line
                 ('[' & Parser_Error_Code & Buffer_Pos'Image (Buffer_Pos'First) &
                    " """ & (-Item.Msg) & """]");
               Put_Recover (Item.Recover_Ops);
            end;
         end if;
      end Handle_Error;

   begin
      if Tree.Is_Empty then
         --  No errors.
         if WisiToken.Debug_Mode then
            Tree.Lexer.Trace.Put_Line ("empty tree");
         end if;

      elsif Tree.Editable then
         for Err_Ref in Tree.Error_Iterate loop
            Handle_Error (Syntax_Trees.Error (Err_Ref), Tree.Error_Node (Err_Ref));
         end loop;
      else
         for Err_Cur in Tree.Stream_Error_Iterate
           ((if Tree.Stream_Count >= 2
             then Tree.First_Parse_Stream
             else Tree.Shared_Stream))
         loop
            declare
               Stream_Err_Ref : constant Syntax_Trees.Stream_Error_Ref := Syntax_Trees.Error (Err_Cur);
            begin
               Handle_Error (Syntax_Trees.Error (Stream_Err_Ref), Tree.Error_Node (Stream_Err_Ref));
            end;
         end loop;
      end if;
   end Put_Errors;

   procedure Put_Error
     (Tree        : in Syntax_Trees.Tree;
      Line_Number : in Line_Number_Type;
      Message     : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (Tree.Lexer.File_Name, Line_Number, 0, Message) & """)");
   end Put_Error;

   ----------
   --  Spec visible private subprograms, alphabetical

   function Compute_Indenting
     (Data : in Parse_Data_Type'Class;
      Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Wisi.Indenting
   is
      Aug : constant Augmented_Access := Get_Augmented (Tree, Node);
   begin
      if Aug.Cache_Version = Data.Augmented_Cache_Version then
         return Aug.Indenting;
      end if;

      declare
         use all type Ada.Containers.Count_Type;
         use all type SAL.Base_Peek_Type;
         use Syntax_Trees;
         use Lexer;

         --  The precondition guarrantees Prev_Non_Grammar and Next_Non_Grammar exist.

         Prev_Non_Grammar  : constant Valid_Node_Access := Tree.Prev_Non_Grammar (Node);
         Next_Non_Grammar  : constant Valid_Node_Access := Tree.Next_Non_Grammar (Node);
         Prev_Terminal     : constant Valid_Node_Access := Tree.Prev_Terminal (Node);
         First_Non_Grammar : constant Node_Access       := Tree.First_Non_Grammar (Node);
         Last_Terminal     : constant Node_Access       := Tree.Last_Terminal (Node);

         function Has_New_Line (Node : in Valid_Node_Access) return Boolean
         is begin
            return (for some Token of Tree.Non_Grammar_Const (Node) =>
                      Contains_New_Line (Token.Line_Region));
         end Has_New_Line;

         function Get_Last (Node : in Valid_Node_Access) return Base_Line_Number_Type
         is
            Non_Grammar  : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Node);
         begin
            if Non_Grammar.Length = 0 then
               return Invalid_Line_Number;
            else
               return Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
            end if;
         end Get_Last;

         function Get_First (Node : in Valid_Node_Access) return Base_Line_Number_Type
         is
            Non_Grammar : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Node);
         begin
            if Non_Grammar.Length = 0 then
               return Invalid_Line_Number;
            else
               return Non_Grammar (Non_Grammar.First_Index).Line_Region.First;
            end if;
         end Get_First;

         First_Code_Line : constant Base_Line_Number_Type :=
           --  Correct even if not first in line.
           (if Prev_Non_Grammar = Prev_Terminal and Has_New_Line (Prev_Non_Grammar)
            then  --  First terminal in Node is first on a line
               Get_Last (Prev_Non_Grammar)
            elsif First_Non_Grammar = Invalid_Node_Access
            then Invalid_Line_Number
            elsif Last_Terminal = Invalid_Node_Access
            then Invalid_Line_Number -- No grammar terminals after first_non_grammar
            else Get_First (First_Non_Grammar));

         Last_Code_Line : constant Base_Line_Number_Type :=
           (if Last_Terminal = Invalid_Node_Access
            then Get_First (Next_Non_Grammar)
            elsif Get_First (Last_Terminal) = Invalid_Line_Number
            then Get_First (Next_Non_Grammar)
            else Get_First (Last_Terminal));

      begin
         return Result : Wisi.Indenting do
            if Last_Terminal = Invalid_Node_Access then
               --  Node is an empty nonterm
               Result.Code    := Null_Line_Region;
               Result.Comment := Null_Line_Region;
            else
               if First_Code_Line = Invalid_Line_Number or Last_Code_Line = Invalid_Line_Number then
                  Result.Code := Null_Line_Region;
               else
                  if Prev_Non_Grammar = Prev_Terminal then
                     --  First terminal in Node is first on line.
                     Result.Code := (First_Code_Line, Last_Code_Line);

                  elsif First_Code_Line = Last_Code_Line then
                     --  Not first on line, none on next line
                     Result.Code := Null_Line_Region;
                  else
                     --  Not first on line, some on next line
                     Result.Code :=
                       (First => First_Code_Line + 1,
                        Last  => Last_Code_Line);
                  end if;
               end if;

               if Last_Terminal = Invalid_Node_Access then
                  Result.Comment := Null_Line_Region;

               else
                  declare
                     Trailing_Non_Grammar : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Last_Terminal);
                  begin
                     if Trailing_Non_Grammar.Length in 0 | 1 then
                        --  Single non_grammar either contains a single new_line, a new_line comment, or a
                        --  non-new_line comment (ie placeholder); none need indenting.
                        --  FIXME: handle multi-line comments
                        Result.Comment := Null_Line_Region;

                     else
                        if Contains_New_Line (Trailing_Non_Grammar (Trailing_Non_Grammar.First_Index).Line_Region)
                        then
                           --  First non_grammar terminates code line.
                           Result.Comment.First := Trailing_Non_Grammar
                             (Trailing_Non_Grammar.First_Index).Line_Region.Last;
                        else
                           --  First non_grammar is a block comment (ie placeholder) on the code
                           --  line; find first blank or comment line, if any.
                           declare
                              First_Line : constant Line_Number_Type := Trailing_Non_Grammar
                                (Trailing_Non_Grammar.First_Index).Line_Region.Last;
                           begin
                              for I in Trailing_Non_Grammar.First_Index + 1 .. Trailing_Non_Grammar.Last_Index loop
                                 if Trailing_Non_Grammar (I).Line_Region.First /= First_Line then
                                    Result.Comment.First := Trailing_Non_Grammar (I).Line_Region.First;
                                    exit;
                                 end if;
                              end loop;
                           end;
                        end if;

                        Result.Comment.Last :=
                          Trailing_Non_Grammar (Trailing_Non_Grammar.Last_Index).Line_Region.First;
                     end if;
                  end;
               end if;
            end if;

            Aug.Cache_Version := Data.Augmented_Cache_Version;
            Aug.Indenting     := Result;
         end return;
      end;
   end Compute_Indenting;

   function Get_Augmented
     (Tree : in Syntax_Trees.Tree'Class;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Augmented_Access
   is
      Aug : Augmented_Access := Augmented_Access (Tree.Augmented (Node));
   begin
      if Aug = null then
         Aug := new Augmented;
         Tree.Set_Augmented (Node, Syntax_Trees.Augmented_Class_Access (Aug));
      end if;
      return Aug;
   end Get_Augmented;

   function Image (Item : in Simple_Delta_Type) return String
   is begin
      return "(" & Trimmed_Image (Item.Controlling_Token_Line) & ": " & Simple_Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Int => Integer'Image (Item.Int_Delta),
         when Anchored => Item.Anchor_Line'Image & Item.Anchored_Delta'Image)
        & ")";
   end Image;

   function Image (Item : in Delta_Type) return String
   is begin
      return "(" & Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when Simple => " " & Image (Item.Simple_Delta),
         when Hanging => Line_Number_Type'Image (Item.Hanging_First_Line) &
           " " & Image (Item.Hanging_Delta_1) & " " & Image (Item.Hanging_Delta_2)) & ")";
   end Image;

   function Current_Indent_Offset
     (Tree         : in Syntax_Trees.Tree'Class;
      Anchor_Token : in Syntax_Trees.Valid_Node_Access;
      Offset       : in Integer)
     return Integer
   is
      Line_Begin_Token : constant Syntax_Trees.Node_Access := Tree.Line_Begin_Token
        (Tree.Line_Region (Anchor_Token, Trailing_Non_Grammar => True).First);
   begin
      return Offset + Integer
        (Tree.Char_Region (Anchor_Token, Trailing_Non_Grammar => False).First -
           (if Line_Begin_Token = WisiToken.Syntax_Trees.Invalid_Node_Access
            then 0
            else Tree.Char_Region (Line_Begin_Token, Trailing_Non_Grammar => False).First));
   end Current_Indent_Offset;

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return String
   is
      use all type Syntax_Trees.Node_Label;
   begin
      case Tree.Label (Tree_Index) is
      when Source_Terminal | Nonterm =>
         return Tree.Lexer.Buffer_Text (Tree.Byte_Region (Tree_Index, Trailing_Non_Grammar => False));

      when Virtual_Terminal | Virtual_Identifier =>
         raise SAL.Programmer_Error;

      end case;
   end Get_Text;

   function Elisp_Escape_Quotes (Item : in String) return String
   is
      Result : String (Item'First .. Item'First + Item'Length * 2);
      Last   : Integer := Item'First - 1;
   begin
      for I in Item'Range loop
         if Item (I) = '"' then
            Last := Last + 1;
            Result (Last) := '\';
         end if;
         Last := Last + 1;
         Result (Last) := Item (I);
      end loop;
      return Result (Result'First .. Last);
   end Elisp_Escape_Quotes;

   function Indent_Anchored_2
     (Data              : in Parse_Data_Type'Class;
      Tree              : in Syntax_Trees.Tree;
      Anchor_Token      : in Syntax_Trees.Valid_Node_Access;
      Indenting_Token   : in Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in Boolean;
      Offset            : in Integer)
     return Delta_Type
   is
      Anchor_Line    : constant Line_Number_Type := Tree.Line_Region
        (Anchor_Token, Trailing_Non_Grammar => True).First;
      Indenting_Line : constant Line_Number_Type :=
        (if Indenting_Comment
         then Compute_Indenting (Data, Tree, Indenting_Token).Comment.First
         else Tree.Line_Region (Indenting_Token, Trailing_Non_Grammar => True).Last);
   begin
      if Anchor_Line = Indenting_Line then
         --  test/ada_mode-interactive_1.adb:
         --  E := (1 =>
         --          'A');
         --
         --  The expression is anchored to itself, which is needed for
         --  multi-line expressions (ada_annex_p.wy assoc_expression).
         --  FIXME: need clearer example; what is "the expression"?
         --  FIXME: need comment example.
         return Null_Delta;
      else
         return
           (Simple,
            (Anchored,
             Controlling_Token_Line => Anchor_Line,
             Anchor_Line            => Anchor_Line,
             Anchored_Delta         => Offset));
      end if;
   end Indent_Anchored_2;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     Syntax_Trees.Valid_Node_Access;
      Param             : in     Indent_Param;
      Indenting_Token   : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean)
     return Delta_Type
   is begin
      --  Evaluate wisi-anchored*, wisi-hanging*.
      case Param.Label is
      when Simple =>
         case Param.Param.Label is
         when None =>
            return Null_Delta;

         when Block =>
            return (Simple, (Int, Invalid_Line_Number, Param.Param.Int_Delta));

         when Int =>
            return
              (Simple,
               (Int,
                Tree.Line_At_Node (Indenting_Token),
                Param.Param.Int_Delta));

         when Simple_Param_Anchored =>
            --  [2] wisi-anchored, wisi-anchored%
            declare
               Anchor_Token  : constant Syntax_Trees.Valid_Node_Access := Tree.Child
                 (Nonterm, Param.Param.Anchored_Index);
            begin
               return Indent_Anchored_2
                 (Data, Tree, Anchor_Token, Indenting_Token, Indenting_Comment,
                  Offset         =>
                    (case Simple_Param_Anchored'(Param.Param.Label) is
                     when Anchored_0 =>
                        --  test/ada_mode-interactive_2.adb 'if (A and B -- Comment 1'
                        Current_Indent_Offset (Tree, Anchor_Token, Param.Param.Anchored_Delta),
                     when Anchored_1 =>
                        Paren_In_Anchor_Line (Data, Tree, Anchor_Token, Param.Param.Anchored_Delta)));
            end;

         when Language =>
            return Param.Param.Function_Ptr
              (Data, Tree, Nonterm, Indenting_Token, Indenting_Comment, Param.Param.Args);
         end case;

      when Hanging_Label =>
         return Indent_Hanging_1
           (Data, Tree, Nonterm, Indenting_Token, Indenting_Comment, Param.Hanging_Delta_1,
            Param.Hanging_Delta_2, Param.Label);
      end case;
   end Indent_Compute_Delta;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Line_Region       : in     WisiToken.Line_Region;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Indenting_Comment_Label;
      Controlling_Delta : in     Delta_Type := Null_Delta)
   is
      Indent : Boolean := True;
   begin
      if Trace_Action > Detail then
         Tree.Lexer.Trace.Put_Line
           ("indent_token_1:      " &
              Image (Line_Region) & " " & Image (Delta_Indent) &
              (if Indenting_Comment /= None then " comment" else " code"));
      end if;

      for Line in Line_Region.First .. Line_Region.Last loop
         if Data.Indent_Comment_Col_0 then
            Indent := True;
            declare
               use all type Ada.Containers.Count_Type;
               use all type Ada.Text_IO.Count;
               Line_Begin_Char_Pos : Buffer_Pos;

               Containing : constant Syntax_Trees.Valid_Node_Access := Tree.Find_New_Line
                 (Line, Line_Begin_Char_Pos);
               --  Line_Begin_Char_Pos is either in a multi-line grammar token, or a
               --  non_grammar token.

               Non_Grammar : WisiToken.Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const (Containing);
            begin
               if Non_Grammar.Length > 0 and then Line_Begin_Char_Pos in
                 Non_Grammar (Non_Grammar.First_Index).Byte_Region.First ..
                   Non_Grammar (Non_Grammar.Last_Index).Byte_Region.Last
               then
                  for Tok of Non_Grammar loop
                     if Tok.Line_Region.First = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       Lexer.Column (Tok, Line_Begin_Char_Pos) = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if not Indent then
                  Indent_Line
                    (Data, Line, (Simple, (Int, Invalid_Line_Number, 0)), Indenting_Comment, Tree.Lexer.Trace);
               end if;
            end;
         end if;

         if Indent then
            if Indenting_Comment /= None and Data.Indents (Line).Label = Not_Set then
               --  In ada_mode-conditional_expressions.adb, case expression for K,
               --  comment before "(if J > 42", the if expression is indented by
               --  ada-indent-aggregate, which returns -1. We need to apply that to
               --  the comment also.
               --
               --  However, in ada_mode-nominal.adb, line "-- Comment before 'end
               --  case'", we don't want to add Controlling_Delta; that applies the
               --  same indent twice.
               Indent_Line (Data, Line, Controlling_Delta, Indenting_Comment, Tree.Lexer.Trace);
            end if;

            Indent_Line (Data, Line, Delta_Indent, Indenting_Comment, Tree.Lexer.Trace);
         end if;
      end loop;
   end Indent_Token_1;

   function Refactor_Parse  (Data : in Parse_Data_Type; Item : in String) return Refactor_Action
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Data);
   begin
      return Refactor_Action'Last;
   end Refactor_Parse;

end Wisi;
