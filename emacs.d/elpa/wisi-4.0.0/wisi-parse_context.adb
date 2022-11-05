--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 - 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Tags;
with GNAT.OS_Lib;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
package body Wisi.Parse_Context is

   function Source_File_Name (Item : in Parse_Context_Access) return String
   is (Ada.Strings.Unbounded.To_String (Item.File_Name));

   package File_Parse_Context_Maps is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Parse_Context_Access,
      Key_Type     => String,
      Key          => Source_File_Name,
      Key_Compare  => SAL.String_Compare);

   Map : File_Parse_Context_Maps.Tree;

   function Create_No_File
     (Language : in Wisi.Parse_Context.Language;
      Trace    : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is
      use WisiToken;
   begin
      return Result : constant Parse_Context_Access :=
        (new Parse_Context'
           (File_Name                         => +"",
            Text_Buffer                       => null,
            Text_Buffer_Byte_Last             => 0,
            Text_Buffer_Char_Last             => 0,
            Parser                            => WisiToken.Parse.LR.Parser.Parser'
              (Ada.Finalization.Limited_Controlled with
               User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
               Table                          => Language.Table,
               Productions                    => Language.Productions,
               Language_Fixes                 => Language.Fixes,
               Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
               Language_String_ID_Set         => Language.String_ID_Set,
               Partial_Parse_Active           => Language.Partial_Parse_Active,
               Partial_Parse_Byte_Goal        => Language.Partial_Parse_Byte_Goal,
               others                         => <>),
            Root_Save_Edited_Name             => <>,
            Save_Edited_Count                 => <>))
      do
         Result.Parser.Tree.Lexer := Language.Lexer;
         if Trace_Incremental_Parse > Outline then
            Trace.Put_Line
              ("parse_context (no file) created, language " & Ada.Tags.Expanded_Name
                 (Language.Parse_Data_Template.all'Tag));
            if Trace_Memory > Outline then
               Report_Memory (Trace.all, Prefix => True);
            end if;
         end if;
      end return;
   end Create_No_File;

   procedure Create_No_Text
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
   is
      Temp : constant Parse_Context_Access := Create_No_File (Language, Trace);
   begin
      Set_File (File_Name, Temp);
   end Create_No_Text;

   procedure Set_File (File_Name : in String; Parse_Context : in Parse_Context_Access)
   is
      use File_Parse_Context_Maps;
      use WisiToken;
      use Ada.Strings.Unbounded;
   begin
      if Length (Parse_Context.File_Name) > 0 then
         raise Protocol_Error;
      end if;

      Parse_Context.File_Name := +File_Name;
      Map.Insert (Parse_Context);
   end Set_File;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Trace_Incremental_Parse > Outline then
                  Trace.Put_Line ("parse_context found");
               end if;
            end return;
         end if;

         return Result : constant Parse_Context_Access :=
           (new Parse_Context'
              (File_Name                         => +File_Name,
               Text_Buffer                       => null,
               Text_Buffer_Byte_Last             => 0,
               Text_Buffer_Char_Last             => 0,
               Parser                            => WisiToken.Parse.LR.Parser.Parser'
                 (Ada.Finalization.Limited_Controlled with
                  User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
                  Table                          => Language.Table,
                  Productions                    => Language.Productions,
                  Language_Fixes                 => Language.Fixes,
                  Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
                  Language_String_ID_Set         => Language.String_ID_Set,
                  Partial_Parse_Active           => Language.Partial_Parse_Active,
                  Partial_Parse_Byte_Goal        => Language.Partial_Parse_Byte_Goal,
                  others                         => <>),
               Root_Save_Edited_Name             => <>,
               Save_Edited_Count                 => <>))
         do
            Result.Parser.Tree.Lexer := Language.Lexer;
            Map.Insert (Result);
            if Trace_Incremental_Parse > Outline then
               Trace.Put_Line
                 ("parse_context created, language " & Ada.Tags.Expanded_Name (Language.Parse_Data_Template.all'Tag));
               if Trace_Memory > Outline then
                  Report_Memory (Trace.all, Prefix => True);
               end if;
            end if;
         end return;
      end;
   end Find_Create;

   function Find
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Have_Text : in Boolean := False)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;
         use all type Ada.Strings.Unbounded.String_Access;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Have_Text and (Result.Text_Buffer = null or else Result.Text_Buffer'Length = 0) then
                  if Trace_Incremental_Parse > Outline then
                     Result.Parser.Tree.Lexer.Trace.Put_Line ("parse_context found, but text buffer empty");
                  end if;
                  raise Not_Found;
               end if;
               if Trace_Incremental_Parse > Outline then
                  Result.Parser.Tree.Lexer.Trace.Put_Line
                    ("parse_context found" & (if Have_Text then " and text present" else ""));
               end if;
            end return;
         else
            raise Not_Found;
         end if;
      end;
   end Find;

   procedure Kill (File_Name : in String)
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if not Has_Element (Found) then
            --  already killed, or never opened
            null;
         else
            declare
               Context : Parse_Context_Access := Element (Found);
            begin
               Map.Delete (File_Name);
               Ada.Strings.Unbounded.Free (Context.Text_Buffer);
               Free (Context);
            end;
         end if;
      end;
   end Kill;

   procedure Clear
   is begin
      Map.Clear;
   end Clear;

   function Image (Item : in Change) return String
   is
      use WisiToken;
   begin
      return "(" &
        Item.Begin_Byte_Pos'Image & "," &
        Item.Begin_Char_Pos'Image & "," &
        Item.Inserted_End_Byte_Pos'Image & "," &
        Item.Inserted_End_Char_Pos'Image & "," &
        " +""" & (-Item.Inserted_Text) & """," &
        Item.Deleted_Bytes'Image & "," &
        Item.Deleted_Chars'Image & ")";
   end Image;

   function Get_Emacs_Change_List
     (Command_Line : in     String;
      Last         : in out Integer)
     return Change_Lists.List
   is
      function Substitute_Escapes (Item : in String) return String
      is begin
         if Item'Length = 0 then
            return Item;
         else
            declare
               I      : Integer := Item'First;
               J      : Integer := Item'First;
               Result : String (Item'Range);
            begin
               loop
                  --  See test_edit_source.adb String_Escape for rationale of what is
                  --  handled here.
                  if Item (I) = '\' and I < Item'Last then
                     if Item (I + 1) = 'n' then
                        Result (J) := ASCII.LF;
                        I := @ + 2;
                     elsif Item (I + 1) = '"' then
                        Result (J) := '"';
                        I := @ + 2;
                     elsif Item (I + 1) = '\' then
                        Result (J) := '\';
                        I := @ + 2;
                     else
                        Result (J) := Item (I);
                        I := @ + 1;
                     end if;
                  else
                     Result (J) := Item (I);
                     I := @ + 1;
                  end if;
                  exit when I > Item'Last;
                  J := @ + 1;
               end loop;
               return Result (Result'First .. J);
            end;
         end if;
      end Substitute_Escapes;

   begin
      return Result : Change_Lists.List do
         Skip (Command_Line, Last, '('); --  start of changes list
         loop
            exit when Last = Command_Line'Last;
            exit when Command_Line (Last + 1) = ')';

            declare
               use WisiToken;
               Item : Change;
            begin
               Skip (Command_Line, Last, '(');
               Item.Begin_Byte_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Begin_Char_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Byte_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Char_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Deleted_Bytes         := Get_Integer (Command_Line, Last);
               Item.Deleted_Chars         := Get_Integer (Command_Line, Last);
               Item.Inserted_Text         := +Substitute_Escapes (Get_String (Command_Line, Last));
               Skip (Command_Line, Last, ')');

               if Integer (Item.Inserted_End_Byte_Pos - Item.Begin_Byte_Pos) /=
                 Ada.Strings.Unbounded.Length (Item.Inserted_Text)
               then
                  raise Protocol_Error with "invalid change; begin, end does not match inserted_text length" &
                    Item.Begin_Byte_Pos'Image & Item.Inserted_End_Byte_Pos'Image & Integer'Image
                      (Ada.Strings.Unbounded.Length (Item.Inserted_Text));
               end if;
               Result.Append (Item);
            end;
         end loop;
         Skip (Command_Line, Last, ')'); --  end of edits list
      end return;
   end Get_Emacs_Change_List;

   procedure Edit_Source
     (Trace         : in out WisiToken.Trace'Class;
      Parse_Context : in out Wisi.Parse_Context.Parse_Context;
      Changes       : in     Change_Lists.List;
      KMN_List      :    out WisiToken.Parse.KMN_Lists.List)
   is
      use Ada.Containers;
      use WisiToken;

      --  Changes is in increasing time order (ie _not_ in buffer pos
      --  order); KMN_List is in buffer pos order.

      Source           : Ada.Strings.Unbounded.String_Access renames Parse_Context.Text_Buffer;
      Source_Byte_Last : Integer renames Parse_Context.Text_Buffer_Byte_Last;
      Source_Char_Last : Integer renames Parse_Context.Text_Buffer_Char_Last;

      Initial_Text_Byte_Region : constant Buffer_Region := (1, Base_Buffer_Pos (Source_Byte_Last));
      Initial_Text_Char_Region : constant Buffer_Region := (1, Base_Buffer_Pos (Source_Char_Last));

      Gap_First : Integer := Source_Byte_Last + 1;
      Gap_Last  : Integer := Source'Last;

      function Gap_Invariant return Boolean
      is (Gap_Last - Gap_First = Source'Last - (Source_Byte_Last + 1));

      Total_Inserted_Bytes : Integer := 0;

      function Reallocate return Boolean
      is begin
         --  This is a conservative analysis; Total_Inserted_Bytes is correct
         --  if all Changes are inserted with no overlap and no deletes;
         --  otherwise it is too large. But the savings from being more
         --  accurate are not large, and this simplifies the editing algorithm.
         if Changes.Length = 0 then
            return False;
         end if;

         for Change of Changes loop
            Total_Inserted_Bytes := @ + Ada.Strings.Unbounded.Length (Change.Inserted_Text);
         end loop;

         if Source_Byte_Last + Total_Inserted_Bytes > Source'Last then
            return True;
         else
            return False;
         end if;
      end Reallocate;

      procedure Move_Gap (New_Gap_First : in Integer)
      with Pre =>
        New_Gap_First /= Gap_First and Gap_Invariant and
        (if New_Gap_First < Gap_First
         then
            (New_Gap_First + Gap_Last - Gap_First) + 1 in Source'First .. Source'Last and
              New_Gap_First in Source'First .. Source'Last
         else
            New_Gap_First - 1 in Source'First .. Source'Last and
              Gap_Last + 1 in Source'First .. Source'Last),
        Post => Gap_Invariant
      is
         --  Examples:
         --  gap_first : 15
         --  gap_last  : 19
         --
         --  new_gap_first: 5
         --     new_gap_last := 9
         --     source (10 .. 19) := source (5 .. 14)
         --
         --  new_gap_first: 25
         --  new_gap_last : 29
         --      source (15 .. 24) := source (20 .. 29)

         New_Gap_Last : constant Integer := New_Gap_First + Gap_Last - Gap_First;
      begin
         if New_Gap_First < Gap_First then
            Source (New_Gap_Last + 1 .. Gap_Last) := Source (New_Gap_First .. Gap_First - 1);
         else
            Source (Gap_First .. New_Gap_First - 1) := Source (Gap_Last + 1 .. New_Gap_Last);
         end if;

         Gap_First := New_Gap_First;
         Gap_Last  := New_Gap_Last;
      end Move_Gap;

      procedure Edit_Text (Change : in Wisi.Parse_Context.Change)
      with Pre => Gap_Invariant, Post => Gap_Invariant
      --  Apply Change to Source. Leaves Gap at edit point.
      is
         use Ada.Strings.Unbounded;
         Inserted_Bytes : constant Integer := Ada.Strings.Unbounded.Length (Change.Inserted_Text);
      begin
         if Gap_First /= Integer (Change.Begin_Byte_Pos) then
            Move_Gap (Integer (Change.Begin_Byte_Pos));
         end if;

         if Change.Deleted_Bytes > 0 then
            Gap_Last         := @ + Change.Deleted_Bytes;
            pragma Assert (Gap_Last <= Source'Last);
            Source_Byte_Last := @ - Change.Deleted_Bytes;
            Source_Char_Last := @ - Change.Deleted_Chars;
         end if;

         if Inserted_Bytes > 0 then
            pragma Assert (Gap_Last + 1 - Gap_First >= Inserted_Bytes);
            Source (Gap_First .. Gap_First + Inserted_Bytes - 1) := -Change.Inserted_Text;

            Gap_First        := Gap_First + Inserted_Bytes;
            Source_Byte_Last := @ + Inserted_Bytes;
            Source_Char_Last := @ + Integer (Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos);
         end if;
      end Edit_Text;

      procedure Delete_KMNs
        (KMN_Last_Byte     : in     Zero_Buffer_Pos;
         KMN_Last_Char     : in     Zero_Buffer_Pos;
         After             : in     Parse.KMN_Lists.Cursor;
         Last_Deleted_Byte : in     Buffer_Pos;
         Last_Deleted_Char : in     Buffer_Pos;
         KMN               : in out Parse.KMN)
      --  Last_Deleted_Byte is deleted from current text by current
      --  Change. Delete KMNs after After whose Stable are entirely within
      --  Last_Deleted_Byte; merge into KMN (initially the current Change).
      --  Adjust following KMN if stable contains Last_Deleted_Byte.
      is
         use Parse.KMN_Lists;
         use all type Parse.KMN;

         Last_Byte : Zero_Buffer_Pos := KMN_Last_Byte + KMN_List (After).Stable_Bytes +
           KMN_List (After).Inserted_Bytes; -- end of After KMN and subsequent deleted KMN
         Last_Char : Zero_Buffer_Pos := KMN_Last_Char + KMN_List (After).Stable_Chars +
           KMN_List (After).Inserted_Chars;

         Cur : Cursor := Next (After);
      begin
         loop
            exit when not Has_Element (Cur);
            if Last_Byte + KMN_List (Cur).Stable_Bytes + KMN_List (Cur).Inserted_Bytes <=
              Last_Deleted_Byte
            then
               --  All of cur inserted are deleted, and some of next.
               --  test_edit_source.adb Edit_06.
               KMN.Deleted_Bytes := @ + KMN_List (Cur).Deleted_Bytes - KMN_List (Cur).Inserted_Bytes;
               KMN.Deleted_Chars := @ + KMN_List (Cur).Deleted_Chars - KMN_List (Cur).Inserted_Chars;

               Last_Byte := @ + KMN_List (Cur).Stable_Bytes + KMN_List (Cur).Inserted_Bytes;
               Last_Char := @ + KMN_List (Cur).Stable_Chars + KMN_List (Cur).Inserted_Chars;

               declare
                  To_Delete : Cursor := Cur;
               begin
                  Cur := Next (Cur);
                  KMN_List.Delete (To_Delete);
               end;

            elsif Last_Byte + KMN_List (Cur).Stable_Bytes <= Last_Deleted_Byte then
               --  Some of Cur.inserted are deleted. test_edit_source.adb Edit_05.
               declare
                  Deleted_Bytes : constant Zero_Buffer_Pos := Last_Deleted_Byte -
                    (Last_Byte + KMN_List (Cur).Stable_Bytes); -- bytes of cur.inserted that are deleted
                  Deleted_Chars : constant Zero_Buffer_Pos := Last_Deleted_Char -
                    (Last_Char + KMN_List (Cur).Stable_Chars);
               begin
                  KMN.Inserted_Bytes := @ - Deleted_Bytes + KMN_List (Cur).Inserted_Bytes;
                  KMN.Inserted_Chars := @ - Deleted_Chars + KMN_List (Cur).Inserted_Chars;

                  KMN.Deleted_Bytes := @ + KMN_List (Cur).Deleted_Bytes - Deleted_Bytes;
                  KMN.Deleted_Chars := @ + KMN_List (Cur).Deleted_Chars - Deleted_Chars;

                  KMN_List.Delete (Cur);
                  exit;
               end;
            else
               --  Last_Byte is in Cur.stable
               KMN_List (Cur).Stable_Bytes := @ - (Last_Deleted_Byte - Last_Byte);
               KMN_List (Cur).Stable_Chars := @ - (Last_Deleted_Char - Last_Char);

               if KMN_List (Cur) = (others => 0) then
                  KMN_List.Delete (Cur);
               end if;
               exit;
            end if;
         end loop;
      end Delete_KMNs;

      procedure Edit_KMN (Change : in Wisi.Parse_Context.Change)
      --  Apply Change to KMN list
      is
         use Parse.KMN_Lists;
         use all type Parse.KMN;

         Cur : Cursor := KMN_List.First;

         KMN_Last_Byte : Base_Buffer_Pos := 0; --  Last byte of prev KMN.
         KMN_Last_Char : Base_Buffer_Pos := 0; --  Last char of prev KMN.

         function To_KMN (Item : in Wisi.Parse_Context.Change) return Parse.KMN
         --  Assuming Change does not overlap any current KMN non-stable,
         --  return a new KMN for it.
         is (Stable_Bytes   => Item.Begin_Byte_Pos - KMN_Last_Byte - 1, -- Begin_Byte_Pos is deleted or inserted
             Stable_Chars   => Item.Begin_Char_Pos - KMN_Last_Char - 1,
             Inserted_Bytes => Item.Inserted_End_Byte_Pos - Item.Begin_Byte_Pos, -- End_Byte_Pos is after last inserted
             Inserted_Chars => Item.Inserted_End_Char_Pos - Item.Begin_Char_Pos,
             Deleted_Bytes  => Base_Buffer_Pos (Item.Deleted_Bytes),
             Deleted_Chars  => Base_Buffer_Pos (Item.Deleted_Chars));

      begin
         loop
            declare
               Cur_KMN : Parse.KMN renames KMN_List (Cur);
               KMN     : Parse.KMN := To_KMN (Change);

               Cur_Last_Inserted_Byte : constant Base_Buffer_Pos :=
                 KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
               Cur_Last_Inserted_Char : constant Base_Buffer_Pos :=
                 KMN_Last_Char + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

               Change_Last_Deleted_Byte : constant Base_Buffer_Pos :=
                 Change.Begin_Byte_Pos + Base_Buffer_Pos (Change.Deleted_Bytes) - 1;

               Change_Last_Deleted_Char : constant Base_Buffer_Pos :=
                 Change.Begin_Char_Pos + Base_Buffer_Pos (Change.Deleted_Chars) - 1;
            begin
               pragma Assert (KMN_Last_Byte < Change.Begin_Byte_Pos);

               if Change.Begin_Byte_Pos + Base_Buffer_Pos (Change.Deleted_Bytes) - 1 <
                 KMN_Last_Byte + Cur_KMN.Stable_Bytes
               then
                  --  Change is entirely within Cur_KMN.Stable_Bytes;
                  --  test_edit_source.adb Edit_01
                  --
                  --  Or Change is inserting at end of text; Edit_10.
                  Cur_KMN.Stable_Bytes := @ - (KMN.Stable_Bytes + KMN.Deleted_Bytes);
                  Cur_KMN.Stable_Chars := @ - (KMN.Stable_Chars + KMN.Deleted_Chars);

                  if KMN_List (Cur) = (others => 0) then
                     Cur_KMN := KMN;
                  else
                     KMN_List.Insert (Before => Cur, Element => KMN);
                  end if;
                  exit;

               elsif Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change starts in or immediately after Cur_KMN.Stable_Bytes, ends
                  --  in or after Cur_KMN.Insert; merge Change into Cur_KMN.

                  if Cur_Last_Inserted_Byte >= Change_Last_Deleted_Byte then
                     --  Some of Cur_KMN.Inserted are preserved; test_edit_source.adb
                     --  Edit_02, _03, Deindent.
                     --
                     --   cur_kmn       next_kmn
                     --  stable|  ins| stable| ins| ...
                     --
                     --   change
                     --     | ins     |
                     --     | del  |

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes + Cur_Last_Inserted_Byte - Change_Last_Deleted_Byte;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars + Cur_Last_Inserted_Char - Change_Last_Deleted_Char;

                     Cur_KMN.Deleted_Bytes := @ + KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 - Change.Begin_Byte_Pos;
                     Cur_KMN.Deleted_Chars := @ + KMN_Last_Char + Cur_KMN.Stable_Chars + 1 - Change.Begin_Char_Pos;
                  else
                     --  All of Cur_KMN.Inserted and some of following KMN are deleted;
                     --  test_edit_source.adb Edit_04, _05, _06.

                     --  cur_kmn      next_kmn
                     --  stable|   ins| stable| ins| ...
                     --
                     --   change
                     --     | ins    |
                     --     | del                          |

                     Delete_KMNs
                       (KMN_Last_Byte, KMN_Last_Char, Cur,
                        Last_Deleted_Byte => Change.Begin_Byte_Pos + KMN.Deleted_Bytes - 1,
                        Last_Deleted_Char => Change.Begin_Char_Pos + KMN.Deleted_Chars - 1,
                        KMN               => KMN);

                     Cur_KMN.Deleted_Bytes := @ + KMN.Deleted_Bytes - Cur_KMN.Inserted_Bytes;
                     Cur_KMN.Deleted_Chars := @ + KMN.Deleted_Chars - Cur_KMN.Inserted_Chars;

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars;
                  end if;

                  Cur_KMN.Stable_Bytes := KMN.Stable_Bytes;
                  Cur_KMN.Stable_Chars := KMN.Stable_Chars;
                  exit;

               elsif Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes + 1 then
                  --  Change starts in or immediately after Cur_KMN inserted; merge
                  --  Change into Cur_KMN. test_edit_source.adb Edit_07, _08, _09,
                  --  Insert_Deindent

                  if Cur_Last_Inserted_Byte >= Change_Last_Deleted_Byte then
                     --  Beginning and end of Cur_KMN.Inserted are preserved; test_edit_source.adb
                     --  Edit_07.
                     --
                     --   cur_kmn          next_kmn
                     --  stable|  ins   | stable| ins| ...
                     --
                     --   change
                     --          | ins     |
                     --          | del|

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes + Cur_KMN.Inserted_Bytes - KMN.Deleted_Bytes;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars + Cur_KMN.Inserted_Chars - KMN.Deleted_Chars;

                     --  Cur_KMN.Deleted_Bytes unchanged
                  else
                     --  Remainder of Cur_KMN.Inserted and some of following KMN are deleted;
                     --  test_edit_source.adb Edit_08, _09

                     --  cur_kmn      next_kmn
                     --  stable|   ins| stable| ins| ...
                     --
                     --   change
                     --         | ins    |
                     --         | del                          |

                     Delete_KMNs
                       (KMN_Last_Byte, KMN_Last_Char, Cur,
                        Last_Deleted_Byte => Change.Begin_Byte_Pos + KMN.Deleted_Bytes - 1,
                        Last_Deleted_Char => Change.Begin_Char_Pos + KMN.Deleted_Chars - 1,
                        KMN               => KMN);

                     declare
                        Remaining_Cur_Ins_Bytes : constant Zero_Buffer_Pos :=
                          Change.Begin_Byte_Pos - (KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1);

                        Remaining_Cur_Ins_Chars : constant Zero_Buffer_Pos :=
                          Change.Begin_Char_Pos - (KMN_Last_Char + Cur_KMN.Stable_Chars + 1);
                     begin
                        Cur_KMN.Deleted_Bytes := @ + KMN.Deleted_Bytes -
                          (Cur_KMN.Inserted_Bytes - Remaining_Cur_Ins_Bytes);

                        Cur_KMN.Deleted_Chars := @ + KMN.Deleted_Chars -
                          (Cur_KMN.Inserted_Chars  - Remaining_Cur_Ins_Chars);

                        Cur_KMN.Inserted_Bytes := Remaining_Cur_Ins_Bytes + KMN.Inserted_Bytes;
                        Cur_KMN.Inserted_Chars := Remaining_Cur_Ins_Chars + KMN.Inserted_Chars;
                     end;
                  end if;

                  exit;

               else
                  --  Change is entirely after Cur_KMN
                  KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
                  KMN_Last_Char := @ + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

                  Next (Cur);

                  if not Has_Element (Cur) then
                     --  Since KMN_List starts with one KMN covering all of Source, we
                     --  should never get here.
                     raise SAL.Programmer_Error;
                  end if;
               end if;
            end;
         end loop;

         if Cur /= KMN_List.Last and then (KMN_List (Cur).Inserted_Bytes = 0 and KMN_List (Cur).Deleted_Bytes = 0) then
            --  Change undone; merge stable with next KMN. test_edit_source.adb Edit_11
            declare
               To_Delete : Cursor := Cur;
            begin
               Next (Cur);
               KMN_List (Cur).Stable_Bytes := @ + KMN_List (To_Delete).Stable_Bytes;
               KMN_List (Cur).Stable_Chars := @ + KMN_List (To_Delete).Stable_Chars;
               KMN_List.Delete (To_Delete);
            end;
         end if;

         if Debug_Mode then
            begin
               WisiToken.Parse.Validate_KMN
                 (List                     => KMN_List,
                  Initial_Text_Byte_Region => Initial_Text_Byte_Region,
                  Initial_Text_Char_Region => Initial_Text_Char_Region,
                  Edited_Text_Byte_Region  => Buffer_Region'(1, Base_Buffer_Pos (Source_Byte_Last)),
                  Edited_Text_Char_Region  => Buffer_Region'(1, Base_Buffer_Pos (Source_Char_Last)));
            exception
            when E : WisiToken.User_Error =>
               raise Protocol_Error with Ada.Exceptions.Exception_Message (E);
            end;
         end if;
      end Edit_KMN;

   begin
      if Reallocate then
         declare
            New_Source : constant Ada.Strings.Unbounded.String_Access := new String
              (Source'First .. Source_Byte_Last + Total_Inserted_Bytes);
         begin
            New_Source (Source'First .. Source_Byte_Last) := Source (Source'First .. Source_Byte_Last);
            Ada.Strings.Unbounded.Free (Source);
            Source := New_Source;
         end;

         Gap_Last := Source'Last;
      end if;

      --  Start with one KMN with stable region = entire source.
      KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Source_Byte_Last),
          Stable_Chars   => Base_Buffer_Pos (Source_Char_Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      for Change of Changes loop
         Edit_Text (Change);
         Edit_KMN (Change);

         if Trace_Incremental_Parse > Detail then
            Trace.Put_Line ("change:" & Image (Change));
            Trace.Put_Line ("kmn_list:");
            for KMN of KMN_List loop
               Trace.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
      end loop;

      if Gap_Last /= Source'Last then
         --  Remove the gap
         Source (Gap_First .. Source_Byte_Last) := Source (Gap_Last + 1 .. Source'Last);
      end if;
   end Edit_Source;

   procedure Save_Text
     (Context   : in Parse_Context;
      File_Name : in String)
   is
      use GNAT.OS_Lib;
      File : File_Descriptor;
      Written : Integer;
      pragma Unreferenced (Written);
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Directories.Delete_File (File_Name);
      end if;
      File := Create_New_File (File_Name, Fmode => Binary);
      Written := Write (File, Context.Text_Buffer (Context.Text_Buffer'First)'Address,
             N => Context.Text_Buffer_Byte_Last - Context.Text_Buffer'First + 1);
      --  Written /= N on disk full; we don't check for that, because there's
      --  nothing to do.
      Close (File);

      Context.Parser.Tree.Lexer.Trace.Put_Line ("text saved to '" & File_Name & "'");
   end Save_Text;

   procedure Save_Text_Auto (Context : in out Parse_Context)
   is begin
      Context.Save_Edited_Count := @ + 1;

      declare
         Save_File_Name : constant String :=
           Ada.Strings.Unbounded.To_String (Context.Root_Save_Edited_Name) & "_" &
           Wisi.Integer_Filled_Image (Item => Context.Save_Edited_Count, Width => 3);
      begin
         Save_Text (Context, Save_File_Name);
      end;
   end Save_Text_Auto;

end Wisi.Parse_Context;
