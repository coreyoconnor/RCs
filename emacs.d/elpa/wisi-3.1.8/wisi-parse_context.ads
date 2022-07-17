--  Abstract :
--
--  Parse context for one source file.
--
--  Copyright (C) 2020 - 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
package Wisi.Parse_Context is

   Not_Found : exception;

   type Language is record
      Descriptor              : WisiToken.Descriptor_Access_Constant;
      Lexer                   : WisiToken.Lexer.Handle;
      Table                   : WisiToken.Parse.LR.Parse_Table_Ptr;
      Productions             : WisiToken.Syntax_Trees.Production_Info_Trees.Vector;
      Partial_Parse_Active    : access Boolean;
      Partial_Parse_Byte_Goal : access WisiToken.Buffer_Pos;
      Fixes                   : WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Matching_Begin_Tokens   : WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      String_ID_Set           : WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Parse_Data_Template     : Wisi.Parse_Data_Access;
   end record;

   type Parse_Context is tagged limited record
      --  'tagged' for Object.Method notation

      File_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Text_Buffer : Ada.Strings.Unbounded.String_Access;
      --  Text_Buffer is encoded in UTF-8. Text_Buffer may hold all or part
      --  of the actual Emacs buffer content. If partial, the lexer holds
      --  the mapping from Text_Buffer index to Emacs buffer position.

      Text_Buffer_Byte_Last : Integer := Integer'First;
      Text_Buffer_Char_Last : Integer := Integer'Last;
      --  For Incremental parse; after editing, there may be empty space at
      --  the end of Text_Buffer.

      Parser : WisiToken.Parse.LR.Parser.Parser;

      Root_Save_Edited_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  If not "", save source text after the edit in a parse_incremental command,
      --  to <root_save_edited_name_nnn>, where 'nnn' is a three-digit number that
      --  increments.

      Save_Edited_Count : Integer := 0;
   end record;
   type Parse_Context_Access is access all Parse_Context;

   function Create_No_File
     (Language : in Wisi.Parse_Context.Language;
      Trace    : in WisiToken.Trace_Access)
     return Parse_Context_Access;

   procedure Create_No_Text
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access);

   procedure Set_File (File_Name : in String; Parse_Context : in Parse_Context_Access);

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access;
   --  If a context for File_Name exists, return it if Language matches.
   --
   --  If no context found for File_Name, create one, return it.
   --
   --  Raise Protocol_Error if Source_File_Name is an empty string.
   --
   --  Raise WisiToken.User_Error if context found for File_Name, but Language does not match.

   function Find
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Have_Text : in Boolean := False)
     return Parse_Context_Access;
   --  If a context for File_Name exists, return it if Language matches.
   --
   --  Raise Protocol_Error if Source_File_Name is an empty string.
   --
   --  Raise WisiToken.User_Error if context found for File_Name, but Language does not match.
   --
   --  Raise Not_Found if no context found for File_Name.
   --  If Have_Text, raise Not_Found if Text_Buffer is empty.

   procedure Kill (File_Name : in String);

   procedure Clear;
   --  Delete all contexts.

   type Change is record
      Begin_Byte_Pos        : WisiToken.Buffer_Pos; -- inserted or deleted
      Begin_Char_Pos        : WisiToken.Buffer_Pos;
      Inserted_End_Byte_Pos : WisiToken.Buffer_Pos;
      Inserted_End_Char_Pos : WisiToken.Buffer_Pos; --  emacs convention: end is after last inserted char
      Inserted_Text         : Ada.Strings.Unbounded.Unbounded_String;
      Deleted_Bytes         : Natural;
      Deleted_Chars         : Natural;
   end record;

   function Image (Item : in Change) return String;

   package Change_Lists is new Ada.Containers.Doubly_Linked_Lists (Change);

   function Get_Emacs_Change_List
     (Command_Line : in     String;
      Last         : in out Integer)
     return Change_Lists.List;

   procedure Edit_Source
     (Trace         : in out WisiToken.Trace'Class;
      Parse_Context : in out Wisi.Parse_Context.Parse_Context;
      Changes       : in     Change_Lists.List;
      KMN_List      :    out WisiToken.Parse.KMN_Lists.List);
   --  Changes must be UTF-8.

   procedure Save_Text
     (Context   : in Parse_Context;
      File_Name : in String);
   --  Write Context.Text_Buffer to File_Name.

   procedure Save_Text_Auto (Context : in out Parse_Context);

   procedure Free is new Ada.Unchecked_Deallocation (Parse_Context, Parse_Context_Access);
   --  Declared last to avoid freezing rules.

end Wisi.Parse_Context;
