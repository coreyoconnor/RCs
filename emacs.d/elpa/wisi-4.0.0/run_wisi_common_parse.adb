--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Memory;
with GNATCOLL.Mmap;
with SAL;
with WisiToken.Lexer;
with WisiToken.Parse.LR.McKenzie_Recover;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Run_Wisi_Common_Parse is

   use Ada.Strings.Unbounded;

   type Command_Type is (Parse_Partial, Parse_Incremental, Refactor, Command_File);

   type Command_Line_Params (Command : Command_Type) is record
      --  Similar to emacs_wisi_common_parse.ads Parse_Params

      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Language_Params  : Ada.Strings.Unbounded.Unbounded_String;
      Repeat_Count     : Integer                    := 1;

      case Command is
      when Parse_Partial =>
         Partial_Post_Parse_Action : Wisi.Base_Post_Parse_Action_Type;
         Partial_Begin_Byte_Pos    : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_End_Byte_Pos      : WisiToken.Base_Buffer_Pos  := WisiToken.Invalid_Buffer_Pos;
         Partial_Goal_Byte_Pos     : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_Begin_Char_Pos    : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_End_Char_Pos      : WisiToken.Base_Buffer_Pos  := WisiToken.Invalid_Buffer_Pos;
         Partial_Goal_Char_Pos     : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_Begin_Line        : WisiToken.Line_Number_Type := WisiToken.Line_Number_Type'First;
         Partial_Begin_Indent      : Integer                    := 0;

      when Parse_Incremental =>
         --  Incremental edit, parse, post_parse_action
         Changes               : Wisi.Parse_Context.Change_Lists.List;
         Inc_Post_Parse_Action : Wisi.Base_Post_Parse_Action_Type;
         Inc_Begin_Byte_Pos    : WisiToken.Buffer_Pos      := WisiToken.Invalid_Buffer_Pos;
         Inc_Begin_Char_Pos    : WisiToken.Buffer_Pos      := WisiToken.Invalid_Buffer_Pos;
         Inc_End_Byte_Pos      : WisiToken.Base_Buffer_Pos := WisiToken.Invalid_Buffer_Pos;
         Inc_End_Char_Pos      : WisiToken.Base_Buffer_Pos := WisiToken.Invalid_Buffer_Pos;

      when Refactor =>
         --  We assume the file contains only the one statement/declaration
         --  that needs refactoring.

         Refactor_Action : Wisi.Refactor_Action;
         --  Language-specific

         Edit_Begin : WisiToken.Buffer_Pos;
         --  Source file byte position at start of expression to refactor.

      when Command_File =>
         Command_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   procedure Usage_1 (Parse_Data : in Wisi.Parse_Data_Type'Class)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("usage: parse_partial <post_parse_action> <file_name> [partial parse params] [options]");
      Put_Line ("   or: parse_incremental <post_parse_action> <file_name> <changes> ");
      Put_Line ("          <action_begin_byte> <action_end_byte> [options]");
      Put_Line ("   or: refactor <refactor_action> <file_name> <edit_begin> [options]");
      Put_Line ("   or: command_file <command_file_name> [source_file_name]");
      Put_Line ("post_parse_action: {Navigate | Face | Indent}");
      Put_Line ("refactor_action:");
      Parse_Data.Refactor_Help;
      New_Line;
   end Usage_1;

   procedure Usage
     (Parse_Data  : in Wisi.Parse_Data_Type'Class;
      Parse_Table : in WisiToken.Parse.LR.Parse_Table_Ptr)
   is
      use all type WisiToken.Parse.LR.Parse_Table_Ptr;
      use Ada.Text_IO;
   begin
      Usage_1 (Parse_Data);
      Put_Line ("partial parse params: begin_byte_pos end_byte_pos goal_byte_pos begin_char_pos end_char_pos" &
                  " begin_line begin_indent");
      Put_Line ("options:");
      Put_Line ("--verbosity <trace config>");
      WisiToken.Enable_Trace_Help;
      Put_Line ("--save_text <file_name> : write edited file text to file_name");
      Put_Line ("--lang_params <language-specific params>");
      Put_Line ("--max_parallel n  : set maximum count of parallel parsers" &
                  (if Parse_Table = null then ""
                   else "; default" & Parse_Table.Max_Parallel'Image));
      Put_Line ("--mckenzie_check_limit n  : set error recover token check limit" &
                  (if Parse_Table = null then ""
                   else "; default" & Parse_Table.McKenzie_Param.Check_Limit'Image));
      Put_Line ("--mckenzie_check_delta n  : set error recover delta check limit" &
                  (if Parse_Table = null then ""
                   else "; default" & Parse_Table.McKenzie_Param.Check_Delta_Limit'Image));
      Put_Line ("--mckenzie_enqueue_limit n  : set error recover token enqueue limit" &
                  (if Parse_Table = null then ""
                   else "; default" & Parse_Table.McKenzie_Param.Enqueue_Limit'Image));
      Put_Line ("--mckenzie_full_explore : force error recover explore all solutions");
      Put_Line ("--mckenzie_high_cost : error recover report high cost solutions");
      Put_Line ("--mckenzie_zombie_limit n  : set error recover token zombie limit" &
                  (if Parse_Table = null then ""
                   else "; default" & Parse_Table.McKenzie_Param.Zombie_Limit'Image));
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      Put_Line ("--log <file_name> : output verbosity trace to <file_name>");
      New_Line;
   end Usage;

   Finish : exception;

   Save_File_Name : Unbounded_String;

   Log_File : Ada.Text_IO.File_Type; -- for Parse recover log; unused

   Trace_File : aliased Ada.Text_IO.File_Type; -- for Trace log; see --log.

   procedure Get_File_Size (Parse_Context : in Wisi.Parse_Context.Parse_Context_Access)
   --  Run lexer to get Parse_Context.Text_Buffer_Char_Last
   is
      use all type WisiToken.Token_ID;

      Token       : WisiToken.Lexer.Token;
      Error_Count : Natural;
      pragma Unreferenced (Error_Count);
   begin
      loop
         Error_Count := Parse_Context.Parser.Tree.Lexer.Find_Next (Token);
         exit when Token.ID = Parse_Context.Parser.Tree.Lexer.Descriptor.EOI_ID;
      end loop;

      Parse_Context.Text_Buffer_Byte_Last := Integer (Token.Byte_Region.Last);
      Parse_Context.Text_Buffer_Char_Last := Integer (Token.Char_Region.Last);
   end Get_File_Size;

   procedure Read_Source_File (Name : in String; Parse_Context : in Wisi.Parse_Context.Parse_Context_Access)
   is
      use GNATCOLL.Mmap;
      File   : Mapped_File   := Open_Read (Name);
      Region : Mapped_Region := Read (File);
   begin
      Free (Parse_Context.Text_Buffer);
      Parse_Context.Text_Buffer := new String'(Data (Region) (1 .. Last (Region)));
      Parse_Context.Text_Buffer_Byte_Last := Parse_Context.Text_Buffer'Last;
      --  Text_Buffer_Char_Last is set by lexer, below.

      if 0 /= Ada.Strings.Fixed.Index (Parse_Context.Text_Buffer.all, ASCII.CR & "") then
         --  Test case: ada_mode-recover_partial_14.adb
         Parse_Context.Text_Buffer_Char_Last := Parse_Context.Text_Buffer'Last;

         --  wisi-process-parse.el wisi-parse-require-process sets the coding
         --  convention for sending data to the parser process to utf-8-unix.
         --  So we have to convert DOS line endings to Unix here to match.
         Wisi.To_Unix_Line_Endings
           (Parse_Context.Text_Buffer, Parse_Context.Text_Buffer_Byte_Last,
            Parse_Context.Text_Buffer_Char_Last);
      end if;

      Parse_Context.Parser.Tree.Lexer.Reset_With_String_Access
        (Parse_Context.Text_Buffer, Parse_Context.Text_Buffer_Byte_Last, To_Unbounded_String (Name));
      Free (Region);
      Close (File);
   end Read_Source_File;

   function Command_File_Name
     (Parse_Data : in     Wisi.Parse_Data_Type'Class;
      Next_Arg   :    out Integer)
     return Command_Line_Params
   --  Read command and source file name from command line.
   is
      use Ada.Command_Line;
      use WisiToken;
      Command : Command_Type;
   begin
      if Argument_Count < 2 then
         Usage (Parse_Data, null);
         Set_Exit_Status (Failure);
         raise Finish;
      end if;

      Command := Command_Type'Value (Argument (1));

      return Result : Command_Line_Params (Command) do
         case Command is
         when Parse_Partial =>
            Result.Partial_Post_Parse_Action := Wisi.Base_Post_Parse_Action_Type'Value (Argument (2));
            Result.Source_File_Name  := +Argument (3);
            Next_Arg := 4;

         when Parse_Incremental =>
            Result.Inc_Post_Parse_Action := Wisi.Base_Post_Parse_Action_Type'Value (Argument (2));
            Result.Source_File_Name      := +Argument (3);
            Next_Arg                     := 4;

         when Refactor =>
            Result.Refactor_Action  := Wisi.Refactor_Action'Value (Argument (2));
            Result.Source_File_Name := +Argument (3);
            Next_Arg := 4;

         when Command_File =>
            Result.Command_File_Name := +Argument (2);
            if Argument_Count > 2 and then
              Argument (3)'Length > 2 and then
              Argument (3)(1 .. 2) /= "--"
            then
               Result.Source_File_Name  := +Argument (3);
               Next_Arg                 := 4;
            else
               Next_Arg                 := 3;
            end if;
         end case;
      end return;
   exception
   when Finish =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Parse_Data, null);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Command_File_Name;

   procedure Remaining_Command_Params
     (Parser : in out WisiToken.Parse.LR.Parser.Parser;
      Params : in out Command_Line_Params;
      Arg    : in out Integer)
   --  Command_File_Name reads the first few command line arguments
   is
      use Ada.Command_Line;
      use WisiToken;
   begin
      if Argument_Count >= Arg and then Argument (Arg) = "--help" then
         Usage (Wisi.Parse_Data_Type'Class (Parser.User_Data.all), Parser.Table);
         raise Finish;
      end if;

      case Params.Command is
      when Parse_Partial =>
         if Argument_Count >= 4 and then Argument (4)(1) /= '-' then
            Params.Partial_Begin_Byte_Pos := WisiToken.Buffer_Pos'Value (Argument (4));
            Params.Partial_End_Byte_Pos   := WisiToken.Buffer_Pos'Value (Argument (5)) - 1; -- match emacs region
            Params.Partial_Goal_Byte_Pos  := WisiToken.Buffer_Pos'Value (Argument (6));
            Params.Partial_Begin_Char_Pos := WisiToken.Buffer_Pos'Value (Argument (7));
            Params.Partial_End_Char_Pos   := WisiToken.Buffer_Pos'Value (Argument (8));
            Params.Partial_Goal_Char_Pos  := WisiToken.Buffer_Pos'Value (Argument (9));
            Params.Partial_Begin_Line     := WisiToken.Line_Number_Type'Value (Argument (10));
            Params.Partial_Begin_Indent   := Integer'Value (Argument (11));
            Arg                           := 12;
         else
            Params.Partial_Begin_Byte_Pos := WisiToken.Invalid_Buffer_Pos;
            Params.Partial_End_Byte_Pos   := WisiToken.Invalid_Buffer_Pos;
            Params.Partial_Begin_Char_Pos := WisiToken.Buffer_Pos'First;
            Params.Partial_Begin_Line     := WisiToken.Line_Number_Type'First;
         end if;

      when Parse_Incremental =>
         declare
            Text : constant String := Argument (4);
            Last : Integer := Text'First - 1;
         begin
            Params.Changes := Wisi.Parse_Context.Get_Emacs_Change_List (Text, Last);
         end;

         Params.Inc_Begin_Byte_Pos := WisiToken.Buffer_Pos'Value (Argument (5));
         Params.Inc_End_Byte_Pos   := WisiToken.Buffer_Pos'Value (Argument (6)) - 1; -- match emacs region
         Arg                       := 7;

      when Refactor =>
         Params.Edit_Begin := WisiToken.Buffer_Pos'Value (Argument (4));
         Arg               := 5;

      when Command_File =>
         null;

      end case;

   exception
   when Finish | SAL.Parameter_Error =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Wisi.Parse_Data_Type'Class (Parser.User_Data.all), Parser.Table);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Remaining_Command_Params;

   procedure Command_Options
     (Parser : in out WisiToken.Parse.LR.Parser.Parser;
      Params : in out Command_Line_Params;
      Arg    : in out Integer)
   is
      use Ada.Command_Line;
      use WisiToken;
   begin
      loop
         exit when Arg > Argument_Count;

         if Argument (Arg) = "--verbosity" then
            WisiToken.Enable_Trace (Argument (Arg + 1));
            Arg := @ + 2;

            Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);

            if Trace_Memory > 0 then
               GNATCOLL.Memory.Configure
                 (Activate_Monitor      => True,
                  Stack_Trace_Depth     => 10, -- gets to "new <type>"
                  Reset_Content_On_Free => False);
            end if;

         elsif Argument (Arg) = "--save_text" then
            Save_File_Name := +Argument (Arg + 1);
            Arg := @ + 2;

         elsif Argument (Arg) = "--lang_params" then
            Params.Language_Params := +Argument (Arg + 1);
            Arg := @ + 2;

         elsif Argument (Arg) = "--max_parallel" then
            Parser.Table.Max_Parallel := SAL.Base_Peek_Type'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--mckenzie_check_delta" then
            Parser.Table.McKenzie_Param.Check_Delta_Limit := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--mckenzie_check_limit" then
            Parser.Table.McKenzie_Param.Check_Limit := WisiToken.Syntax_Trees.Sequential_Index'Value
              (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--mckenzie_enqueue_limit" then
            Parser.Table.McKenzie_Param.Enqueue_Limit := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--mckenzie_full_explore" then
            WisiToken.Parse.LR.McKenzie_Recover.Force_Full_Explore := True;
            Arg := @ + 1;

         elsif Argument (Arg) = "--mckenzie_high_cost" then
            WisiToken.Parse.LR.McKenzie_Recover.Force_High_Cost_Solutions := True;
            Arg := @ + 1;

         elsif Argument (Arg) = "--mckenzie_zombie_limit" then
            Parser.Table.McKenzie_Param.Zombie_Limit := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--repeat_count" then
            Params.Repeat_Count := Integer'Value (Argument (Arg + 1));
            Arg := @ + 2;

         elsif Argument (Arg) = "--log" then
            declare
               Log_File_Name : constant String := Argument (Arg + 1);
            begin
               Arg := @ + 2;
               Ada.Text_IO.Open (Trace_File, Ada.Text_IO.Out_File, Log_File_Name);
               WisiToken.Text_IO_Trace.Trace (Parser.Tree.Lexer.Trace.all).Set_File (Trace_File'Access);
            end;

         else
            Ada.Text_IO.Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
            Usage (Wisi.Parse_Data_Type'Class (Parser.User_Data.all), Parser.Table);
            Set_Exit_Status (Failure);
            raise SAL.Parameter_Error;
         end if;
      end loop;
   exception
   when SAL.Parameter_Error =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Wisi.Parse_Data_Type'Class (Parser.User_Data.all), Parser.Table);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Command_Options;

   procedure Put_Errors (Parser : in WisiToken.Parse.LR.Parser.Parser)
   is begin
      if Parser.Tree.Stream_Count = 0 then
         Parser.Put_Errors;

      elsif Parser.Tree.Stream_Count >= 2 then
         Parser.Put_Errors (Parser.Tree.First_Parse_Stream);

      else
         --  Probably an error in Edit_Tree
         Parser.Put_Errors (Parser.Tree.Shared_Stream);
      end if;
   end Put_Errors;

   procedure Process_Command
     (Parse_Context : in out Wisi.Parse_Context.Parse_Context_Access;
      Language      : in     Wisi.Parse_Context.Language;
      Line          : in     String;
      Trace         : in     WisiToken.Trace_Access)
   is
      use Ada.Strings.Fixed;
      use WisiToken; -- "+" unbounded
      use all type WisiToken.Lexer.Handle;

      type File_Command_Type is
        (File, Kill_Context, Language_Params, McKenzie_Options, Memory_Report_Reset, Memory_Report, Parse_Full,
         Parse_Incremental, Post_Parse, Read_Tree, Refactor, Query_Tree, Save_Text, Save_Text_Auto, Verbosity);

      Last  : Integer := Index (Line, " ");
      First : Integer;

      function Get_Command return File_Command_Type
      is
         use Ada.Text_IO;
         Cmd_String : constant String := Line (Line'First .. (if Last = 0 then Line'Last else Last - 1));
      begin
         return File_Command_Type'Value (Cmd_String);
      exception
      when Constraint_Error =>
         Put_Line ("invalid file command '" & Cmd_String & "'");
         Put ("expecting ");
         for Cmd in File_Command_Type'Range loop
            Put (Cmd'Image & ", ");
         end loop;
         raise SAL.Parameter_Error;
      end Get_Command;

      Command : constant File_Command_Type := Get_Command;
   begin
      case Command is
      when File =>
         if Ada.Strings.Unbounded.Length (Parse_Context.File_Name) > 0 then
            --  Changing files
            Parse_Context := Wisi.Parse_Context.Create_No_File (Language, Trace);
         end if;

         declare
            Source_File_Name : constant String := Line (Last + 1 .. Line'Last);
         begin
            Read_Source_File (Source_File_Name, Parse_Context);
            if Trace_Memory > Detail then
               Ada.Text_IO.Put_Line ("file read");
               Report_Memory (Trace.all, Prefix => False);
            end if;
            Get_File_Size (Parse_Context);
            Wisi.Parse_Context.Set_File (Source_File_Name, Parse_Context);
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("'" & Source_File_Name & "' cannot be opened");
            return;
         end;

      when Kill_Context =>
         declare
            Source_File_Name : constant String := Line (Last + 1 .. Line'Last);
         begin
            if Source_File_Name = -Parse_Context.File_Name then
               Parse_Context := null;
            end if;
            Wisi.Parse_Context.Kill (Source_File_Name);
         end;

      when Language_Params =>
         Wisi.Parse_Data_Type'Class (Parse_Context.Parser.User_Data.all).Parse_Language_Params
           (Line (Last + 1 .. Line'Last));

      when McKenzie_Options =>
         WisiToken.Parse.LR.Set_McKenzie_Options
           (Parse_Context.Parser.Table.McKenzie_Param, Line (Last + 1 .. Line'Last));

      when Memory_Report_Reset =>
         WisiToken.Memory_Baseline := GNATCOLL.Memory.Get_Ada_Allocations.Current;
         Ada.Text_IO.Put_Line ("(message ""memory report reset"")");

      when Memory_Report =>
         Report_Memory (Trace.all, Prefix => False);

      when Parse_Full =>
         --  Force a dispatching call.
         Wisi.Parse_Data_Type'Class (Parse_Context.Parser.User_Data.all).Initialize;

         Parse_Context.Parser.User_Data.Reset;
         Parse_Context.Parser.Tree.Lexer.Reset;
         begin
            Parse_Context.Parser.Parse (Log_File);
            Wisi.Put_Errors (Parse_Context.Parser.Tree);
         exception
         when WisiToken.Syntax_Error =>
            Put_Errors (Parse_Context.Parser);
            Ada.Text_IO.Put_Line ("(parse_error)");

         when E : WisiToken.Parse_Error =>
            Put_Errors (Parse_Context.Parser);
            Ada.Text_IO.Put_Line
              ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                 Ada.Exceptions.Exception_Message (E) & """)");
         end;

      when Parse_Incremental =>
         declare
            Changes  : constant Wisi.Parse_Context.Change_Lists.List :=
              Wisi.Parse_Context.Get_Emacs_Change_List (Line, Last);
            KMN_List : WisiToken.Parse.KMN_Lists.List;
         begin
            Wisi.Parse_Context.Edit_Source (Trace.all, Parse_Context.all, Changes, KMN_List);

            if Length (Parse_Context.Root_Save_Edited_Name) /= 0 then
               Parse_Context.Save_Text_Auto;
            end if;

            Parse_Context.Parser.Tree.Lexer.Reset_With_String_Access
              (Parse_Context.Text_Buffer,
               Parse_Context.Text_Buffer_Byte_Last,
               +Parse_Context.Parser.Tree.Lexer.File_Name);

            --  Same logic as emacs_wisi_common_parse.adb
            if Parse_Context.Parser.Tree.Editable then
               Parse_Context.Parser.Parse (Log_File, KMN_List);
            else
               --  Last parse failed; can't edit tree, so do full parse.
               Parse_Context.Parser.Parse (Log_File, Parse.KMN_Lists.Empty_List);
            end if;

            Wisi.Put_Errors (Parse_Context.Parser.Tree);
         exception
         when E : WisiToken.Syntax_Error | WisiToken.Parse_Error =>
            Put_Errors (Parse_Context.Parser);
            Ada.Text_IO.Put_Line
              ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                 Ada.Exceptions.Exception_Message (E) & """)");
         end;

      when Post_Parse =>

         First := Last + 1;
         Last  := Index (Line, " ", From => First);
         declare
            use all type Wisi.Post_Parse_Action_Type;

            Action : constant Wisi.Post_Parse_Action_Type := Wisi.Post_Parse_Action_Type'Value (Line (First .. Last));

            Begin_Byte_Pos : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last));
            Begin_Char_Pos : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last));

            --  Emacs end is after last char. FIXME: if last char is multibyte,
            --  this is wrong; add something in wisitoken-utf_8.
            End_Byte_Pos   : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last)) - 1;
            End_Char_Pos   : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last)) - 1;
         begin
            Wisi.Parse_Data_Type'Class (Parse_Context.Parser.User_Data.all).Reset_Post_Parse
              (Parse_Context.Parser.Tree, Action,
               Action_Region_Bytes => (Begin_Byte_Pos, End_Byte_Pos),
               Action_Region_Chars => (Begin_Char_Pos, End_Char_Pos),
               Begin_Indent        => 0);

            Parse_Context.Parser.Execute_Actions (Action_Region_Bytes => (Begin_Byte_Pos, End_Byte_Pos));

            if Trace_Memory > Detail then
               Trace.Put_Line ("post_parse action done");
               Report_Memory (Trace.all, Prefix => False);
            end if;

            Wisi.Parse_Data_Type'Class (Parse_Context.Parser.User_Data.all).Put (Parse_Context.Parser);
         end;

      when Read_Tree =>
         declare
            Dump_File : constant String := Line (Last + 1 .. Line'Last);
         begin
            --  We assume a corresponding File command is also present.
            Parse_Context.Parser.Tree.Get_Tree (Dump_File);
         end;

      when Refactor =>
         declare
            Action     : constant Wisi.Refactor_Action := Wisi.Parse_Data_Type'Class
              (Parse_Context.Parser.User_Data.all).Refactor_Parse (Wisi.Get_Enum (Line, Last));
            Edit_Begin : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last));
         begin
            Wisi.Parse_Data_Type'Class (Parse_Context.Parser.User_Data.all).Refactor
              (Parse_Context.Parser.Tree, Action, Edit_Begin);
         end;

      when Query_Tree =>
         declare
            use Wisi;
            Label : constant Wisi.Query_Label := Wisi.Query_Label'Value (Wisi.Get_Enum (Line, Last));

            Parse_Data : constant Wisi.Parse_Data_Access_Constant :=
              Wisi.Parse_Data_Access_Constant (Parse_Context.Parser.User_Data);
         begin
            case Label is
            when Point_Query =>
               declare
                  Point : constant WisiToken.Buffer_Pos := WisiToken.Buffer_Pos (Wisi.Get_Integer (Line, Last));
                  IDs : constant WisiToken.Token_ID_Arrays.Vector :=
                    (case Point_Query'(Label) is
                     when Node | Containing_Statement => WisiToken.Token_ID_Arrays.Empty_Vector,
                     when Ancestor => Wisi.Get_Token_IDs (Parse_Data.all, Line, Last));
                  Query : constant Wisi.Query :=
                    (case Point_Query'(Label) is
                     when Node => (Node, Point),
                     when Containing_Statement => (Containing_Statement, Point),
                     when Ancestor => (Ancestor, Point, IDs));
               begin
                  Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, Query);
               end;

            when Parent | Child =>
               declare
                  Address : constant String := Wisi.Get_String (Line, Last);
                  Node    : constant WisiToken.Syntax_Trees.Valid_Node_Access := Wisi.To_Node_Access (Address);
                  N       : constant Integer := Wisi.Get_Integer (Line, Last);
               begin
                  Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, (Node_Query'(Label), Node, N));
               end;

            when Print =>
               Wisi.Query_Tree (Parse_Data, Parse_Context.Parser.Tree, (Label => Print));

            when Dump =>
               declare
                  File_Name : constant String := Line (Last + 1 .. Line'Last);
               begin
                  Wisi.Query_Tree
                    (Parse_Data,
                     Parse_Context.Parser.Tree,
                     (Label     => Dump,
                      File_Name => +File_Name));
               end;
            end case;
         end;

      when Save_Text =>
         declare
            Save_File_Name : constant String := Line (Last + 1 .. Line'Last);
         begin
            Parse_Context.Save_Text (Save_File_Name);
         end;

      when Save_Text_Auto =>
         declare
            Save_File_Name : constant String := Line (Last + 1 .. Line'Last);
         begin
            Parse_Context.Root_Save_Edited_Name := +Save_File_Name;
            Parse_Context.Save_Edited_Count     := 0;
            Ada.Text_IO.Put_Line ("auto text save enabled, to '" & Save_File_Name & "_nnn'");
         end;

      when Verbosity =>
         WisiToken.Enable_Trace (Line (Last + 1 .. Line'Last));
         if Parse_Context.Parser.Tree.Lexer /= null then
            Parse_Context.Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);
         end if;
         if Trace_Memory > 0 then
            GNATCOLL.Memory.Configure
              (Activate_Monitor      => True,
               Stack_Trace_Depth     => 10, -- gets to "new <type>"
               Reset_Content_On_Free => False);
         end if;

      end case;
   end Process_Command;

   procedure Parse_File (Language : in Wisi.Parse_Context.Language; Trace : in WisiToken.Trace_Access)
   is
      use Ada.Text_IO;
      use WisiToken;

      Start : Ada.Real_Time.Time;
   begin
      declare
         use all type Wisi.Base_Post_Parse_Action_Type;

         Arg       : Integer;
         Cl_Params : Command_Line_Params := Command_File_Name (Language.Parse_Data_Template.all, Arg);

         Parse_Context : Wisi.Parse_Context.Parse_Context_Access :=
           (if Length (Cl_Params.Source_File_Name) > 0 -- can be empty for Command_File
            then Wisi.Parse_Context.Find_Create (-Cl_Params.Source_File_Name, Language, Trace)
            else Wisi.Parse_Context.Create_No_File (Language, Trace));

         Parser : WisiToken.Parse.LR.Parser.Parser renames Parse_Context.Parser;

         Parse_Data : Wisi.Parse_Data_Type'Class renames Wisi.Parse_Data_Type'Class (Parser.User_Data.all);
      begin
         Remaining_Command_Params (Parser, Cl_Params, Arg);

         Trace.Set_Prefix (";; "); -- so we get the same debug messages as Emacs_Wisi_Common_Parse

         begin
            case Cl_Params.Command is
            when Parse_Partial =>
               Parser.Tree.Lexer.Reset_With_File
                 (-Cl_Params.Source_File_Name, Cl_Params.Partial_Begin_Byte_Pos, Cl_Params.Partial_End_Byte_Pos,
                  Cl_Params.Partial_Begin_Char_Pos, Cl_Params.Partial_Begin_Line);

            when Refactor =>
               Parser.Tree.Lexer.Reset_With_File (-Cl_Params.Source_File_Name);

            when Parse_Incremental | Command_File =>
               if Length (Cl_Params.Source_File_Name) > 0 then
                  Read_Source_File (-Cl_Params.Source_File_Name, Parse_Context);
               end if;
            end case;
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line (Standard_Error, "'" & (-Cl_Params.Source_File_Name) & "' cannot be opened");
            return;
         end;

         if Length (Cl_Params.Source_File_Name) > 0 then
            Get_File_Size (Parse_Context);
         end if;

         case Cl_Params.Command is
         when Parse_Partial =>
            if Cl_Params.Partial_Begin_Byte_Pos = WisiToken.Invalid_Buffer_Pos then
               Cl_Params.Partial_Begin_Byte_Pos := WisiToken.Buffer_Pos'First;
               Cl_Params.Partial_Begin_Char_Pos := WisiToken.Buffer_Pos'First;
               Cl_Params.Partial_End_Byte_Pos   := Base_Buffer_Pos (Parse_Context.Text_Buffer_Byte_Last);
               Cl_Params.Partial_End_Char_Pos   := Base_Buffer_Pos (Parse_Context.Text_Buffer_Char_Last);
            else
               Parser.Partial_Parse_Active.all    := True;
               Parser.Partial_Parse_Byte_Goal.all := Cl_Params.Partial_Goal_Byte_Pos;
            end if;
         when Parse_Incremental =>
            if Cl_Params.Inc_Begin_Byte_Pos = WisiToken.Invalid_Buffer_Pos then
               Cl_Params.Inc_Begin_Byte_Pos := WisiToken.Buffer_Pos'First;
               Cl_Params.Inc_Begin_Char_Pos := WisiToken.Buffer_Pos'First;
               Cl_Params.Inc_End_Byte_Pos   := Parser.Tree.Byte_Region
                 (Parser.Tree.EOI, Trailing_Non_Grammar => False).Last;
               Cl_Params.Inc_End_Char_Pos   := Parser.Tree.Char_Region
                 (Parser.Tree.EOI, Trailing_Non_Grammar => False).Last;
            end if;
         when Refactor | Command_File =>
            null;
         end case;

         case Cl_Params.Command is
         when Parse_Partial =>

            Parse_Data.Initialize;

            Command_Options (Parser, Cl_Params, Arg);
            Parse_Data.Parse_Language_Params (-Cl_Params.Language_Params);

            if Cl_Params.Repeat_Count > 1 then
               Start := Ada.Real_Time.Clock;
            end if;

            for I in 1 .. Cl_Params.Repeat_Count loop
               begin
                  Parse_Data.Reset;
                  Parser.Tree.Lexer.Reset;

                  Parser.Parse (Log_File);
                  --  Raises Parse_Error for ambiguous parse and similar errors.

                  Wisi.Put_Errors (Parser.Tree);

                  if Trace_Memory > 0 then
                     Report_Memory (Trace.all, Prefix => False);
                  end if;

                  if Cl_Params.Partial_Post_Parse_Action /= None then
                     Parse_Data.Reset_Post_Parse
                       (Parser.Tree,
                        Post_Parse_Action   => Cl_Params.Partial_Post_Parse_Action,
                        Action_Region_Bytes => (Cl_Params.Partial_Begin_Byte_Pos, Cl_Params.Partial_Goal_Byte_Pos),
                        Action_Region_Chars => (Cl_Params.Partial_Begin_Char_Pos, Cl_Params.Partial_Goal_Char_Pos),
                        Begin_Indent        => Cl_Params.Partial_Begin_Indent);

                     Parser.Execute_Actions (Action_Region_Bytes => Parse_Data.Action_Region_Bytes);

                     if Cl_Params.Repeat_Count = 1 then
                        Parse_Data.Put (Parser);
                     end if;
                  end if;
               exception
               when WisiToken.Syntax_Error =>
                  Put_Errors (Parser);
                  Put_Line ("(parse_error)");

               when E : WisiToken.Parse_Error =>
                  Put_Errors (Parser);
                  Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");

               when E : others => -- includes Fatal_Error
                  Put_Errors (Parser);
                  Put_Line ("(error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");
               end;
            end loop;

            if Cl_Params.Repeat_Count > 1 then
               declare
                  use Ada.Real_Time;
                  Finish : constant Time := Clock;
               begin
                  Put_Line ("Total time:" & Duration'Image (To_Duration (Finish - Start)));
                  Put_Line
                    ("per iteration:" & Duration'Image (To_Duration ((Finish - Start) / Cl_Params.Repeat_Count)));
               end;
            end if;

         when Parse_Incremental | Refactor =>
            Command_Options (Parser, Cl_Params, Arg);

            if Cl_Params.Command /= Refactor then
               Parse_Data.Parse_Language_Params (-Cl_Params.Language_Params);
            end if;

            --  First do a full parse to get the syntax tree
            begin
               Parse_Data.Initialize;
               Parser.Tree.Lexer.Reset;
               Parser.Parse (Log_File);
               Wisi.Put_Errors (Parse_Context.Parser.Tree);
               if Trace_Memory > 0 then
                  Put ("initial full parse ");
                  Report_Memory (Trace.all, Prefix => False);
               end if;

            exception
            when WisiToken.Syntax_Error =>
               Put_Errors (Parser);
               Put_Line ("(parse_error)");

            when E : WisiToken.Parse_Error =>
               Put_Errors (Parser);
               Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                           Ada.Exceptions.Exception_Message (E) & """)");
            end;

            case Cl_Params.Command is
            when Parse_Incremental =>
               declare
                  KMN_List : WisiToken.Parse.KMN_Lists.List;
               begin
                  Wisi.Parse_Context.Edit_Source (Trace.all, Parse_Context.all, Cl_Params.Changes, KMN_List);

                  if -Save_File_Name /= "" then
                     declare
                        use Ada.Directories;
                        Save_File : File_Type;
                     begin
                        if Exists (-Save_File_Name) then
                           Delete_File (-Save_File_Name);
                        end if;
                        Create (Save_File, Out_File, -Save_File_Name);
                        Put (Save_File, Parse_Context.Text_Buffer (1 .. Parse_Context.Text_Buffer_Byte_Last));
                        Close (Save_File);
                     end;
                  end if;

                  Parse_Data.Parse_Language_Params (-Cl_Params.Language_Params);

                  Parser.Tree.Lexer.Reset_With_String_Access
                    (Parse_Context.Text_Buffer, Parse_Context.Text_Buffer_Byte_Last, Cl_Params.Source_File_Name);

                  Parser.Parse (Log_File, KMN_List);
                  Wisi.Put_Errors (Parse_Context.Parser.Tree);

                  if Cl_Params.Inc_Post_Parse_Action /= None then
                     Parse_Data.Reset_Post_Parse
                       (Parser.Tree, Cl_Params.Inc_Post_Parse_Action,
                        Action_Region_Bytes => (Cl_Params.Inc_Begin_Byte_Pos, Cl_Params.Inc_End_Byte_Pos),
                        Action_Region_Chars => (Cl_Params.Inc_Begin_Char_Pos, Cl_Params.Inc_End_Char_Pos),
                        Begin_Indent        => 0);

                     Parser.Execute_Actions
                       (Action_Region_Bytes => (Cl_Params.Inc_Begin_Byte_Pos, Cl_Params.Inc_End_Byte_Pos));

                     Parse_Data.Put (Parser);
                  end if;

                  if Trace_Memory > 0 then
                     Put ("incremental parse ");
                     Report_Memory (Trace.all, Prefix => False);
                  end if;

               exception
               when WisiToken.Syntax_Error =>
                  Put_Errors (Parser);
                  Put_Line ("(parse_error)");

               when E : WisiToken.Parse_Error =>
                  Put_Errors (Parser);
                  Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                              Ada.Exceptions.Exception_Message (E) & """)");
               end;

            when Refactor =>
               Parse_Data.Refactor
                 (Parser.Tree,
                  Cl_Params.Refactor_Action, Cl_Params.Edit_Begin);

            when others =>
               null;
            end case;

         when Command_File =>
            Command_Options (Parser, Cl_Params, Arg);

            --  We don't do a full parse here, to let .cmd file set debug params for full parse.

            if Length (Cl_Params.Source_File_Name) > 0 then
               Ada.Text_IO.Put_Line ('"' & (-Cl_Params.Source_File_Name) & '"' & (-Cl_Params.Language_Params));
               Ada.Text_IO.New_Line;
            end if;
            declare
               Cmd_File : Ada.Text_IO.File_Type;
            begin
               Open (Cmd_File, In_File, -Cl_Params.Command_File_Name);
               Ada.Directories.Set_Directory (Ada.Directories.Containing_Directory (-Cl_Params.Command_File_Name));
               loop
                  exit when End_Of_File (Cmd_File);
                  declare
                     Line : constant String := Get_Line (Cmd_File);
                  begin
                     if Line'Length > 0 then
                        Trace.Put_Line (Line);
                        if Line (1 .. 2) = "--" then
                           null;
                        else
                           Process_Command (Parse_Context, Language, Line, Trace);
                           Trace.New_Line;
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end case;
      end;

      if Ada.Text_IO.Is_Open (Trace_File) then
         Ada.Text_IO.Close (Trace_File);
      end if;
   exception
   when SAL.Parameter_Error | Finish =>
      --  From Get_CL_Params; already handled.
      null;

   when E : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      New_Line (2);
      Put_Line
        ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E) & """)");

      Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
      Trace.New_Line;
      if Ada.Text_IO.Is_Open (Trace_File) then
         Ada.Text_IO.Close (Trace_File);
      end if;
   end Parse_File;

end Run_Wisi_Common_Parse;
