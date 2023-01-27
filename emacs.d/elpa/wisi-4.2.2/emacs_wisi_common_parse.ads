--  Abstract :
--
--  Common utilities for Gen_Emacs_Wisi_*_Parse
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

with Ada.Strings.Unbounded;
with System;
with WisiToken.Parse;
with Wisi.Parse_Context;
package Emacs_Wisi_Common_Parse is

   Protocol_Version : constant String := "7";
   --  Protocol_Version defines the data sent between elisp and the
   --  background process, except for the language-specific parameters,
   --  which are defined by the Language_Protocol_Version parameter to
   --  Parse_Stream, below.
   --
   --  This value must match wisi-process-parse.el
   --  wisi-process-parse-protocol-version.
   --
   --  See wisi-process-parse.el functions, and this package body, for
   --  the implementation of the protocol.
   --
   --  Only changes once per wisi release. Increment as soon as required,
   --  record new version in NEWS-wisi.text. If working on a branch and
   --  main has already incremented, increment again, in case main is
   --  released before branch is merged; leave two "increment protocol"
   --  lines in NEWS-wisi.text to indicate the issue.

   Prompt : constant String := ";;> ";

   Finish : exception;

   procedure Usage (Name : in String);

   procedure Read_Input (A : System.Address; N : Integer);
   --  Read N bytes from standard_input to A.

   function Get_Command_Length return Integer;

   type Process_Start_Params is record
      Recover_Log_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  log enabled if non-empty.
   end record;

   function Get_Process_Start_Params return Process_Start_Params;
   --  Get from Ada.Command_Line. Handles --help by outputing help,
   --  raising Finish.

   procedure Process_Stream
     (Name                      : in String;
      Language_Protocol_Version : in String;
      Params                    : in Process_Start_Params;
      Factory                   : in WisiToken.Parse.Factory;
      Free_Parser               : in WisiToken.Parse.Free_Parser;
      Trace                     : in WisiToken.Trace_Access);

   ----------
   --  Parse command

   type Parse_Kind is (Partial, Incremental, Full);

   type Parse_Params (Kind : Parse_Kind) is record
      --  See Get_Parse_Params in body for what elisp this must match.

      Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;

      Verbosity     : Ada.Strings.Unbounded.Unbounded_String;
      Task_Count    : Integer;
      Zombie_Limit  : Integer;
      Enqueue_Limit : Integer;
      Max_Parallel  : Integer;

      Language_Params : Ada.Strings.Unbounded.Unbounded_String;

      case Kind is
      when Partial =>
         Post_Parse_Action : Wisi.Post_Parse_Action_Type;

         Begin_Byte_Pos : Integer;
         --  Source file byte position of first char sent; start parse here.

         End_Byte_Pos : Integer;
         --  Byte position of last char sent.
         --  Emacs convention; after last char

         Goal_Byte_Pos : Integer;
         --  Byte position of end of desired parse region; terminate parse at
         --  or after here.

         Begin_Char_Pos : WisiToken.Buffer_Pos;
         End_Char_Pos   : WisiToken.Base_Buffer_Pos;
         Goal_Char_Pos  : WisiToken.Buffer_Pos;
         --  Corresponding char positions; end is 0 if buffer empty.

         Begin_Line : WisiToken.Line_Number_Type;
         --  Line containing Begin_Byte_Pos

         Begin_Indent : Integer;
         --  Indentation of Line_Begin

         Partial_Parse_Active : Boolean;

      when Incremental =>
         Changes : Wisi.Parse_Context.Change_Lists.List;

      when Full =>
         Byte_Count        : Integer;
         Full_End_Char_Pos : WisiToken.Base_Buffer_Pos; -- 0 if buffer empty.

      end case;
   end record;

   function Get_Parse_Params (Command_Line : in String; Last : in out Integer) return Parse_Params;
   --  Raise Protocol_Error if, after processing Command_Line, Last /=
   --  Command_Line'Last.

   ----------
   --  Post-Parse command

   type Post_Parse_Params is record
      Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Verbosity         : Ada.Strings.Unbounded.Unbounded_String;
      Post_Parse_Action : Wisi.Post_Parse_Action_Type;

      Begin_Byte_Pos : Integer;
      Begin_Char_Pos : Integer;
      End_Byte_Pos   : Integer;
      End_Char_Pos   : Integer;
      --  Region to execute action in.
      --  Emacs convention; end is after last char

      Language_Params : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Get_Post_Parse_Params (Command_Line : in String; Last : in out Integer) return Post_Parse_Params;
   --  Raise Protocol_Error if, after processing Command_Line, Last /=
   --  Command_Line'Last.

   ----------
   --  Refactor command

   type Refactor_Params is record
      Refactor_Action  : Wisi.Refactor_Action; -- Language-specific
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      Edit_Begin : WisiToken.Buffer_Pos;
      --  Source file byte position at start of expression to refactor.

      Verbosity : Ada.Strings.Unbounded.Unbounded_String;

      --  no Language_Params
   end record;

   function Get_Refactor_Params (Command_Line : in String; Last : in out Integer) return Refactor_Params;

end Emacs_Wisi_Common_Parse;
