--  Abstract :
--
--  Parser for Wisi grammar files, producing Ada source
--  files for a parser.
--
--  Copyright (C) 2012 - 2015, 2017 - 2023 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.BNF.Generate_Utils;
with WisiToken.BNF.Output_Ada;
with WisiToken.BNF.Output_Ada_Common;
with WisiToken.BNF.Output_Ada_Emacs;
with WisiToken.BNF.Output_Elisp_Common;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.Packrat;
with WisiToken.Parse.LR.Parser_No_Recover; -- for reading BNF file
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Generate.Tree_Sitter;
with WisiToken_Grammar_Editing;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Main;
procedure WisiToken.BNF.Generate
is
   use all type SAL.Base_Peek_Type;

   procedure Put_Usage
   is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      Put_Line (Standard_Error, "version 4.1"); -- matches release version in Docs/wisitoken.html
      Put_Line (Standard_Error, "wisitoken-bnf-generate [options] {wisi grammar file}");
      Put_Line (Standard_Error, "Generate source code implementing a parser for the grammar.");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "The following grammar file directives control parser generation:");
      Put_Line (Standard_Error,
                "%generate <algorithm> <output language> [<lexer>] [<interface>] [text_rep]");
      Put_Line (Standard_Error, "   specify one of each generate parameter. May be repeated.");
      Put (Standard_Error, "   algorithm: ");
      for I of Generate_Algorithm_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);

      Put (Standard_Error, "   output language: ");
      First := True;
      for I of Output_Language_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);

      Put_Line (Standard_Error, "   interface: interface Process | Module");
      Put_Line (Standard_Error, "      only valid with Ada_Emacs:");
      Put_Line (Standard_Error, "      Process is for an external subprocess communicating with Emacs.");
      Put_Line (Standard_Error, "      Module  is for a dynamically loaded Emacs module.");
      Put (Standard_Error, "   lexer: ");
      First := True;
      for I of Output_Language_Image loop
         if First then
            First := False;
         else
            Put (Standard_Error, " | ");
         end if;
         Put (Standard_Error, I.all);
      end loop;
      New_Line (Standard_Error);
      Put_Line
        (Standard_Error, "   text_rep: output LR parse table in a text file, not as source code; for large tables");

      New_Line (Standard_Error);
      Put_Line (Standard_Error, "options:");
      Put_Line (Standard_Error, "  --help: show this help");

      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line
        (Standard_Error, "  --verbosity <key=value ...> sets verbosity levels (default 0):");
      Put_Line (Standard_Error, "     0 - only error messages to standard error");
      Put_Line (Standard_Error, "     1 - add diagnostics to standard out");
      Put_Line (Standard_Error, "     2 - more diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line (Standard_Error, "  --generate ...: override grammar file %generate directive");
      Put_Line (Standard_Error, "  --output_bnf : output translated EBNF source to <grammar file name base>_bnf.wy");
      Put_Line (Standard_Error, "  --suffix <string>; appended to grammar file name");
      Put_Line (Standard_Error, "  --ignore_conflicts; ignore excess/unknown conflicts");
      Put_Line (Standard_Error,
                "  --test_main; generate standalone main program for running the generated parser, modify file names");
      Put_Line (Standard_Error, "verbosity keys:");
      Enable_Trace_Help;
   end Put_Usage;

   Language_Name           : Ada.Strings.Unbounded.Unbounded_String; -- The language the grammar defines
   Output_File_Name_Root   : Ada.Strings.Unbounded.Unbounded_String;
   Suffix                  : Ada.Strings.Unbounded.Unbounded_String;
   Output_BNF              : Boolean                          := False;
   Ignore_Conflicts        : Boolean                          := False;
   Test_Main               : Boolean                          := False;

   Command_Generate_Set : Generate_Set_Access; -- override grammar file declarations

   Trace          : aliased WisiToken.Text_IO_Trace.Trace;
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser := Wisitoken_Grammar_Main.Create_Parser
     (Trace'Unchecked_Access, Input_Data'Unchecked_Access);
   Log_File       : Ada.Text_IO.File_Type; -- not used

   procedure Use_Input_File (File_Name : in String)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      Output_File_Name_Root := +Ada.Directories.Base_Name (File_Name) & Suffix;

      Grammar_Parser.Tree.Lexer.Reset_With_File (File_Name);

      declare
         Language_Name_Dir   : constant Integer := Ada.Strings.Fixed.Index
           (File_Name, Ada.Strings.Maps.To_Set ("/\"), Going => Ada.Strings.Backward);
         Language_Name_Ext   : constant Integer := Ada.Strings.Fixed.Index (File_Name, ".wy");
      begin
         Language_Name := +WisiToken.BNF.Output_Elisp_Common.Elisp_Name_To_Ada
           (File_Name
              ((if Language_Name_Dir = 0
                then File_Name'First
                else Language_Name_Dir + 1) ..
                 Language_Name_Ext - 1),
            Append_ID => False,
            Trim      => 0);
      end;
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

begin
   declare
      use Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         --   --help, --verbosity first, then alphabetical

         if Argument (Arg_Next) = "--help" then
            Put_Usage;
            return;

         elsif Argument (Arg_Next) = "--verbosity" then
            Arg_Next := Arg_Next + 1;
            WisiToken.Enable_Trace (Argument (Arg_Next));
            Arg_Next := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--ignore_conflicts" then
            Ignore_Conflicts := True;
            Arg_Next         := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--generate" then
            Arg_Next  := Arg_Next + 1;
            declare
               Tuple : Generate_Tuple;
               Done  : Boolean := False;
            begin
               begin
                  Tuple.Gen_Alg := Generate_Algorithm'Value (Argument (Arg_Next));
                  Arg_Next      := Arg_Next + 1;
               exception
               when Constraint_Error =>
                  raise User_Error with "invalid value for generator_algorithm: '" & Argument (Arg_Next) & ";";
               end;
               if Tuple.Gen_Alg /= None then
                  begin
                     Tuple.Out_Lang := To_Output_Language (Argument (Arg_Next));
                     Arg_Next       := Arg_Next + 1;
                  end;

                  loop
                     exit when Done;
                     declare
                        Text : constant String := Argument (Arg_Next);
                     begin
                        if Text = "text_rep" then
                           Tuple.Text_Rep := True;
                           Arg_Next := Arg_Next + 1;

                        elsif (for some I of Lexer_Image => To_Lower (Text) = I.all) then
                           Tuple.Lexer := To_Lexer (Text);
                           Arg_Next := Arg_Next + 1;

                        elsif (for some I of Interface_Image => To_Lower (Text) = I.all)  then
                           Tuple.Interface_Kind := WisiToken.BNF.Valid_Interface'Value (Text);
                           Arg_Next := Arg_Next + 1;

                        else
                           Done := True;
                        end if;
                     end;
                  end loop;
               end if;
               Add (Command_Generate_Set, Tuple);
            end;

         elsif Argument (Arg_Next) = "--output_bnf" then
            Output_BNF := True;
            Arg_Next   := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--suffix" then
            Arg_Next := Arg_Next + 1;
            Suffix   := +Argument (Arg_Next);
            Arg_Next := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--test_main" then
            Arg_Next  := Arg_Next + 1;
            Test_Main := True;

         else
            raise User_Error with "invalid argument '" & Argument (Arg_Next) & "'";
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));

      if Arg_Next /= Argument_Count then
         raise User_Error with "arg count" & Integer'Image (Argument_Count) &
           " different from expected count" & Integer'Image (Arg_Next);
      end if;
   end;

   if Trace_Generate > Outline then
      Trace.Put_Line ("parse grammar file");
   end if;

   begin
      Grammar_Parser.Parse (Log_File);
   exception
   when WisiToken.Syntax_Error =>
      if Grammar_Parser.Tree.Parents_Set then
         Grammar_Parser.Put_Errors;
      elsif Grammar_Parser.Tree.Stream_Count >= 2 then
         Grammar_Parser.Put_Errors (Grammar_Parser.Tree.Last_Parse_Stream);
      else
         Grammar_Parser.Put_Errors (Grammar_Parser.Tree.Shared_Stream);
      end if;
      raise;
   when E : WisiToken.Parse_Error =>
      WisiToken.Generate.Put_Error (Ada.Exceptions.Exception_Message (E));
      raise;
   end;

   declare
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use Ada.Text_IO;
      use all type WisiToken_Grammar_Runtime.Meta_Syntax;

      Generate_Set    : Generate_Set_Access;
      Multiple_Tuples : Boolean;

      Lexer_Done : Lexer_Set := (others => False);

      BNF_Tree : Syntax_Trees.Tree; --  Only filled when source is EBNF, alg is LR

      --  In general, all of the data in Generate_Utils.Generate_Data
      --  depends on the generate tuple parameters. However, if
      --  'If_Lexer_Present' is false, then they don't depend on the lexer,
      --  and if 'If_Parser_Present' is false, then they don't depend on
      --  Gen_Alg, except for the parser table. But it's not worth trying to
      --  cache results in those cases; they only happen in test grammars,
      --  which are small.

      procedure Translate_To_BNF
      is begin
         if Trace_Generate > Outline or Trace_Generate_EBNF > Outline then
            Ada.Text_IO.Put_Line ("Translate EBNF tree to BNF");
         end if;

         declare
            Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Tree       : WisiToken.Syntax_Trees.Tree renames Grammar_Parser.Tree;
         begin
            Syntax_Trees.Copy_Tree
              (Source      => Tree,
               Destination => BNF_Tree,
               User_Data   => Input_Data'Unchecked_Access);

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.Put_Line ("EBNF tree:");
               BNF_Tree.Print_Tree;
            end if;

            WisiToken_Grammar_Editing.Translate_EBNF_To_BNF (BNF_Tree, Input_Data);

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("BNF tree:");
               BNF_Tree.Print_Tree;
            end if;

            if Output_BNF then
               --  FIXME: if %if is present, it can change the bnf tree; output one for each tuple.
               WisiToken_Grammar_Editing.Print_Source (-Output_File_Name_Root & "_bnf.wy", BNF_Tree, Input_Data);
            end if;

            if Trace_Time then
               Ada.Text_IO.Put_Line
                 ("translate to bnf time:" & Duration'Image (Ada.Calendar."-" (Ada.Calendar.Clock, Time_Start)));
            end if;

            if WisiToken.Generate.Error then
               raise WisiToken.Grammar_Error with "errors during translating EBNF to BNF: aborting";
            end if;
         end;
      end Translate_To_BNF;

      procedure Parse_Check
        (Lexer  : in Lexer_Type;
         Parser : in Generate_Algorithm;
         Phase  : in WisiToken_Grammar_Runtime.Action_Phase)
      --  Ensure that the proper grammar file post-parse processing has been done.
      is
         use all type Ada.Containers.Count_Type;
         use all type WisiToken_Grammar_Runtime.Action_Phase;
      begin
         Input_Data.User_Parser := Parser;
         Input_Data.User_Lexer  := Lexer;
         --  Specifying the parser and lexer can change the parsed grammar, due
         --  to %if {parser | lexer}.

         Input_Data.Reset; -- only resets Other data

         Input_Data.Phase := Phase;

         case Phase is
         when Meta =>
            if Trace_Generate > Outline then
               Trace.Put_Line ("post-parse grammar file META");
            end if;

            Grammar_Parser.Execute_Actions;

            case Input_Data.Meta_Syntax is
            when Unknown =>
               Input_Data.Meta_Syntax := BNF_Syntax;

            when BNF_Syntax | EBNF_Syntax =>
               null;
            end case;

         when Other =>
            case Valid_Generate_Algorithm'(Parser) is
            when LR_Generate_Algorithm | Packrat_Generate_Algorithm =>
               --  IMPROVEME: for now, Packrat requires a BNF tree; eventually, it
               --  will use the EBNF tree.

               if Input_Data.Meta_Syntax = EBNF_Syntax and BNF_Tree.Is_Empty then
                  Translate_To_BNF;
               end if;

               if BNF_Tree.Is_Empty then
                  if Trace_Generate > Outline then
                     Trace.Put_Line ("post-parse grammar file OTHER, main tree");
                  end if;

                  Grammar_Parser.Execute_Actions;
               else
                  if Trace_Generate > Outline then
                     Trace.Put_Line ("post-parse grammar file OTHER, bnf tree");
                  end if;

                  WisiToken.Parse.Execute_Actions
                    (BNF_Tree, Grammar_Parser.Productions, Input_Data'Unchecked_Access);
               end if;

            when External =>
               null;

            when Tree_Sitter =>
               null;
            end case;

            if Input_Data.Rule_Count = 0 or Input_Data.Tokens.Rules.Length = 0 then
               raise WisiToken.Grammar_Error with "no rules";
            end if;
         end case;
      exception
      when E : WisiToken.Syntax_Error | WisiToken.Parse_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (E));
         Grammar_Parser.Put_Errors;
         raise;
      end Parse_Check;

      Cached_Recursions : WisiToken.Generate.Recursions := WisiToken.Generate.Empty_Recursions;
   begin
      --  Get the generate tuples
      Parse_Check (None, None, WisiToken_Grammar_Runtime.Meta);

      if Command_Generate_Set = null then
         if Input_Data.Generate_Set = null then
            raise User_Error with
              WisiToken.Generate.Error_Message
                (Grammar_Parser.Tree.Lexer.File_Name, 1,
                 "generate algorithm, output_language, lexer, interface not specified");
         end if;

         Generate_Set := Input_Data.Generate_Set;
      else
         Generate_Set := Command_Generate_Set;
      end if;

      Multiple_Tuples :=
        --  Preserve output file names when overriding file %generate
        (if Input_Data.Generate_Set = null then False else Input_Data.Generate_Set'Length > 1) or
           (if Command_Generate_Set = null then False else Command_Generate_Set'Length > 1);

      for Tuple of Generate_Set.all loop
         if Trace_Generate > Outline then
            Trace.New_Line;
            Trace.Put_Line ("process tuple " & Image (Tuple));
         end if;

         case Tuple.Gen_Alg is
         when None | External =>
            if Input_Data.Meta_Syntax = EBNF_Syntax and BNF_Tree.Is_Empty then
               --  'none' is used in unit tests to test bnf translation.
               Translate_To_BNF;
            end if;


         when Tree_Sitter =>
            Parse_Check
              (Lexer  => Tuple.Lexer,
               Parser => Tuple.Gen_Alg,
               Phase  => WisiToken_Grammar_Runtime.Other);

            declare
               use WisiToken.Generate.Tree_Sitter;

               procedure Translate (Tree : in out Syntax_Trees.Tree)
               is begin
                  if Trace_Generate > Outline then
                     Ada.Text_IO.New_Line;
                     Ada.Text_IO.Put_Line ("output tree_sitter grammar");
                  end if;

                  Eliminate_Empty_Productions (Input_Data, Tree);

                  Print_Tree_Sitter
                    (Input_Data,
                     Tree,
                     Tree.Lexer,
                     Output_File_Name => -Output_File_Name_Root & ".js",
                     Language_Name    => -Language_Name);

                  if WisiToken.Generate.Error then
                     --  FIXME: support --warning=error
                     raise WisiToken.Grammar_Error with "errors during translating grammar to tree-sitter: aborting";
                  end if;
               end Translate;
            begin

               Translate (Grammar_Parser.Tree);

               Create_Test_Main (-Output_File_Name_Root);
            end;

         when LR_Packrat_Generate_Algorithm =>
            Parse_Check
              (Lexer  => Tuple.Lexer,
               Parser => Tuple.Gen_Alg,
               Phase  => WisiToken_Grammar_Runtime.Other);

            declare
               use all type WisiToken.Parse.LR.Parse_Table_Ptr;
               use Ada.Real_Time;

               Time_Start : Time;
               Time_End   : Time;

               --  We could use a cached Generate_Data if not
               --  (Input_Data.If_Lexer_Present or Input_Data.If_Parser_Present), but
               --  that would not save much time, and would complicate this logic. We
               --  do cache Recursions.
               Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
                 WisiToken.BNF.Generate_Utils.Initialize
                   (Input_Data'Unchecked_Access, Grammar_Parser.Tree.Lexer.File_Name, Ignore_Conflicts);

               Packrat_Data : WisiToken.Generate.Packrat.Data
                 (Generate_Data.Descriptor.First_Terminal, Generate_Data.Descriptor.First_Nonterminal,
                  Generate_Data.Descriptor.Last_Nonterminal);

               Parse_Table_File_Name : constant String :=
                 (if Tuple.Gen_Alg in LALR .. Packrat_Proc
                  then -Output_File_Name_Root & "_" & To_Lower (Tuple.Gen_Alg'Image) &
                    (if Input_Data.If_Lexer_Present
                     then "_" & Lexer_Image (Input_Data.User_Lexer).all
                     else "") &
                    ".parse_table"
                  else "");

               procedure Parse_Table_Append_Stats
               is
                  Parse_Table_File : File_Type;
               begin
                  Open (Parse_Table_File, Append_File, Parse_Table_File_Name);
                  Set_Output (Parse_Table_File);
                  Generate_Data.Parser_State_Count :=
                    Generate_Data.LR_Parse_Table.State_Last - Generate_Data.LR_Parse_Table.State_First + 1;
                  WisiToken.BNF.Generate_Utils.Put_Stats (Input_Data, Generate_Data);
                  Set_Output (Standard_Output);
                  Close (Parse_Table_File);
               end Parse_Table_Append_Stats;

            begin
               if not Lexer_Done (Input_Data.User_Lexer) then
                  Lexer_Done (Input_Data.User_Lexer) := True;
                  case Input_Data.User_Lexer is
                  when re2c_Lexer =>
                     WisiToken.BNF.Output_Ada_Common.Create_re2c_File
                       (Input_Data, Tuple, Generate_Data, -Output_File_Name_Root);
                  when others =>
                     null;
                  end case;
               end if;

               case LR_Packrat_Generate_Algorithm'(Tuple.Gen_Alg) is
               when LALR =>

                  Time_Start := Clock;

                  if Generate_Data.Grammar (Generate_Data.Descriptor.Accept_ID).LHS = Invalid_Token_ID then
                     WisiToken.Generate.Put_Error
                       (WisiToken.Generate.Error_Message
                          (Grammar_Parser.Tree.Lexer.File_Name, 1,
                           "%start token not specified or not found; no LALR parse table generated"));
                  else
                     if Trace_Generate > Outline then
                        Trace.Put_Line ("generate LALR parse table");
                     end if;

                     Generate_Data.LR_Parse_Table := WisiToken.Generate.LR.LALR_Generate.Generate
                       (Generate_Data.Grammar,
                        Generate_Data.Descriptor.all,
                        Grammar_Parser.Tree.Lexer.File_Name,
                        Input_Data.Language_Params.Error_Recover,
                        Generate_Data.Conflicts,
                        Generate_Utils.To_McKenzie_Param (Generate_Data, Input_Data.McKenzie_Recover),
                        Input_Data.Max_Parallel,
                        Parse_Table_File_Name,
                        Include_Extra         => Test_Main,
                        Ignore_Conflicts      => Ignore_Conflicts,
                        Recursion_Strategy    => Input_Data.Language_Params.Recursion_Strategy,
                        Use_Cached_Recursions => not (Input_Data.If_Lexer_Present or Input_Data.If_Parser_Present),
                        Recursions            => Cached_Recursions);

                     if WisiToken.Trace_Time then
                        Time_End := Clock;

                        Put_Line
                          (Standard_Error,
                           "LALR " & Lexer_Image (Tuple.Lexer).all & " generate time:" &
                             Duration'Image (To_Duration (Time_End - Time_Start)));
                     end if;

                     if Parse_Table_File_Name /= "" then
                        Parse_Table_Append_Stats;
                     end if;
                  end if;

               when LR1 =>
                  Time_Start := Clock;

                  if Generate_Data.Grammar (Generate_Data.Descriptor.Accept_ID).LHS = Invalid_Token_ID then
                     WisiToken.Generate.Put_Error
                       (WisiToken.Generate.Error_Message
                          (Grammar_Parser.Tree.Lexer.File_Name, 1,
                           "%start token not specified or not found; no LALR parse table generated"));
                  else
                     if Trace_Generate > Outline then
                        Trace.Put_Line ("generate LR1 parse table");
                     end if;

                     Generate_Data.LR_Parse_Table := WisiToken.Generate.LR.LR1_Generate.Generate
                       (Generate_Data.Grammar,
                        Generate_Data.Descriptor.all,
                        Grammar_Parser.Tree.Lexer.File_Name,
                        Input_Data.Language_Params.Error_Recover,
                        Generate_Data.Conflicts,
                        Generate_Utils.To_McKenzie_Param (Generate_Data, Input_Data.McKenzie_Recover),
                        Input_Data.Max_Parallel,
                        Parse_Table_File_Name,
                        Include_Extra         => Test_Main,
                        Ignore_Conflicts      => Ignore_Conflicts,
                        Recursion_Strategy    => Input_Data.Language_Params.Recursion_Strategy,
                        Use_Cached_Recursions => not (Input_Data.If_Lexer_Present or Input_Data.If_Parser_Present),
                        Recursions            => Cached_Recursions);

                     if Trace_Time then
                        Time_End := Clock;

                        Put_Line
                          (Standard_Error,
                           "LR1 " & Lexer_Image (Tuple.Lexer).all & " generate time:" &
                             Duration'Image (To_Duration (Time_End - Time_Start)));
                     end if;

                     if Parse_Table_File_Name /= "" then
                        Parse_Table_Append_Stats;
                     end if;
                  end if;

               when Packrat_Generate_Algorithm =>
                  --  The only significant computation done for Packrat is First, done
                  --  in Initialize; not worth timing.

                  Packrat_Data := WisiToken.Generate.Packrat.Initialize
                    (Grammar_Parser.Tree.Lexer.File_Name, Generate_Data.Grammar, Generate_Data.Source_Line_Map,
                     Generate_Data.Descriptor.First_Terminal);

                  if Parse_Table_File_Name /= "" then
                     declare
                        Parse_Table_File : File_Type;
                     begin
                        Create (Parse_Table_File, Out_File, Parse_Table_File_Name);
                        Set_Output (Parse_Table_File);
                        Put_Line ("Tokens:");
                        WisiToken.Put_Tokens (Generate_Data.Descriptor.all);
                        New_Line;
                        Put_Line ("Productions:");
                        WisiToken.Productions.Put (Generate_Data.Grammar, Generate_Data.Descriptor.all);
                        Set_Output (Standard_Output);
                        Close (Parse_Table_File);
                     end;
                  end if;

                  Packrat_Data.Check_All (Generate_Data.Descriptor.all, Input_Data.Suppress);

                  if WisiToken.Generate.Warning then
                     WisiToken.Generate.Put_Warning ("warnings during packrat generation");
                  end if;
               end case;

               if WisiToken.Generate.Error then
                  raise WisiToken.Grammar_Error with "errors: aborting";
               end if;

               case Tuple.Gen_Alg is
               when LR_Generate_Algorithm =>
                  pragma Assert
                    (Generate_Data.LR_Parse_Table /= null and then
                     Generate_Data.LR_Parse_Table.Error_Recover_Enabled = Input_Data.Language_Params.Error_Recover);

                  if Tuple.Text_Rep then
                     WisiToken.Generate.LR.Put_Text_Rep
                       (Generate_Data.LR_Parse_Table.all,
                        Text_Rep_File_Name (-Output_File_Name_Root, Tuple, Input_Data.If_Lexer_Present));
                  end if;

               when others =>
                  null;
               end case;

               case Tuple.Out_Lang is
               when Ada_Lang =>
                  if Trace_Generate > Outline then
                     Trace.Put_Line ("output Ada");
                  end if;

                  WisiToken.BNF.Output_Ada
                    (Input_Data, Grammar_Parser.Tree.Lexer.File_Name, -Output_File_Name_Root, Generate_Data,
                     Packrat_Data, Tuple, Test_Main, Multiple_Tuples);

               when Ada_Emacs_Lang =>
                  if Trace_Generate > Outline then
                     Trace.Put_Line ("output Ada for Emacs");
                  end if;
                  WisiToken.BNF.Output_Ada_Emacs
                    (Input_Data, Grammar_Parser.Tree.Lexer.File_Name, -Output_File_Name_Root, Generate_Data,
                     Packrat_Data, Tuple, Test_Main, Multiple_Tuples, -Language_Name);

               end case;
               if WisiToken.Generate.Error then
                  raise WisiToken.Grammar_Error with "errors: aborting";
               end if;
            end;
         end case;
      end loop;
   end;
exception
when WisiToken.Syntax_Error | WisiToken.Parse_Error =>
   --  error message already output
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

when E : User_Error =>
   declare
      use Ada.Command_Line;
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Put_Command_Line (Ada_Comment);
      Set_Exit_Status (Failure);
      Put_Usage;
   end;

when E : WisiToken.Grammar_Error =>
   --  error message not already output
   declare
      use Ada.Command_Line;
      use Ada.Exceptions;
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);
   end;

when E :  others =>
   --  IMPROVEME: for some exceptions, Error message already output via wisi.utils.Put_Error
   declare
      use Ada.Text_IO;
      use Ada.Exceptions;
      use Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;

end WisiToken.BNF.Generate;
