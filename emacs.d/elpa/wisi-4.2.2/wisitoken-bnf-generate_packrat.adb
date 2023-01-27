--  Abstract :
--
--  Generate Ada code for a Packrat parser.
--
--  References:
--
--  See wisitoken-parse-packrat.ads.
--
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
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

with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.Packrat;
with WisiToken.Productions;
procedure WisiToken.BNF.Generate_Packrat
  (Data          : in WisiToken.Generate.Packrat.Data;
   Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
is
   use WisiToken.Generate;

   Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

   subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

   --  FIXME packrat: optimize memoizing? small productions not worth the
   --  memory cost? or just use langkit space optimization.

   function Parser_Name (Nonterm : in Token_ID) return String
   is begin
      return "Parse_" & Image (Nonterm, Descriptor);
   end Parser_Name;

   procedure Put_Parser_Spec (Name : in String)
   is begin
      Indent_Line ("function " & Name);
      Indent_Start
        ("  (Parser : in out Generated.Parser; Last_Pos : in Syntax_Trees.Stream_Index) return Result_Type");
   end Put_Parser_Spec;

   function Var_Suffix (I, J : in Integer) return String
   is begin
      return Trimmed_Image (I) & '_' & Trimmed_Image (J);
   end Var_Suffix;

   procedure Generate_Parser_Body (Prod : in Productions.Instance)
   --  Generate the parser function for one production.
   is
      use all type Ada.Containers.Count_Type;

      Result_ID : constant String := Trimmed_Image (Prod.LHS);
   begin
      --  We use gotos and function scope vars rather than nested if/declare
      --  to avoid excessive indenting for long productions.

      Put_Parser_Spec (Parser_Name (Prod.LHS)); New_Line;
      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("Tree       : Syntax_Trees.Tree renames Parser.Tree;");
      Indent_Line ("Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;");
      Indent_Line
        ("Start_Pos  : constant Syntax_Trees.Stream_Index := Tree.Stream_Next (Tree.Shared_Stream, Last_Pos);");
      Indent_Line ("Start_Pos_Index  : constant Syntax_Trees.Node_Index :=");
      Indent_Line ("  Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos);");
      Indent_Line ("Pos              : Syntax_Trees.Stream_Index := Last_Pos; --  last token parsed.");
      Indent_Line ("Next_Pos         : Syntax_Trees.Stream_Index := Start_Pos;");
      Indent_Line ("Max_Examined_Pos : Syntax_Trees.Stream_Index := Last_Pos;");

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);
         begin
            for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
               if RHS.Tokens (Token_Index) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                  Indent_Line ("Pos_" & Var_Suffix (RHS_Index, Token_Index) & "  : Syntax_Trees.Stream_Index;");
               else
                  Indent_Line ("Memo_" & Var_Suffix (RHS_Index, Token_Index) & " : Memo_Entry;");
               end if;
            end loop;
         end;
      end loop;

      if Data.Direct_Left_Recursive (Prod.LHS) then
         Indent_Line ("Pos_Recurse_Last : Syntax_Trees.Stream_Index := Last_Pos;");
         Indent_Line ("Result_Recurse   : Memo_Entry;");
      end if;

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      Indent_Line ("if Next_Pos = Syntax_Trees.Invalid_Stream_Index then");
      Indent_Line ("   return (State => Failure, Max_Examined_Pos => Next_Pos);");
      Indent_Line ("end if;");
      Indent_Line ("declare");
      Indent_Line
        ("   Memo : Memo_Entry := Get_Deriv (Parser.Derivs, " & Result_ID & ", Start_Pos_Index);");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("case Memo.State is");
      Indent_Line ("when Success =>");
      Indent_Line ("   return Parser.Derivs (" & Result_ID & ")(Start_Pos_Index);");
      Indent_Line ("when Failure =>");

      Indent_Line ("   goto RHS_" & Trimmed_Image (Prod.RHSs.Last_Index) & "_Fail;");

      Indent_Line ("when No_Result =>");
      Indent_Line ("   if Memo.Recursive then");
      Indent_Line ("      raise Recursive with Image (" & Result_ID & ", Descriptor) &");
      Indent_Line ("        Start_Pos_Index'Image & "": recursive"";");
      Indent_Line ("   end if;");
      Indent_Line ("   Memo.Recursive := True;");
      Indent_Line ("end case;");
      Indent := Indent - 3;
      Indent_Line ("end;");
      New_Line;

      if Data.Direct_Left_Recursive (Prod.LHS) then
         --  This is the top of the 'while' loop in [warth 2008] figure 3 Grow-LR.
         Indent_Line
           ("Set_Deriv (Parser.Derivs, " & Result_ID &
              ", Start_Pos_Index, (State => Failure, Max_Examined_Pos => Next_Pos));");
         Indent_Line ("<<Recurse_Start>>");
      end if;

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);

            procedure Finish
            is begin
               if Data.Direct_Left_Recursive (Prod.LHS) then
                  Indent_Line ("Result_Recurse :=");
                  Indent := Indent + 2;
               else
                  Indent_Line ("Set_Deriv");
                  Indent_Line ("  (Parser.Derivs, " & Result_ID & ", Start_Pos_Index,");
                  Indent := Indent + 3;
               end if;
               Indent_Line ("(State            => Success,");
               Indent_Line (" Max_Examined_Pos => Max_Examined_Pos,");
               Indent_Line (" Result           => Parser.Tree.Add_Nonterm");

               Indent := Indent + 3;
               Indent_Line ("(Production    => (" & Result_ID & ", " & Trimmed_Image (RHS_Index) & "),");

               if RHS.Tokens.Length = 0 then
                  Indent_Line (" Children      => (1 .. 0 => Syntax_Trees.Invalid_Node_Access),");

               elsif RHS.Tokens.Length = 1 then
                  Indent_Start (" Children      => ");
                  if RHS.Tokens (RHS.Tokens.First_Index) in Terminal then
                     Put ("(1 => Tree.Get_Node (Tree.Shared_Stream, Pos_" &
                            Var_Suffix (RHS_Index, RHS.Tokens.First_Index) & ")),");
                  else
                     Put ("(1 => Memo_" & Var_Suffix (RHS_Index, RHS.Tokens.First_Index) & ".Result),");
                  end if;

               else
                  Indent_Line (" Children      =>");

                  for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if RHS.Tokens (Token_Index) in Terminal then
                        Indent_Start
                          ((if Token_Index = RHS.Tokens.First_Index
                            then "  ("
                            else "   ") &
                             "Tree.Get_Node (Tree.Shared_Stream, Pos_" & Var_Suffix (RHS_Index, Token_Index) & ")");
                     else
                        Indent_Start
                          ((if Token_Index = RHS.Tokens.First_Index
                            then "  ("
                            else "   ") &
                             "Memo_" & Var_Suffix (RHS_Index, Token_Index) & ".Result");
                     end if;
                     if Token_Index = RHS.Tokens.Last_Index then
                        Put_Line ("),");
                     else
                        Put_Line (",");
                     end if;
                  end loop;
               end if;

               Indent_Line (" Clear_Parents => True),");
               --  We must be able to steal nodes from failed nonterms;
               --  body_instantiation_conflict.wy.

               Indent := Indent - 3;
               Indent_Start (" Last_Pos         => Pos)");

               if Data.Direct_Left_Recursive (Prod.LHS) then
                  Put_Line (";");
                  Indent := Indent - 2;
                  Indent_Line ("goto Finish;");
               else
                  Put_Line (");");
                  Indent := Indent - 3;
                  Indent_Line ("goto Succeed;");
               end if;
            end Finish;

         begin
            Indent_Wrap_Comment (Productions.Image (Prod.LHS, RHS_Index, RHS.Tokens, Descriptor), Ada_Comment);
            Indent_Line ("Pos := Last_Pos;");
            Indent_Line ("Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);");

            if RHS.Tokens.Length = 0 then
               Finish;
            else
               for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                  declare
                     ID      : constant String := Trimmed_Image (RHS.Tokens (Token_Index));
                     ID_Name : constant String := Image (RHS.Tokens (Token_Index), Descriptor);
                     Var_Suf : constant String := Var_Suffix (RHS_Index, Token_Index);
                  begin
                     if RHS.Tokens (Token_Index) in Terminal then
                        Indent_Line ("Update (Parser, """ & ID_Name & """, Next_Pos, Max_Examined_Pos);");
                        Indent_Line ("if Tree.ID (Next_Pos) = " & ID & " then");
                        Indent := Indent + 3;
                        Indent_Line ("Pos := Next_Pos;");
                        Indent_Line ("Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);");
                        Indent_Line ("Pos_" & Var_Suf & " := Pos;");
                        if Token_Index = RHS.Tokens.Last_Index then
                           Finish;
                        end if;
                        Indent := Indent - 3;
                        Indent_Line ("else");
                        Indent_Line ("   goto RHS_" & Trimmed_Image (RHS_Index) & "_Fail;");
                        Indent_Line ("end if;");

                     else -- nonterminal
                        Indent_Line
                          ("Memo_" & Var_Suf & " := Parse_" & Image (RHS.Tokens (Token_Index), Descriptor) &
                             " (Parser, Pos);");
                        Indent_Line
                          ("Update (Parser, """ & ID_Name & """, Memo_" & Var_Suf &
                             ".Max_Examined_Pos, Max_Examined_Pos);");
                        Indent_Line ("case Result_States'(Memo_" & Var_Suf & ".State) is");
                        Indent_Line ("when Success =>");
                        Indent := Indent + 3;
                        Indent_Line ("Pos := Memo_" & Var_Suf & ".Last_Pos;");
                        Indent_Line ("Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);");
                        if Token_Index = RHS.Tokens.Last_Index then
                           Finish;
                        end if;
                        Indent := Indent - 3;
                        Indent_Line ("when Failure =>");
                        Indent_Line ("   goto RHS_" & Trimmed_Image (RHS_Index) & "_Fail;");
                        Indent_Line ("end case;");
                     end if;
                  end;
               end loop;
            end if;

            New_Line;
            Indent_Line ("<<RHS_" & Trimmed_Image (RHS_Index) & "_Fail>>");
         end;
      end loop;

      --  We get here if the last alternative fails.
      if Data.Direct_Left_Recursive (Prod.LHS) then
         Indent_Line ("Result_Recurse := (State => Failure, Max_Examined_Pos => Max_Examined_Pos);");
      else
         Indent_Line ("Set_Deriv");
         Indent_Line ("   (Parser.Derivs, " & Result_ID & ", Start_Pos_Index,");
         Indent_Line ("    (State => Failure, Max_Examined_Pos => Max_Examined_Pos));");
         Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos_Index);");
      end if;

      if Data.Direct_Left_Recursive (Prod.LHS) then
         Indent_Line ("<<Finish>>");
         Indent_Line ("if Result_Recurse.State = Success then");
         Indent := Indent + 3;
         Indent_Line ("if Tree.Get_Node_Index (Tree.Shared_Stream, Pos) >");
         Indent_Line ("  Tree.Get_Node_Index (Tree.Shared_Stream, Pos_Recurse_Last)");
         Indent_Line ("then");
         --  made progress, try again
         Indent := Indent + 3;
         Indent_Line ("Set_Deriv (Parser.Derivs, " & Result_ID & ", Start_Pos_Index, Result_Recurse);");
         Indent_Line ("Pos_Recurse_Last := Pos;");
         Indent_Line ("if WisiToken.Trace_Parse > Detail then");
         Indent_Line ("   Tree.Lexer.Trace.Put_Line");
         Indent_Line ("     (Parser.Tree.Image (Result_Recurse.Result,");
         Indent_Line ("      Children => True, Terminal_Node_Numbers => True, RHS_Index => True));");
         Indent_Line ("end if;");
         Indent_Line ("goto Recurse_Start;");
         Indent := Indent - 3;
         Indent_Line
           ("elsif Pos = Pos_Recurse_Last and then " &
              "Parser.Tree.Is_Empty_Nonterm (Result_Recurse.Result) then");
         --  Parse succeeded producing an empty nonterm; don't try again. This
         --  special case is not in [warth 2008].
         Indent_Line ("   Set_Deriv (Parser.Derivs, " & Result_ID & ", Start_Pos_Index, Result_Recurse);");
         Indent_Line ("end if;");
         Indent := Indent - 3;
         Indent_Line ("end if;");
      end if;
      New_Line;

      if not Data.Direct_Left_Recursive (Prod.LHS) then
         Indent_Line ("<<Succeed>>");
         Indent_Line ("if WisiToken.Trace_Parse > Detail then");
         Indent := Indent + 3;
         Indent_Line ("Tree.Lexer.Trace.Put_Line");
         Indent_Line ("  (Parser.Tree.Image");
         Indent_Line ("    (Parser.Derivs (" & Result_ID & ")(Start_Pos_Index).Result,");
         Indent_Line ("     Children => True, Terminal_Node_Numbers => True, RHS_Index => True));");
         Indent := Indent - 3;
         Indent_Line ("end if;");
      end if;

      Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos_Index);");
      Indent := Indent - 3;
      Indent_Line ("end " & Parser_Name (Prod.LHS) & ";");
      New_Line;
   end Generate_Parser_Body;

begin
   Indent_Line ("use WisiToken;");
   Indent_Line ("use WisiToken.Parse.Packrat;");
   Indent_Line ("use WisiToken.Parse.Packrat.Generated;");
   Indent_Line ("use all type WisiToken.Syntax_Trees.Stream_Index;");
   Indent_Line ("use all type WisiToken.Syntax_Trees.Node_Index;");

   for Prod of Data.Grammar loop
      Put_Parser_Spec (Parser_Name (Prod.LHS)); Put_Line (";");
   end loop;
   New_Line;

   Indent_Line ("procedure Update");
   Indent_Line ("  (Parser           : in out Generated.Parser;");
   Indent_Line ("   Nonterm          : in     String;");
   Indent_Line ("   New_Pos          : in     Syntax_Trees.Stream_Index;");
   Indent_Line ("   Max_Examined_Pos : in out Syntax_Trees.Stream_Index)");
   Indent_Line ("is");
   Indent_Line ("   Tree : Syntax_Trees.Tree renames Parser.Tree;");
   Indent_Line ("begin");
   Indent_Line ("   if Tree.Byte_Region");
   Indent_Line ("     (Tree.Get_Node (Tree.Shared_Stream, New_Pos), Trailing_Non_Grammar => False).First >");
   Indent_Line ("     Tree.Byte_Region");
   Indent_Line ("       (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos), Trailing_Non_Grammar => False).First");
   Indent_Line ("   then");
   Indent_Line ("      Max_Examined_Pos := New_Pos;");
   Indent_Line ("   end if;");
   Indent_Line ("   if Trace_Parse > Extra then");
   Indent_Line ("      Tree.Lexer.Trace.Put_Line");
   Indent_Line ("        (Nonterm & "": max_examined_pos "" & Tree.Image");
   Indent_Line ("          (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos), Node_Numbers => True));");
   Indent_Line ("   end if;");
   Indent_Line ("end Update;");

   for Prod of Data.Grammar loop
      Generate_Parser_Body (Prod);
   end loop;

end WisiToken.BNF.Generate_Packrat;
