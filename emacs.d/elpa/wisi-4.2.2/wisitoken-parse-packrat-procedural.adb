--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with WisiToken.In_Parse_Actions;
package body WisiToken.Parse.Packrat.Procedural is

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Apply_Rule'Result.State in Failure .. Success;

   function Eval
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Eval'Result.State in Failure .. Success;

   ----------
   --  bodies

   procedure Trace_Deriv_Success
     (Parser : in out Procedural.Parser;
      Pos    : in     WisiToken.Syntax_Trees.Stream_Index;
      Memo   : in     Success_Memo_Entry)
   is
      use Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      --  match LR parse trace with trace_parse=2
      Tree.Lexer.Trace.Put_Line
        ((if Trace_Parse > Extra
          then Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Pos)) & "; "
          else "") &
           Tree.Image
             (Memo.Result,
              Children              => True,
              Node_Numbers          => Trace_Parse > Extra,
              Terminal_Node_Numbers => True,
              RHS_Index             => True) &
           (if Trace_Parse > Extra
            then " last_pos" & Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Memo.Last_Pos)) &
              " max_ex_pos" & Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Memo.Max_Examined_Pos))
            else ""));
   end Trace_Deriv_Success;

   function Eval
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      Pos      : Syntax_Trees.Stream_Index := Last_Pos; --  last token parsed.
      Next_Pos : Syntax_Trees.Stream_Index := Tree.Stream_Next (Tree.Shared_Stream, Pos);

      Max_Examined_Pos : Syntax_Trees.Stream_Index := Last_Pos;

      procedure Update_Max_Examined_Pos (New_Pos : in Syntax_Trees.Stream_Index)
      is begin
         if Tree.Byte_Region
           (Tree.Get_Node (Tree.Shared_Stream, New_Pos), Trailing_Non_Grammar => False).First >
           Tree.Byte_Region
             (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos), Trailing_Non_Grammar => False).First
         then
            Max_Examined_Pos := New_Pos;
         end if;
      end Update_Max_Examined_Pos;

   begin
      if Trace_Parse > Extra then
         Trace.Put_Line ("eval: " & Image (R, Descriptor) & " @" & Image_Pos (Next_Pos));
      end if;

      for RHS_Index in Parser.Grammar (R).RHSs.First_Index .. Parser.Grammar (R).RHSs.Last_Index loop
         declare
            use all type Ada.Containers.Count_Type;
            RHS  : WisiToken.Productions.Right_Hand_Side renames Parser.Grammar (R).RHSs (RHS_Index);
            Memo : Memo_Entry; --  for temporary or intermediate results
         begin
            if RHS.Tokens.Length = 0 then
               return Result : constant Memo_Entry :=
                 (State            => Success,
                  Max_Examined_Pos => Max_Examined_Pos,
                  Result           => Tree.Add_Nonterm
                    (Production    => (R, RHS_Index),
                     Children      => (1 .. 0 => Syntax_Trees.Invalid_Node_Access),
                     Clear_Parents => False),
                  Last_Pos         => Pos)
               do
                  if Trace_Parse > Extra then
                     Trace.Put_Line (Image (Result, R, RHS_Index, Pos, Tree));
                  end if;
               end return;
            else
               declare
                  use all type WisiToken.Syntax_Trees.In_Parse_Actions.In_Parse_Action;
                  use all type WisiToken.Syntax_Trees.Node_Access;
                  Children : Syntax_Trees.Node_Access_Array
                    (SAL.Base_Peek_Type (RHS.Tokens.First_Index) .. SAL.Base_Peek_Type (RHS.Tokens.Last_Index));

                  In_Parse_Action : constant Syntax_Trees.In_Parse_Actions.In_Parse_Action :=
                    Parser.Get_In_Parse_Action ((R, RHS_Index));
               begin
                  for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if Trace_Parse > Extra then
                        Trace.Put (Image (Production_ID'(R, RHS_Index), Descriptor) & "," & I'Image & ": ");
                     end if;

                     if RHS.Tokens (I) in Terminal then
                        if Next_Pos = Syntax_Trees.Invalid_Stream_Index then
                           --  We don't update Max_Examined_Pos here; it must already be EOI
                           pragma Assert (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos) = Tree.EOI);
                           goto Fail_RHS;

                        else
                           if Children (SAL.Base_Peek_Type (I)) = Syntax_Trees.Invalid_Node_Access then
                              if Tree.ID (Next_Pos) = RHS.Tokens (I) then
                                 Pos := Next_Pos;
                                 Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
                                 Children (SAL.Base_Peek_Type (I)) := Tree.Get_Node (Tree.Shared_Stream, Pos);
                                 --  FIXME: why not Update_Max_Examined_Pos here?

                                 if Trace_Parse > Extra then
                                    Trace.Put_Line
                                      (Tree.Image (Children (SAL.Base_Peek_Type (I)), Node_Numbers => True));
                                 end if;

                              else
                                 Update_Max_Examined_Pos (Next_Pos);
                                 goto Fail_RHS;
                              end if;
                           end if;
                        end if;

                     else -- not Terminal
                        Memo := Apply_Rule (Parser, RHS.Tokens (I), Pos);
                        Update_Max_Examined_Pos (Memo.Max_Examined_Pos);

                        case Memo.State is
                        when Success =>
                           if Trace_Parse > Extra then
                              Trace.Put_Line
                                (Image (Production_ID'(R, RHS_Index), Descriptor) & "," & I'Image & ": " &
                                   Tree.Image (Memo.Result, Node_Numbers => True, RHS_Index => True));
                           end if;
                           Children (SAL.Base_Peek_Type (I)) := Memo.Result;
                           Pos := Memo.Last_Pos;
                           Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);

                        when Failure =>
                           goto Fail_RHS;

                        when No_Result =>
                           raise SAL.Programmer_Error;
                        end case;
                     end if;
                  end loop;

                  return Result : constant Memo_Entry :=
                    (State            => Success,
                     Max_Examined_Pos => Max_Examined_Pos,
                     Result           => Parser.Tree.Add_Nonterm
                       (Production    => (R, RHS_Index),
                        Children      => Syntax_Trees.To_Valid_Node_Access (Children),
                        Clear_Parents => True),
                     --  We must be able to steal nodes from failed nonterms;
                     --  body_instantiation_conflict.wy.
                     Last_Pos         => Pos)
                  do
                     if Trace_Parse > Extra then
                        Trace.Put_Line (Image (Result, R, RHS_Index, Pos, Tree));
                     end if;

                     if In_Parse_Action = null then
                        null;

                     else
                        declare
                           Nonterm_Token : Syntax_Trees.Recover_Token := Parser.Tree.Get_Recover_Token (Result.Result);

                           Children_Token : constant Syntax_Trees.Recover_Token_Array :=
                             Parser.Tree.Children_Recover_Tokens (Result.Result);
                           Status         : constant Syntax_Trees.In_Parse_Actions.Status := In_Parse_Action
                             (Parser.Tree, Nonterm_Token, Children_Token, Recover_Active => False);
                        begin
                           if Trace_Parse > Extra then
                              Trace.Put_Line
                                ("in_parse_action " & WisiToken.In_Parse_Actions.Image
                                   (Status, Tree, Result.Result));
                           end if;

                           case Status.Label is
                           when Syntax_Trees.In_Parse_Actions.Ok =>
                              null;

                           when Syntax_Trees.In_Parse_Actions.Error =>
                              raise SAL.Not_Implemented with "packrat in_parse_actions fail";
                              --  FIXME: store the error somewhere, raise a different exception?
                              --  Parser.Tree.Add_Error_To_Stack_Top
                              --    (Parser_State.Stream,
                              --     In_Parse_Action_Error'
                              --       (Status       => Status,
                              --        Recover_Ops  => Recover_Op_Arrays.Empty_Vector,
                              --        Recover_Cost => 0),
                              --     Parser.User_Data);
                           end case;
                        end;
                     end if;
                  end return;

                  <<Fail_RHS>>
                  if Trace_Parse > Extra then
                     Trace.Put_Line
                       (Image (Production_ID'(R, RHS_Index), Descriptor) & " @" &
                          Image_Pos (Next_Pos) & ": fail");
                  end if;
                  Pos := Last_Pos;
                  Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
               end;
            end if;
         end;
      end loop;
      --  get here when all RHSs fail

      return (Failure, Max_Examined_Pos);
   end Eval;

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;
      use all type WisiToken.Syntax_Trees.Node_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;

      Pos       : Syntax_Trees.Stream_Index          := Last_Pos; --  last token parsed.
      Start_Pos : constant Syntax_Trees.Stream_Index := Tree.Stream_Next
        (Tree.Shared_Stream, Last_Pos);                         --  first token in current nonterm
      Memo      : Memo_Entry                         := Get_Deriv
        (Parser.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));

      Pos_Recurse_Last : Syntax_Trees.Stream_Index := Last_Pos;
      Result_Recurse   : Memo_Entry;
   begin
      case Memo.State is
      when Success | Failure =>
         if Trace_Parse > Extra then
            Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));
         end if;
         return Memo;

      when No_Result =>
         if Parser.Direct_Left_Recursive (R) then
            Set_Deriv
              (Parser.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos),
               (Failure, Last_Pos));
         else
            Memo := Eval (Parser, R, Last_Pos);

            if Trace_Parse > Extra then
               Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));

            elsif Trace_Parse > Detail and Memo.State = Success then
               --  Match LR parse trace detail
               Trace_Deriv_Success (Parser, Start_Pos, Memo);
            end if;
            Set_Deriv (Parser.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Memo);
            return Memo;
         end if;
      end case;

      if Trace_Parse > Extra then
         Trace.Put_Line ("apply recursive " & Image (R, Descriptor));
      end if;

      loop
         --  Production is like: list : list element | element
         --
         --  Each time around this loop starts at the same point, but
         --  accumulates more tokens in the first 'list'; it exits when
         --  'element' does not match the remaining input.
         Pos := Last_Pos;

         Result_Recurse := Eval (Parser, R, Pos);

         if Result_Recurse.State = Success then
            if Tree.Get_Node_Index (Tree.Shared_Stream, Result_Recurse.Last_Pos) >
              Tree.Get_Node_Index (Tree.Shared_Stream, Pos_Recurse_Last)
            then
               Set_Deriv (Parser.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               Pos              := Result_Recurse.Last_Pos;
               Pos_Recurse_Last := Pos;

               if WisiToken.Trace_Parse > Detail then
                  Trace_Deriv_Success (Parser, Start_Pos, Result_Recurse);
                  if Trace_Parse > Extra then
                     Trace.Put_Line ("apply recursive continue");
                  end if;
               end if;
               --  continue looping

            elsif Result_Recurse.Last_Pos = Pos_Recurse_Last then
               if Parser.Tree.Is_Empty_Nonterm (Result_Recurse.Result) then
                  Set_Deriv
                    (Parser.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               end if;
               exit;
            else
               --  Result_Recurse.Last_Pos < Pos_Recurse_Last
               exit;
            end if;
         else
            exit;
         end if;
      end loop;

      declare
         Result : Memo_Entry renames Parser.Derivs (R)(Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));
      begin
         Result.Max_Examined_Pos := Result_Recurse.Max_Examined_Pos;
         if Trace_Parse > Extra then
            Trace.Put_Line
              ("apply recursive " & Image (R, Descriptor) & " end: " & Image (Result, R, Pos, Tree));
         end if;

         return Result;
      end;
   end Apply_Rule;

   ----------
   --  Public subprograms

   function Create
     (Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Direct_Left_Recursive : in Token_ID_Set;
      Start_ID              : in Token_ID;
      Lexer                 : in WisiToken.Lexer.Handle;
      Productions           : in WisiToken.Syntax_Trees.Production_Info_Trees.Vector;
      User_Data             : in WisiToken.Syntax_Trees.User_Data_Access)
     return Procedural.Parser
   is begin
      return Parser                   : Procedural.Parser (Grammar.First_Index, Grammar.Last_Index) do
         Parser.Tree.Lexer            := Lexer;
         Parser.Productions           := Productions;
         Parser.User_Data             := User_Data;
         Parser.Grammar               := Grammar;
         Parser.Start_ID              := Start_ID;
         Parser.Direct_Left_Recursive := Direct_Left_Recursive;
      end return;
   end Create;

   overriding procedure Parse
     (Parser     : in out Procedural.Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is
      pragma Unreferenced (Log_File, Pre_Edited);
      use all type Ada.Containers.Count_Type;
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;

      Result : Memo_Entry;
   begin
      if Edits.Length > 0 then
         raise WisiToken.Parse_Error;
      end if;

      if Trace_Time then
         Trace.Put_Clock ("start");
      end if;

      Parser.Tree.Clear;
      --  Creates Shared_Stream, but no parse stream; packrat does not
      --  use a parse stream.

      Clear (Parser.Derivs);

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Lex_All;

      Result := Apply_Rule
        (Parser, Parser.Start_ID,  Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));

      Parser.Finish_Parse (Result);
   end Parse;

end WisiToken.Parse.Packrat.Procedural;
