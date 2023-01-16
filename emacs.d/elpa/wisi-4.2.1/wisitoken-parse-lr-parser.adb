--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with WisiToken.In_Parse_Actions;
with WisiToken.Parse.LR.McKenzie_Recover;
package body WisiToken.Parse.LR.Parser is

   function Reduce_Stack_1
     (Shared_Parser  : in out Parser;
      Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      New_State      : in     State_Index)
     return Syntax_Trees.In_Parse_Actions.Status_Label
   is
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      use all type Syntax_Trees.In_Parse_Actions.Status_Label;
      use all type Syntax_Trees.In_Parse_Actions.In_Parse_Action;

      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;

      Nonterm : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, New_State,
         Recover_Conflict => Parser_State.Resume_Active and Shared_Parser.Parsers.Count > 1);

      In_Parse_Action : constant Syntax_Trees.In_Parse_Actions.In_Parse_Action := Shared_Parser.Get_In_Parse_Action
        (Action.Production);
   begin
      if Trace_Parse > Detail then
         Shared_Parser.Tree.Lexer.Trace.Put_Line
           (Shared_Parser.Tree.Image
              (Nonterm.Node,
               Children              => True,
               Terminal_Node_Numbers => True,
               RHS_Index             => True));
      end if;

      if In_Parse_Action = null then
         return Ok;

      else
         --  We have to call the semantic action even when Resume_Active,
         --  because it might do other things than return a status (ie
         --  Propagate_Name).
         declare
            Nonterm_Token : Syntax_Trees.Recover_Token := Shared_Parser.Tree.Get_Recover_Token (Nonterm);

            Children_Token : constant Syntax_Trees.Recover_Token_Array :=
              Shared_Parser.Tree.Children_Recover_Tokens (Parser_State.Stream, Nonterm.Element);
            Status         : Syntax_Trees.In_Parse_Actions.Status;
         begin
            Status := In_Parse_Action
              (Shared_Parser.Tree, Nonterm_Token, Children_Token, Recover_Active => False);

            if Trace_Parse > Detail then
               Shared_Parser.Tree.Lexer.Trace.Put_Line
                 ("in_parse_action " & WisiToken.In_Parse_Actions.Image (Status, Shared_Parser.Tree, Nonterm.Node));
            end if;

            case Status.Label is
            when Ok =>
               return Ok;

            when Syntax_Trees.In_Parse_Actions.Error =>
               if Parser_State.Resume_Active then
                  --  Ignore this error; that's how McKenzie_Recover decided to fix it
                  return Ok;

               else
                  Shared_Parser.Tree.Add_Error_To_Stack_Top
                    (Parser_State.Stream,
                     In_Parse_Action_Error'
                       (Status       => Status,
                        Recover_Ops  => Recover_Op_Nodes_Arrays.Empty_Vector,
                        Recover_Test => null),
                     Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));

                  return Status.Label;
               end if;
            end case;
         end;
      end if;
   end Reduce_Stack_1;

   procedure Get_Action
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State;
      Action_Cur    :    out Parse_Action_Node_Ptr;
      Action        :    out Parse_Action_Rec)
   is
      --  Same logic as in McKenzie_Recover.Parse.Get_Action, but this
      --  operates on Parser_State.
      use Syntax_Trees;

      Table : Parse_Table renames Shared_Parser.Table.all;
      Tree  : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      loop -- handle delete empty nonterm, undo_reduce
         declare
            Current_State : constant State_Index := Tree.State (Parser_State.Stream);

            function Handle_Error return Boolean
            --  Return True if should return immediately; False if Undo_Reduce was done.
            is begin
               if Tree.Label (Tree.Peek (Parser_State.Stream)) in Terminal_Label then
                  return True;

               else
                  --  [Wagner Graham 1998] has Right_Breakdown here, but that is often
                  --  overkill; we only need Undo_Reduce until Current_Token is
                  --  shiftable. ada_mode-interactive_03.adb
                  --
                  --  IMPROVEME: if error recovery is correct here, we do more
                  --  Undo_Reduce than necessary (and it will never happen in
                  --  Error_Recovery).

                  if Parser_State.Last_Action.Verb = Reduce then
                     --  We are in an erroneous branch of a conflict, or there is a real error.
                     --  ada_mode-incremental_01.adb
                     return True;
                  end if;

                  if Trace_Parse > Detail then
                     Shared_Parser.Tree.Lexer.Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                          Trimmed_Image (Current_State) & ": " & Tree.Image
                            (Shared_Parser.Tree.Current_Token (Parser_State.Stream), First_Terminal => True) &
                          " error; undo_reduce");
                     Shared_Parser.Tree.Lexer.Trace.Put
                       (" ... " & Tree.Image (Tree.Peek (Parser_State.Stream), State => True));
                  end if;
                  Undo_Reduce
                    (Tree, Table, Parser_State.Stream, Syntax_Trees.User_Data_Access_Constant
                       (Shared_Parser.User_Data));

                  if Trace_Parse > Detail then
                     Shared_Parser.Tree.Lexer.Trace.Put
                       (" => " & Tree.Image (Tree.Peek (Parser_State.Stream), State => True),
                        Prefix => False);
                     Shared_Parser.Tree.Lexer.Trace.New_Line;
                  end if;
                  return False;
               end if;
            end Handle_Error;

            Current_Node : constant Valid_Node_Access := Shared_Parser.Tree.Current_Token (Parser_State.Stream).Node;

         begin
            if Tree.Label (Current_Node) in Terminal_Label then
               Action_Cur := Action_For (Table, Current_State, Tree.ID (Current_Node));
               Action     := Action_Cur.Item;

               case Action.Verb is
               when Shift | Accept_It | Reduce =>
                  return;
               when Error =>
                  if Handle_Error then
                     return;
                  end if;
               end case;
            else
               declare
                  New_State : constant Unknown_State_Index := Goto_For
                    (Table, Current_State, Tree.ID (Current_Node));
               begin
                  if New_State /= Unknown_State then
                     Action_Cur := null;
                     Action     :=
                       (Verb       => Shift,
                        Production => Invalid_Production_ID,
                        State      => New_State);
                     return;
                  else
                     declare
                        Checking_Next : Boolean := False;

                        procedure Delete_Empty
                        is begin
                           if Trace_Parse > Detail then
                              Shared_Parser.Tree.Lexer.Trace.Put_Line
                                (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                                   (if Trace_Parse_No_State_Numbers
                                    then "-- : "
                                    else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
                                   ": delete empty nonterm " &
                                   Tree.Image
                                     (Shared_Parser.Tree.Current_Token (Parser_State.Stream), First_Terminal => True));
                           end if;

                           Tree.Delete_Current_Token (Parser_State.Stream);
                        end Delete_Empty;

                        function Get_First_Terminal return Valid_Node_Access
                        is
                           Temp : Node_Access := Tree.First_Terminal (Current_Node);
                        begin
                           if Temp = Invalid_Node_Access then
                              --  Current_Token is an empty nonterm; peek at next terminal,
                              --  do reduce until this nonterm is shiftable.
                              --  ada_mode-interactive_03.adb
                              --  test_incremental.adb Recover_1 aspect_specification_opt.
                              Temp := Tree.First_Terminal (Shared_Parser.Tree.Current_Token (Parser_State.Stream)).Node;
                              Checking_Next := True;
                           end if;
                           return Temp;
                        end Get_First_Terminal;

                        First_In_Current : constant Valid_Node_Access := Get_First_Terminal;

                     begin
                        Action_Cur := Action_For (Table, Current_State, Tree.ID (First_In_Current));
                        Action     := Action_Cur.Item;

                        case Action.Verb is
                        when Shift =>
                           if Checking_Next then
                              --  If the empty nonterm was shiftable, it would have been handled by
                              --  Goto_For above. test_incremental.adb Edit_Code_9. Edit_Tree could
                              --  delete this nonterm, but handling it here is simpler.
                              Delete_Empty;

                           else
                              declare
                                 Current_Token : Rooted_Ref := Shared_Parser.Tree.Current_Token (Parser_State.Stream);
                              begin
                                 if Shared_Parser.Tree.Current_Token (Parser_State.Stream).Stream /=
                                   Parser_State.Stream
                                 then
                                    --  To breakdown a shared_stream token, we first have to create a
                                    --  parse stream input element for it, and do the breakdown in the
                                    --  parse stream input.
                                    Tree.Move_Shared_To_Input (Parser_State.Stream);
                                    Current_Token := Shared_Parser.Tree.Current_Token (Parser_State.Stream);
                                 end if;

                                 if Trace_Parse > Detail then
                                    Shared_Parser.Tree.Lexer.Trace.Put_Line
                                      (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                                         ": left_breakdown " &
                                         Tree.Image (Current_Token, First_Terminal => True));
                                 end if;
                                 Tree.Left_Breakdown
                                   (Current_Token, Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));

                                 if Trace_Parse > Extra then
                                    Shared_Parser.Tree.Lexer.Trace.Put_Line
                                      (" ... current_token: " & Tree.Image (Current_Token, First_Terminal => True));
                                    if Trace_Parse > Detail then
                                       Shared_Parser.Tree.Lexer.Trace.Put_Line
                                         (" ... input stream: " & Tree.Image
                                            (Parser_State.Stream, Stack => False, Input => True, Shared => True));
                                    end if;
                                 end if;
                              end;
                              return;
                           end if;

                        when Accept_It | Reduce =>
                           return;

                        when Error =>
                           if Checking_Next then
                              Delete_Empty;

                           elsif Handle_Error then
                              return;
                           end if;
                        end case;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Get_Action;

   procedure Do_Action
     (Action         : in     Parse_Action_Rec;
      Current_Parser : in     Parser_Lists.Cursor;
      Shared_Parser  : in out LR.Parser.Parser)
   --  Apply Action to Current_Parser; sets Current_Parser.Verb.
   is
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Tree.Lexer.Trace.all;
      Status       : Syntax_Trees.In_Parse_Actions.Status_Label;

   begin
      if Trace_Parse > Detail then
         Trace.Put
           --  No prefix, leading space for compatibility with existing tests.
           (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
              (if Trace_Parse_No_State_Numbers
               then "-- : "
               else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
              Shared_Parser.Tree.Image
                (Shared_Parser.Tree.Current_Token (Parser_State.Stream),
                 First_Terminal => True, Terminal_Node_Numbers => True) & " : " &
              Trace_Image (Action, Shared_Parser.Tree.Lexer.Descriptor.all));
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Parser_State.Set_Verb (Shift);
         Parser_State.Last_Action := Action;

         Shared_Parser.Tree.Shift (Parser_State.Stream, Action.State);

      when Reduce =>
         declare
            New_State : constant Unknown_State_Index := Goto_For
              (Table => Shared_Parser.Table.all,
               State => Shared_Parser.Tree.State
                 (Parser_State.Stream, Shared_Parser.Tree.Peek
                    (Parser_State.Stream, SAL.Base_Peek_Type (Action.Token_Count) + 1)),
               ID    => Action.Production.LHS);
         begin
            if New_State = Unknown_State then
               --  This is due to a bug in the LALR parser generator (see
               --  lalr_generator_bug_01.wy); we treat it as a syntax error.
               Parser_State.Set_Verb (Error);
               Parser_State.Last_Action := (Error, Invalid_Production_ID);

               Shared_Parser.Tree.Add_Error_To_Input
                 (Parser_State.Stream,
                  Parse_Error'
                    (First_Terminal => 1,
                     Last_Terminal  => 0,
                     Expecting      => (1 .. 0 => False),
                     Recover_Ops    => Recover_Op_Nodes_Arrays.Empty_Vector,
                     Recover_Test   => null),
                  Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));

               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... error unknown state");
               end if;

            else
               begin
                  Status := Reduce_Stack_1 (Shared_Parser, Current_Parser, Action, New_State);
               exception
               when Partial_Parse =>
                  if Parser_State.Resume_Active or Shared_Parser.Parsers.Count > 1 then
                     --  Wait until there is one parser not in resume.
                     if Trace_Parse > Outline then
                        Trace.Put_Line (" ... partial parse done, waiting for other parsers");
                     end if;
                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line (" ... partial parse done");
                     end if;

                     declare
                        Current_Token : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Current_Token
                          (Parser_State.Stream);
                        Tree_Root : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Stream_Prev (Current_Token);
                     begin
                        if Shared_Parser.Tree.ID (Current_Token.Node) /= Shared_Parser.Tree.Lexer.Descriptor.EOI_ID then
                           --  Insert EOI on Shared_Stream
                           declare
                              Last_Token_Byte_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Byte_Region
                                (Current_Token.Node, Trailing_Non_Grammar => False).Last;
                              Last_Token_Char_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Char_Region
                                (Current_Token.Node, Trailing_Non_Grammar => False).Last;
                              Last_Token_Line_Region_Last : constant Line_Number_Type := Shared_Parser.Tree.Line_Region
                                (Current_Token, Trailing_Non_Grammar => True).Last;

                              EOI_Token : constant Lexer.Token :=
                                (ID          => Shared_Parser.Tree.Lexer.Descriptor.EOI_ID,
                                 Byte_Region =>
                                   (First    => Last_Token_Byte_Region_Last + 1,
                                    Last     => Last_Token_Byte_Region_Last),
                                 Char_Region =>
                                   (First    => Last_Token_Char_Region_Last + 1,
                                    Last     => Last_Token_Char_Region_Last),
                                 Line_Region => (First | Last => Last_Token_Line_Region_Last));
                           begin
                              Shared_Parser.Tree.Insert_Source_Terminal
                                (Shared_Parser.Tree.Shared_Stream,
                                 Terminal => EOI_Token,
                                 Before   => Shared_Parser.Tree.Stream_Next (Current_Token).Element,
                                 Errors   => Syntax_Trees.Null_Error_List);
                           end;
                        end if;

                        if Shared_Parser.Tree.ID (Tree_Root.Node) /= Shared_Parser.Tree.Lexer.Descriptor.Accept_ID then
                           --  Add Accept_ID node.
                           declare
                              Accept_Node : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Reduce
                                (Parser_State.Stream,
                                 Production       =>
                                   (LHS           => Shared_Parser.Tree.Lexer.Descriptor.Accept_ID,
                                    RHS           => 1),
                                 Child_Count      => 1,
                                 State            => Accept_State,
                                 Recover_Conflict => False);
                           begin
                              Shared_Parser.Tree.Set_Root (Accept_Node.Node);
                           end;
                        end if;
                     end;
                     raise;
                  end if;
               end;

               case Status is
               when Ok =>
                  Parser_State.Set_Verb (Reduce);
                  Parser_State.Last_Action := Action;

                  if Trace_Parse > Detail then
                     Trace.Put_Line
                       (" ... goto state " &
                          (if Trace_Parse_No_State_Numbers
                           then "--"
                           else Trimmed_Image (New_State)));
                  end if;

               when Syntax_Trees.In_Parse_Actions.Error =>
                  Parser_State.Set_Verb (Error);
                  Parser_State.Last_Action := Action; -- not Error, since we did a reduce.
                  Parser_State.Error_Count        := @ + 1;
                  Parser_State.Zombie_Token_Count := 1;
               end case;
            end if;
         end;

      when Accept_It =>
         case Reduce_Stack_1
           (Shared_Parser, Current_Parser,
            (Reduce, Action.Production, Action.Token_Count),
            Accept_State)
         is
         when Ok =>
            Parser_State.Set_Verb (Action.Verb);

         when Syntax_Trees.In_Parse_Actions.Error =>
            Parser_State.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Parser_State.Set_Verb (Action.Verb);
         Parser_State.Error_Count := @ + 1;

         Parser_State.Zombie_Token_Count := 1;

         declare
            use WisiToken.Syntax_Trees;
            Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Tree.State (Parser_State.Stream));

            New_Error : constant Parse_Error :=
              (First_Terminal => Tree.Lexer.Descriptor.First_Terminal,
               Last_Terminal  => Tree.Lexer.Descriptor.Last_Terminal,
               Expecting      => Expecting,
               Recover_Ops    => Recover_Op_Nodes_Arrays.Empty_Vector,
               Recover_Test   => null);

         begin
            if Tree.Input_Has_Matching_Error (Parser_State.Stream, New_Error) then
               --  Keep the recover information so it can be used again.
               null;

            else
               Tree.Delete_Errors_In_Input
                 (Parser_State.Stream,
                  Error_Pred_Parse'Access,
                  Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));

               Tree.Add_Error_To_Input
                 (Stream    => Parser_State.Stream,
                  Data      => New_Error,
                  User_Data => Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));

            end if;

            if Trace_Parse > Detail then
               Trace.Put_Line
                 (" " & Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                    (if Trace_Parse_No_State_Numbers
                     then "--"
                     else Trimmed_Image (Tree.State (Parser_State.Stream))) &
                    ": expecting: " & Image (Expecting, Tree.Lexer.Descriptor.all));
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Nodes_Arrays;
      use Syntax_Trees;
      use Parser_Lists;

      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      if Parser_State.Current_Recover_Op = No_Insert_Delete then
         return;
      end if;

      declare
         --  Resume only processes recover_ops in one error; the current error.
         --  If it also fixes a following error, that error has no recover.
         Err          : Error_Data'Class := Error (Parser_State.Current_Error_Ref (Tree));
         Recover_Ops  : Recover_Op_Nodes_Arrays.Vector renames Recover_Op_Array_Var_Ref (Err);
         Err_Modified : Boolean          := False;
      begin
         One_Error :
         loop
            declare
               Op  : Recover_Op_Nodes renames Recover_Ops (Parser_State.Current_Recover_Op);
            begin
               if Op.Op = Delete then
                  declare
                     Deleted_Node : constant Valid_Node_Access := Tree.First_Terminal
                       (Tree.Current_Token (Parser_State.Stream)).Node;
                  begin
                     if Op.Del_Index = Tree.Get_Sequential_Index (Deleted_Node) then
                        Err_Modified := True;

                        Parser_State.Do_Delete
                          (Tree, Op, User_Data_Access_Constant (Shared_Parser.User_Data));

                        if Trace_Parse > Extra  then
                           Tree.Lexer.Trace.Put_Line
                             (" " & Tree.Trimmed_Image (Parser_State.Stream) & ": delete " & Op.Del_Index'Image);
                        end if;

                        Parser_State.Next_Recover_Op (Tree);

                        exit One_Error when Parser_State.Current_Recover_Op = No_Insert_Delete;

                     else
                        exit One_Error;
                     end if;
                  end;
               else
                  exit One_Error;
               end if;
            end;
         end loop One_Error;

         if Err_Modified then
            Parser_State.Update_Error (Tree, Err, User_Data_Access_Constant (Shared_Parser.User_Data));
         end if;
      end;
   end Do_Deletes;

   procedure Parse_Verb
     (Shared_Parser : in out LR.Parser.Parser;
      Verb          :    out All_Parse_Action_Verbs;
      Zombie_Count  :    out SAL.Base_Peek_Type)
   --  Verb: the type of parser cycle to execute;
   --
   --     Accept_It : all Parsers.Verb return Accept - done parsing.
   --
   --     Shift : some Parsers.Verb return Shift.
   --
   --     Pause : Resume is active, and this parser has reached
   --  Resume_Goal, so it is waiting for the others to catch up. Or
   --  resume is not active, and this parser has shifted a nonterminal,
   --  while some other parser has broken down that nonterminal; it is
   --  waiting for the others to catch up. This ensures parsers are
   --  within Mckenzie_Param.Zombie_Limit of the same terminal when they
   --  enter error recovery.
   --
   --     Reduce : some Parsers.Verb return Reduce.
   --
   --     Error : all Parsers.Verb return Error.
   --
   --  Zombie_Count: count of parsers in Error state
   is
      use all type WisiToken.Syntax_Trees.Stream_Node_Ref;

      Shift_Count   : SAL.Base_Peek_Type := 0;
      Accept_Count  : SAL.Base_Peek_Type := 0;
      Resume_Active : Boolean            := False;
      Some_Paused   : Boolean            := False;

      Min_Sequential_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
      Max_Byte_Last        : Buffer_Pos                    := Buffer_Pos'First;
   begin
      Zombie_Count := 0;

      for Parser_State of Shared_Parser.Parsers loop
         --  Parser_State.Verb is set by Do_Action, except Pause, Accept_It are
         --  set here.
         case Parser_State.Verb is
         when Pause | Shift =>
            Shift_Count := Shift_Count + 1;
            Parser_State.Set_Verb (Shift);

            --  We call Do_Deletes here so it can break down a nonterm if needed;
            --  then the check for resume done is correct.
            --  ada_mode-recover_bad_char.adb.
            Do_Deletes (Shared_Parser, Parser_State);

            if Parser_State.Resume_Active then
               --  We want to set Resume_Active False _after_ we shift the goal
               --  token, so we check the stack top. test_incremental.adb
               --  Nonterm_Resume_01.
               declare
                  use WisiToken.Syntax_Trees;

                  function Get_Terminal return Node_Access
                  is
                     Ref : Stream_Node_Parents := Shared_Parser.Tree.To_Stream_Node_Parents
                       (Shared_Parser.Tree.To_Rooted_Ref
                          (Parser_State.Stream, Shared_Parser.Tree.Peek (Parser_State.Stream)));
                  begin
                     Shared_Parser.Tree.Last_Sequential_Terminal (Ref, Parser_State.Stream, Preceding => True);
                     return Ref.Ref.Node;
                  end Get_Terminal;

                  Terminal : constant Node_Access := Get_Terminal;

                  Terminal_Index : constant Base_Sequential_Index := Shared_Parser.Tree.Get_Sequential_Index
                    (Terminal);
               begin
                  if Terminal_Index = Invalid_Sequential_Index
                    --  Most likely we just shifted a nonterm that got past the resume
                    --  goal; ada_mode-interactive_02.adb.
                    or else
                    (Parser_State.Resume_Token_Goal <= Terminal_Index and
                       Parser_State.Current_Recover_Op = No_Insert_Delete)
                       --  Parser_State.Current_Recover_Op can be No_Insert_Delete here
                       --  when Current_Token is a nonterm that needs to be broken down
                       --  before the remaining ops can be performed.
                       --  ada_mode-interactive_01.adb
                  then
                     Parser_State.Resume_Active := False;
                     Parser_State.Resume_Token_Goal := Syntax_Trees.Invalid_Sequential_Index;
                     Parser_State.Clear_Current_Error_Features;
                     if Trace_Parse > Detail then
                        Shared_Parser.Tree.Lexer.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": resume_active: False");
                     end if;
                  else
                     Resume_Active := True;
                  end if;
               end;

            elsif Shared_Parser.Resume_Active then
               declare
                  use Syntax_Trees;
                  First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                    (Shared_Parser.Tree.Shared_Token (Parser_State.Stream).Node);
               begin
                  if First_Terminal /= Invalid_Node_Access then
                     Min_Sequential_Index := Syntax_Trees.Sequential_Index'Min
                       (@, Shared_Parser.Tree.Get_Sequential_Index (First_Terminal));
                  else
                     --  No terminal in Shared_Token
                     null;
                  end if;
               end;

            else
               --  Ensure parsers stay close to the same terminal; see note below at
               --  use of Max_Byte_Last.
               declare
                  use Syntax_Trees;

                  --  We don't just use Byte_Region (stack_top), because that can be
                  --  slow, and we do this every parse cycle.
                  Last_Term : constant Node_Access := Shared_Parser.Tree.Last_Terminal
                    (Shared_Parser.Tree.Get_Node (Parser_State.Stream, Shared_Parser.Tree.Peek (Parser_State.Stream)));
               begin
                  if Last_Term /= Invalid_Node_Access then
                     Max_Byte_Last := Buffer_Pos'Max
                       (@, Shared_Parser.Tree.Byte_Region (Last_Term, Trailing_Non_Grammar => False).Last);
                  end if;
               end;
            end if;

         when Reduce =>
            Verb := Reduce;
            --  No need to review rest of parsers, and Zombie_Count will be
            --  ignored.
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            --  This parser is waiting for others to error; they can continue
            --  parsing.
            Zombie_Count := Zombie_Count + 1;
         end case;
      end loop;

      if Accept_Count > 0 and Shared_Parser.Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

         if Shared_Parser.Resume_Active then
            Shared_Parser.Resume_Active := False;
            McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
         end if;

      elsif Shared_Parser.Parsers.Count = Zombie_Count then
         Verb := Error;
         return;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;

      if Resume_Active then
         Shared_Parser.Resume_Active := True;

         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift and not Parser_State.Resume_Active then
               Parser_State.Set_Verb (Pause);
               if Trace_Parse > Detail then
                  Shared_Parser.Tree.Lexer.Trace.Put_Line
                    (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": pause: resume exit");
               end if;
            end if;
         end loop;

      elsif Shared_Parser.Resume_Active then
         --  Ensure all parsers are on the same terminal before exiting resume.
         --  All error recover insert and delete are done, so all parsers must
         --  see the same terminals.
         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift then
               declare
                  use Syntax_Trees;
                  First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                    (Shared_Parser.Tree.Shared_Token (Parser_State.Stream).Node);
               begin
                  if First_Terminal /= Invalid_Node_Access and then
                    Min_Sequential_Index /= Syntax_Trees.Sequential_Index'Last and then
                    Min_Sequential_Index /= Shared_Parser.Tree.Get_Sequential_Index (First_Terminal)
                  then
                     Some_Paused := True;
                     Parser_State.Set_Verb (Pause);
                     if Trace_Parse > Detail then
                        Shared_Parser.Tree.Lexer.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                             ": pause: resume sync (min index" & Min_Sequential_Index'Image & ")");
                     end if;
                  end if;
               end;
            end if;
         end loop;

         if Shared_Parser.Resume_Active and not Some_Paused then
            Shared_Parser.Resume_Active := False;
            McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
         end if;

      elsif Shared_Parser.Parsers.Count > 1 then
         --  Ensure parsers stay close to the same terminal. In general,
         --  Parser_State.Current_Token.Byte_Region should not be within
         --  another parser stack_top, unless it just included that token in a
         --  reduce. But in incremental parse, one parser can shift a nonterm,
         --  while another parser has broken down that nonterm and is working
         --  thru it one terminal at a time.
         declare
            Not_Paused         : array (1 .. Shared_Parser.Parsers.Count) of Boolean := (others => False);
            Parser_Index       : SAL.Base_Peek_Type                                  := Not_Paused'First;
            Max_Terminal_Count : Integer                                             := 0;
         begin
            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Shift then
                  declare
                     use Syntax_Trees;
                     Current_Token_Node : constant Node_Access := Shared_Parser.Tree.Current_Token
                       (Parser_State.Stream).Node;
                     First_Terminal : constant Node_Access :=
                       (if Current_Token_Node /= Invalid_Node_Access
                        then Shared_Parser.Tree.First_Terminal (Current_Token_Node)
                        else Invalid_Node_Access);
                  begin
                     if First_Terminal /= Invalid_Node_Access then
                        if Shift_Count < Shared_Parser.Parsers.Count then
                           --  Some parsers are zombies; otherwise this count is a waste of time.
                           --  ada_mode-recover_40.adb used to require Max_Terminal_Count (before
                           --  Matching_Begin added 'null;') .
                           Max_Terminal_Count := Integer'Max
                             (@, Shared_Parser.Tree.Count_Terminals (Current_Token_Node));
                        end if;

                        if Shared_Parser.Tree.Label (First_Terminal) = Source_Terminal then
                           declare
                              Region : constant Buffer_Region := Shared_Parser.Tree.Byte_Region
                                (First_Terminal, Trailing_Non_Grammar => False);
                           begin
                              --  Max_Byte_Last is last byte of farthest token on stack top; parsers
                              --  whose Current_Token are within that token are not paused, so they
                              --  can catch up.
                              if Region.First < Max_Byte_Last then
                                 Not_Paused (Parser_Index) := False;
                              end if;
                           end;
                        end if;
                     end if;
                  end;
                  Parser_Index := @ + 1;
               end if;
            end loop;

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Error then
                  Parser_State.Zombie_Token_Count := @ + Max_Terminal_Count;

                  if Trace_Parse > Extra then
                     Shared_Parser.Tree.Lexer.Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": zombie (" &
                          Integer'Image
                            (Shared_Parser.Table.McKenzie_Param.Zombie_Limit - Parser_State.Zombie_Token_Count) &
                          " tokens remaining)");
                  end if;
               end if;
            end loop;

            if (for all P of Not_Paused => P = False) then
               --  All parsers Current_Token are after farthest stack top; none need
               --  to be paused.
               null;
            else
               Parser_Index := Not_Paused'First;
               for Parser_State of Shared_Parser.Parsers loop
                  if Parser_State.Verb = Shift and not Not_Paused (Parser_Index) then
                     Parser_State.Set_Verb (Pause);
                     if Trace_Parse > Detail then
                        Shared_Parser.Tree.Lexer.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": pause: main sync");
                     end if;
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Parse_Verb;

   procedure Recover_To_Log
     (Shared_Parser            : in out LR.Parser.Parser;
      Recover_Log_File         : in     Ada.Text_IO.File_Type;
      Recover_Result           : in     McKenzie_Recover.Recover_Status;
      Pre_Recover_Parser_Count : in     SAL.Base_Peek_Type)
   is
      use Ada.Text_IO;
   begin
      Put
        (Recover_Log_File,
         Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " " &
           Shared_Parser.Partial_Parse_Active'Image & " " &
           Recover_Result'Image & " " &
           Pre_Recover_Parser_Count'Image & " '" &
           Shared_Parser.Tree.Lexer.File_Name & "'");

      for Parser of Shared_Parser.Parsers loop
         Put (Recover_Log_File, '(');
         if Parser.Recover.Results.Count > 0 then
            --  Count can be 0 when error recovery fails
            Put (Recover_Log_File, Image (Parser.Recover.Results.Peek.Strategy_Counts));
         end if;
         Put
           (Recover_Log_File,
            Integer'Image (Parser.Recover.Enqueue_Count) &
              Integer'Image (Parser.Recover.Check_Count) & " " &
              Boolean'Image (Parser.Recover.Success));
         Put (Recover_Log_File, ')');
      end loop;

      New_Line (Recover_Log_File);
      Flush (Recover_Log_File);
   exception
   when others =>
      New_Line (Recover_Log_File);
      Flush (Recover_Log_File);
   end Recover_To_Log;

   procedure Check_Error
     (Shared_Parser : in out LR.Parser.Parser;
      Check_Parser  : in out Parser_Lists.Cursor)
   is
      procedure Report_Error
      is
         --  This is actually a bug in error recovery, not a source syntax error.
         Msg : constant String := Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) &
              ": error during resume";
      begin
         if Debug_Mode then
            raise SAL.Programmer_Error with Msg;
         else
            raise WisiToken.Parse_Error with Msg;
         end if;
      end Report_Error;

   begin
      if Check_Parser.Verb = Error then
         --  This parser errored on last input. This is how grammar conflicts
         --  are resolved when the input text is valid, in which case we should
         --  just terminate this parser. However, this may be due to invalid
         --  input text, so we keep the parser alive but suspended for a few
         --  tokens, to see if the other parsers also error, in which case they
         --  all participate in error recovery.

         --  We do not create zombie parsers during resume.
         if not Check_Parser.State_Ref.Resume_Active then
            --  Parser is now a zombie
            if Trace_Parse > Detail then
               Shared_Parser.Tree.Lexer.Trace.Put_Line
                 (" " & Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) & ": zombie");
            end if;
            Check_Parser.Next;

         else
            if Shared_Parser.Parsers.Count = 1 then
               Report_Error;

            else
               --  This is ok if a conflict occured during resume - we assume this is
               --  a branch that failed during recover as well. Otherwise it's a
               --  programmer error.
               if Check_Parser.State_Ref.Conflict_During_Resume then
                  Shared_Parser.Parsers.Terminate_Parser
                    (Check_Parser, Shared_Parser.Tree, "error in conflict during resume",
                     Shared_Parser.Tree.Lexer.Trace.all);
               else
                  Report_Error;
               end if;
            end if;
         end if;
      else
         Check_Parser.Next;
      end if;
   end Check_Error;

   procedure Finish_Parse
     (Parser            : in out LR.Parser.Parser;
      Incremental_Parse : in     Boolean)
   with Pre => Parser.Parsers.Count = 1
   --  Final actions after LR accept state reached; call
   --  User_Data.Insert_Token, Delete_Token.
   is
      use WisiToken.Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;

      Last_Deleted_Node_Parent : Node_Access;
   begin
      Parser.Tree.Set_Root (Get_Node (Parser.Tree.Peek (Parser.Tree.First_Parse_Stream)));

      --  We need parents set in the following code.
      Parser.Tree.Finish_Parse;
      Parser_State.Clear_Stream;

      if Debug_Mode then
         declare
            I : Integer := 1;
         begin
            for Node of Parser_State.Recover_Insert_Delete loop
               if not Parser.Tree.In_Tree (Node) then
                  raise SAL.Programmer_Error with "recover_insert_delete node" & I'Image & " not in tree";
               end if;
               I := @ + 1;
            end loop;
         end;
      end if;

      if Trace_Parse > Extra and then Parser_State.Recover_Insert_Delete.Length > 0 then
         Parser.Tree.Lexer.Trace.New_Line;
         Parser.Tree.Lexer.Trace.Put_Line ("before insert/delete tree:");
         Parser.Tree.Print_Tree
           (Non_Grammar  => True,
            Line_Numbers => True);
         Parser.Tree.Lexer.Trace.Put_Line
           ("recover_insert_delete: " & Parser_Lists.Recover_Image (Parser_State, Parser.Tree));
         Parser.Tree.Lexer.Trace.New_Line;
      end if;

      --  ada-mode-recover_33.adb requires calling Insert_Token,
      --  Delete_Token in lexical order, which is Recover_Insert_Delete
      --  order. Other use cases would benefit from calling all Delete
      --  first, or all Insert first, but we use this order as the least
      --  surprising.
      for Node of Parser_State.Recover_Insert_Delete loop
         for Err of Parser.Tree.Error_List (Node) loop
            if not (Err in Lexer_Error) then
               for Op of Recover_Op_Array_Const_Ref (Err) loop
                  case Op.Op is
                  when Insert =>
                     if Parser.User_Data /= null then
                        Parser.User_Data.Insert_Token (Parser.Tree, Op.Ins_Node);
                     end if;
                  when Delete =>
                     --  Op.Del_Node.Non_Grammar were previously moved to
                     --  Op.Del_Node.Parent in Syntax_Tree.Add_Deleted; now we can edit the
                     --  shared stream, so we can clear them.
                     Parser.Tree.Non_Grammar_Var (Op.Del_Node).Clear;

                     if Parser.Tree.Parent (Op.Del_Node) /= Last_Deleted_Node_Parent then
                        Last_Deleted_Node_Parent := Parser.Tree.Parent (Op.Del_Node);
                        if Parser.User_Data /= null then
                           Parser.User_Data.Delete_Tokens (Parser.Tree, Last_Deleted_Node_Parent);
                        end if;
                     end if;
                  end case;
               end loop;
            end if;
         end loop;
      end loop;

      if Trace_Parse > Extra or Trace_Action > Detail then
         Parser.Tree.Lexer.Trace.Put_Line ("post-parse tree:");
         Parser.Tree.Lexer.Trace.Put_Line
           (Parser.Tree.Image
              (Children     => True,
               Non_Grammar  => True,
               Augmented    => True,
               Line_Numbers => True));
         Parser.Tree.Lexer.Trace.New_Line;
      end if;

      if Trace_Memory > Detail then
         Parser.Tree.Lexer.Trace.Put_Line ("parse complete");
         --  IMPROVEME: we want Prefix True when running as an Emacs
         --  subprocess, False otherwise. No way to tell from here.
         Report_Memory (Parser.Tree.Lexer.Trace.all, Prefix => True);
      end if;

      if Debug_Mode then
         declare
            Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
         begin
            if Parser.User_Data = null then
               declare
                  Dummy : User_Data_Type;
               begin
                  Parser.Tree.Validate_Tree
                    (Dummy, Error_Reported,
                     Node_Index_Order => not Incremental_Parse,
                     Validate_Node    => Syntax_Trees.Mark_In_Tree'Access);
               end;
               Parser.Tree.Clear_Augmented;
            else
               Parser.Tree.Validate_Tree
                 (Parser.User_Data.all, Error_Reported, Node_Index_Order => not Incremental_Parse);
            end if;

            if Error_Reported.Count /= 0 then
               raise WisiToken.Validate_Error with "parser: validate_tree failed";
            end if;
         end;
      end if;
   end Finish_Parse;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Finalize (Object : in out Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser                         :    out LR.Parser.Parser;
      Lexer                          : in     WisiToken.Lexer.Handle;
      Table                          : in     Parse_Table_Ptr;
      Productions                    : in     Syntax_Trees.Production_Info_Trees.Vector;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access;
      User_Data                      : in     Syntax_Trees.User_Data_Access)
   is begin
      Parser.Tree.Lexer  := Lexer;
      Parser.Productions := Productions;
      Parser.User_Data   := User_Data;

      --  In Base_Parser; Tree, Line_Begin_Token, Last_Grammar_Node are default initialized.

      Parser.Table                          := Table;
      Parser.Language_Fixes                 := Language_Fixes;
      Parser.Language_Matching_Begin_Tokens := Language_Matching_Begin_Tokens;
      Parser.Language_String_ID_Set         := Language_String_ID_Set;

      --  In Parser; String_Quote_Checked, Post_Recover, Parsers are default
      --  initialized. Partial_Parse_Active is set by user after this.
   end New_Parser;

   procedure Edit_Tree
     (Parser : in out LR.Parser.Parser;
      Edits  : in     KMN_Lists.List)
   is
      --  Similar to [Lahav 2004] Algorithms 3, 4. That assumes creating a
      --  separate temp list of new tokens, and then merging that into the
      --  parse tree, is faster than merging new tokens in one by one; we
      --  just do the latter. We also don't modify the edit list.
      --
      --  Parser.Lexer contains the edited text; the initial text is not
      --  available.
      use WisiToken.Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      use all type KMN_Lists.Cursor;

      Tree : Syntax_Trees.Tree renames Parser.Tree;

      KMN_Node         : KMN_Lists.Cursor := Edits.First;
      Old_Byte_Pos     : Base_Buffer_Pos  := 0;
      Old_Char_Pos     : Base_Buffer_Pos  := 0;
      New_Byte_Pos     : Base_Buffer_Pos  := 0;
      New_Char_Pos     : Base_Buffer_Pos  := 0;

      Scanned_Byte_Pos : Base_Buffer_Pos  := 0;
      Scanned_Char_Pos : Base_Buffer_Pos  := 0;
      --  End of last token saved after being scanned by the lexer; this
      --  does not include trailing whitespace, so it is not actually the
      --  last position scanned by the lexer. Note that the lexer has
      --  effectively scanned the deleted bytes in the current KMN, so when
      --  comparing unshifted token positions to Scanned_Byte_Pos, we may
      --  need to add KMN.Deleted_Bytes.

      Shift_Bytes      : Base_Buffer_Pos  := 0;
      Shift_Chars      : Base_Buffer_Pos  := 0;

      Shift_Lines : Base_Line_Number_Type := 0;
      --  Whenever a non_grammar is deleted from Tree (either permanently,
      --  or moved to Floating_Non_Grammar), Shift_Lines is decremented by
      --  New_Line_Count (non_grammar_token). Then if a token is restored
      --  from Floating_Non_Grammar to the Tree, Shift_Lines is incremented;
      --  if a token is deleted from Floating_Non_Grammar, Shift_Lines is
      --  not changed. See Deleted_Shift_Lines in KMN_Loop for additional
      --  rules.

      Floating_Non_Grammar : Lexer.Token_Arrays.Vector;
      --  Non_grammar that are detached from a node (for various reasons).
      --  These are not shifted, because the correct shift is
      --  unknown at the time they are detached. ada_mode-recover_42.adb.
      --
      --  If a non_grammar is floated from a scanned node, it is unshifted
      --  to be consistent.

      Delayed_Scan           : Boolean             := False;
      Delayed_Floating_Index : Positive_Index_Type := Positive_Index_Type'Last;
      Delayed_Lex_Start_Byte : Buffer_Pos          := Buffer_Pos'Last;
      Delayed_Lex_Start_Char : Buffer_Pos          := Buffer_Pos'Last;
      Delayed_Lex_Start_Line : Line_Number_Type    := Line_Number_Type'Last;
      --  When multiple edits occur in a token, the last one may insert or
      --  delete an end delimiter, so it is not possible to compute the
      --  correct scan end when handling the first edit. So the scan is
      --  delayed. If Delayed_Floating_Index /= 'Last, the token is a
      --  non_grammar. We also get delayed scans when inserting/deleting a
      --  block delimeter sets Scan_End past the next KMN.

      Scan_End : Base_Buffer_Pos := Invalid_Buffer_Pos;
      --  If Scan_End /= Invalid_Buffer_Pos, an edit exposed text as
      --  code; a comment or string end was inserted, or a comment or string
      --  start was deleted. Scan all exposed code thru Scan_End (which
      --  is shifted).
      --
      --  If the start and end delimiters of the block token are different,
      --  we don't need to check for start delimiter inserted or end
      --  delimiter deleted; the edited token is scanned, and all the tokens
      --  that the new token covers will be deleted because they are in the
      --  region scanned.
      --
      --  However, if the start and end delimiters are the same (as for
      --  strings), then deleting either delimiter requires all text thru
      --  new-line or EOI be scanned.

      type Lexer_Error_Data is record
         Node : Node_Access;
         --  The node containing the error.

         Scan_End : Base_Buffer_Pos := Invalid_Buffer_Pos;
         --  If Node is scanned and this is not invalid, the scan must continue
         --  until this position is scanned. Shifted.

         Scan_Start_Node : Node_Access := Invalid_Node_Access;
         --  If Node is scanned and this is not invalid, the scan must start at
         --  this node.

         Edit_Region : Buffer_Region := Null_Buffer_Region;
         --  Null_Buffer_Region if Node.ID is not block delimited.
         --  Otherwise, if a matching delimiter is inserted or deleted in this
         --  region, Node must be scanned. Unshifted.
      end record;

      package Lexer_Error_Data_Lists is new Ada.Containers.Doubly_Linked_Lists (Lexer_Error_Data);

      Lexer_Errors : Lexer_Error_Data_Lists.List;
      --  This list records the scan region for lexer errors, depending on
      --  where they occur. Then if an edit might affect the lexer error,
      --  the scan for the edit covers that region. That lets an edit fix
      --  the lexer error. test_incremental.adb Edit_String_06,
      --  Lexer_Errors_04.

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;
      --  Node being considered for shift or delete. Terminals before
      --  Terminal are shifted; Terminal and terminals after it are
      --  unshifted.

      Terminal_Non_Grammar_Next : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
      --  Next non_grammar in Terminal to be shifted or deleted.

      Terminal_Shifted : Boolean := False;
      --  On exit from Unchanged_Loop, Terminal is not shifted unless
      --  Terminal_Non_Grammar_Next is not No_Index. Terminal_Shifted is
      --  used in Delete_Scanned_Loop to account for this.

      procedure Maybe_Delete_Lexer_Errors (Node : in Valid_Node_Access)
      is begin
         if Invalid_Error_Ref /= Tree.Has_Error_Class (Node, Lexer_Error'(others => <>)) then
            --  Delete from Lexer_Errors. test_incremental.adb Edit_String_09
            declare
               use Lexer_Error_Data_Lists;
               Cur : Cursor := Lexer_Errors.First;
            begin
               loop
                  exit when Cur = No_Element;

                  if Lexer_Errors (Cur).Node = Node then
                     declare
                        To_Delete_1 : Cursor := Cur;
                     begin
                        Next (Cur);
                        Lexer_Errors.Delete (To_Delete_1);
                     end;
                  end if;
                  Next (Cur);
               end loop;
            end;
         end if;
      end Maybe_Delete_Lexer_Errors;

      procedure Breakdown (Terminal : in out Terminal_Ref; To_Single : in Boolean := False)
      with Pre => Terminal /= Invalid_Stream_Node_Ref
      is begin
         if Tree.Label (Terminal.Element) = Nonterm then
            if Trace_Incremental_Parse > Detail then
               Tree.Lexer.Trace.Put_Line
                 ("breakdown " & (if To_Single then "single " else "") & Tree.Image
                    (Tree.Get_Node (Terminal.Stream, Terminal.Element), Node_Numbers => True) &
                    " target " & Tree.Image (Terminal.Node, Node_Numbers => True));
               if Trace_Incremental_Parse > Extra + 1 then
                  Tree.Lexer.Trace.Put_Line ("... before:");
                  Tree.Lexer.Trace.Put_Line (Tree.Image (Stream, Children => True));
                  Tree.Lexer.Trace.New_Line;
               end if;
            end if;
            Tree.Breakdown
              (Terminal, Parser.Productions,
               Syntax_Trees.User_Data_Access_Constant (Parser.User_Data), First_Terminal => True);

            if To_Single and then Tree.Label (Terminal.Element) = Nonterm then
               Tree.Left_Breakdown (Terminal, Syntax_Trees.User_Data_Access_Constant (Parser.User_Data));
            end if;
            if Trace_Incremental_Parse > Extra then
               Tree.Lexer.Trace.Put_Line
                 ("... result " & Tree.Image (Stream, Children => Trace_Incremental_Parse > Extra + 1));
            end if;
         end if;
      end Breakdown;

   begin
      Tree.Start_Edit;

      if Edits.Length = 0 then
         return;
      end if;

      Stream := Tree.Shared_Stream;

      --  Breakdown Recover_Conflict nonterms. Nonterms are marked
      --  Recover_Conflict when the initial parse resolves a conflict during
      --  error recovery; a subseuent edit may require a different conflict
      --  resolution. We breakdown all the way to terminals, because some of
      --  the children may be nonterms that were created before the error
      --  was detected, but need to change. test_incremental.adb
      --  Undo_Conflict_01.
      --
      --  We must do this before Undo_Recover, because that might breakdown
      --  a Recover_Conflict node.
      declare
         Ref          : Stream_Node_Ref := Tree.First_Recover_Conflict;
         To_Breakdown : Stream_Node_Ref;
      begin
         Breakdown_Recover_Conflict :
         loop
            exit Breakdown_Recover_Conflict when Ref = Invalid_Stream_Node_Ref;

            if Tree.Label (Ref.Node) = Nonterm and then Tree.Is_Empty_Nonterm (Ref.Node) then
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("delete empty recover_conflict node " & Tree.Image (Ref.Node, Node_Numbers => True));
               end if;

               if Get_Node (Ref.Element) /= Ref.Node then
                  Tree.Breakdown
                    (Ref, Parser.Productions,
                     Syntax_Trees.User_Data_Access_Constant (Parser.User_Data), First_Terminal => False);
               end if;

               declare
                  To_Delete : Stream_Index := Ref.Element;
               begin
                  Tree.Stream_Next (Ref, Rooted => True);
                  Tree.Stream_Delete (Stream, To_Delete);
               end;

            else
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("breakdown recover_conflict node " & Tree.Image (Ref, Node_Numbers => True));
               end if;

               Tree.Breakdown
                 (Ref, Parser.Productions,
                  Syntax_Trees.User_Data_Access_Constant (Parser.User_Data),
                  First_Terminal => False);
               To_Breakdown := Ref;
               Tree.First_Terminal (To_Breakdown);
               Tree.Stream_Next (Ref, Rooted => True);

               To_Terminals :
               loop
                  if Trace_Incremental_Parse > Extra then
                     Tree.Lexer.Trace.Put_Line
                       ("... to_breakdown " & Tree.Image (To_Breakdown, Node_Numbers => True));
                  end if;

                  if Tree.Label (To_Breakdown.Element) = Nonterm then
                     if Tree.Is_Empty_Nonterm (Tree.Get_Node (Stream, To_Breakdown.Element)) then
                        declare
                           To_Delete : Stream_Index := To_Breakdown.Element;
                        begin
                           Tree.Stream_Next (To_Breakdown, Rooted => False);
                           Tree.Stream_Delete (Stream, To_Delete);
                        end;
                     else
                        Tree.Left_Breakdown (To_Breakdown, Syntax_Trees.User_Data_Access_Constant (Parser.User_Data));
                        Tree.Stream_Next (To_Breakdown, Rooted => False);
                     end if;
                     if Trace_Incremental_Parse > Extra then
                        Tree.Lexer.Trace.Put_Line
                          ("... stream " & Tree.Image (Stream, Node_Numbers => True));
                     end if;

                  else
                     Tree.Stream_Next (To_Breakdown, Rooted => False);
                  end if;
                  exit To_Terminals when To_Breakdown.Element = Ref.Element;
               end loop To_Terminals;

               To_Breakdown := Invalid_Stream_Node_Ref;
            end if;

            Tree.First_Recover_Conflict (Ref);
         end loop Breakdown_Recover_Conflict;
      end;

      --  Undo all error recover insert/delete, in case this is needed as
      --  part of an edit in another place; test_incremental.adb
      --  Preserve_Parse_Errors_2.
      --
      --  IMPROVEME incremental: This algorithm visits every terminal; not
      --  incremental. Cache Has_Following_Deleted, has_virtual in nonterms.

      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));
      Undo_Recover :
      loop
         Next_Recover :
         loop
            exit Undo_Recover when Terminal.Node = Invalid_Node_Access;
            exit Next_Recover when Tree.Label (Terminal.Node) in Virtual_Terminal;
            exit Next_Recover when Tree.Label (Terminal.Node) = Source_Terminal and then
              Tree.Has_Following_Deleted (Terminal.Node);
            Tree.Next_Terminal (Terminal);
         end loop Next_Recover;

         case Terminal_Label'(Tree.Label (Terminal.Node)) is
         when Source_Terminal =>
            declare
               Has_Deleted   : constant Valid_Node_Access := Terminal.Node;
               Insert_Before : Terminal_Ref;
            begin
               Tree.Next_Terminal (Terminal);

               Breakdown (Terminal);
               Insert_Before := Terminal;

               for Deleted_Node of reverse Tree.Following_Deleted (Has_Deleted) loop
                  if Tree.Label (Deleted_Node) in Virtual_Terminal_Label then
                     --  This would be deleted in the next step, so don't bother restoring
                     --  it.
                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("drop virtual deleted node " & Tree.Image (Deleted_Node, Node_Numbers => True));
                     end if;

                  else
                     declare
                        Deleted_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                          (Deleted_Node, Trailing_Non_Grammar => False);

                        Has_Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var
                          (Has_Deleted);
                        First_To_Move : Positive_Index_Type := Positive_Index_Type'Last;
                        Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Deleted_Node);
                        pragma Assert (Deleted_Non_Grammar.Length = 0);
                     begin
                        if Has_Deleted_Non_Grammar.Length > 0 and then
                          Deleted_Byte_Region.First < Has_Deleted_Non_Grammar
                            (Has_Deleted_Non_Grammar.Last_Index).Byte_Region.Last
                        then
                           --  Move some Non_Grammar to Deleted_Non_Grammar
                           --  test_incremental.adb Modify_Deleted_Element, Lexer_Errors_01,
                           --  Restore_Deleted_01
                           for I in Has_Deleted_Non_Grammar.First_Index .. Has_Deleted_Non_Grammar.Last_Index loop
                              if Deleted_Byte_Region.First < Has_Deleted_Non_Grammar (I).Byte_Region.Last then
                                 First_To_Move := I;
                                 exit;
                              end if;
                           end loop;
                           for I in First_To_Move .. Has_Deleted_Non_Grammar.Last_Index loop
                              Deleted_Non_Grammar.Append (Has_Deleted_Non_Grammar (I));
                           end loop;
                           if First_To_Move = Has_Deleted_Non_Grammar.First_Index then
                              Has_Deleted_Non_Grammar.Clear;
                           else
                              Has_Deleted_Non_Grammar.Set_First_Last
                                (Has_Deleted_Non_Grammar.First_Index, First_To_Move - 1);
                           end if;
                        end if;
                     end;

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("restore deleted node " & Tree.Image
                             (Deleted_Node, Node_Numbers => True, Non_Grammar => True) &
                             " before " & Tree.Image
                               (Insert_Before, Node_Numbers => True, Non_Grammar => True));
                     end if;

                     Tree.Set_Sequential_Index (Deleted_Node, Invalid_Sequential_Index);
                     Insert_Before := Tree.Stream_Insert (Stream, Deleted_Node, Insert_Before.Element);
                  end if;
               end loop;
               Tree.Following_Deleted (Has_Deleted).Clear;

               if Trace_Incremental_Parse > Extra then
                  Tree.Lexer.Trace.Put_Line
                    ("stream:" & Tree.Image
                       (Stream,
                        Children    => Trace_Incremental_Parse > Detail,
                        Non_Grammar => True,
                        Augmented   => True));
               end if;
            end;

         when Virtual_Terminal_Label =>
            --  Delete Terminal.
            Breakdown (Terminal, To_Single => True);

            declare
               Terminal_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
               To_Delete            : Stream_Index := Terminal.Element;
               Next_Element         : Stream_Index := Tree.Stream_Next (Terminal.Stream, Terminal.Element);
            begin
               if Terminal_Non_Grammar.Length > 0 then
                  declare
                     Term_Non_Gramm_Region : constant Buffer_Region :=
                       (First => Terminal_Non_Grammar (Terminal_Non_Grammar.First_Index).Byte_Region.First,
                        Last  => Terminal_Non_Grammar (Terminal_Non_Grammar.Last_Index).Byte_Region.Last);

                     Next_Terminal : Stream_Node_Ref := Tree.Next_Terminal (Terminal);
                  begin
                     --  Terminal_Non_Grammar is non-empty only if User_Data.Insert_Token
                     --  moved some non_grammar to it. If the terminal they were moved from
                     --  was subsequently deleted and restored, it may now be
                     --  Next_Terminal: ada_mode-interactive_09.adb new_line after 'for'.
                     --  Or it may be before a previous terminal; ada_mode-recover_09.adb.
                     --
                     --  Find a terminal to move Terminal_Non_Grammar to.
                     if Next_Terminal = Invalid_Stream_Node_Ref then
                        --  ada_mode-recover_partial_26.adb
                        Next_Terminal := Terminal;
                     end if;
                     loop
                        exit when Tree.Byte_Region (Next_Terminal.Node, Trailing_Non_Grammar => False).First <
                          Term_Non_Gramm_Region.First;
                        exit when Tree.ID (Next_Terminal.Node) = Tree.Lexer.Descriptor.SOI_ID;
                        Tree.Prev_Terminal (Next_Terminal);
                     end loop;
                     Tree.Non_Grammar_Var (Next_Terminal.Node).Append (Terminal_Non_Grammar);
                     Terminal_Non_Grammar.Clear;
                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("move non_grammar to " & Tree.Image
                             (Next_Terminal.Node, Node_Numbers => True, Non_Grammar => True));
                     end if;
                  end;
               end if;

               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("delete virtual " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
               end if;

               Tree.Next_Terminal (Terminal);
               Tree.Stream_Delete (Terminal.Stream, To_Delete);


               --  Delete immediately following empty nonterms. For example, in Ada,
               --  error recover often inserts 'end <name_opt> ;', where name_opt is
               --  empty; delete all three tokens. On the other hand, an empty
               --  nonterm could be a block name; it will be recreated by the parser,
               --  not treated as an error.
               loop
                  exit when Next_Element = Invalid_Stream_Index;
                  declare
                     Node : constant Valid_Node_Access := Tree.Get_Node (Terminal.Stream, Next_Element);
                     To_Delete : Stream_Index;
                  begin
                     if Tree.Label (Node) = Nonterm and then Tree.Child_Count (Node) = 0 then
                        To_Delete := Next_Element;

                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
                             ("delete empty nonterm " & Tree.Image
                                (To_Delete, Node_Numbers => True, Non_Grammar => True));
                        end if;

                        Next_Element := Tree.Stream_Next (Terminal.Stream, @);
                        Tree.Stream_Delete (Terminal.Stream, To_Delete);
                     else
                        exit;
                     end if;
                  end;
               end loop;
            end;
         end case;
      end loop Undo_Recover;

      --  Delete parse error nodes, record lexer error nodes. Any parse
      --  errors remaining after the edit is applied will be recreated
      --  during parse. We don't delete lexer errors, because they will not
      --  be recreated by parsing; they will be deleted if they are fixed by
      --  an edit, because the tokens containing them will be rescanned.
      --
      --  It is tempting to try to retain parse errors so the previously
      --  found solution can be reapplied, but then it is difficult to
      --  decide which errors to delete during parse; test_incremental.adb
      --  Edit_String_05.
      declare
         Err_Ref : Stream_Error_Ref := Tree.First_Error (Stream);
      begin
         loop
            exit when not Has_Error (Err_Ref);
            declare
               Err       : constant Error_Data'Class := Error (Err_Ref);
               Error_Ref : constant Stream_Node_Ref  := Tree.Error_Stream_Node_Ref (Err_Ref);

               function Find_Edit_Region return Buffer_Region
               is
                  ID : constant Token_ID := Tree.ID (Error_Ref.Node);
               begin
                  if not Tree.Lexer.Is_Block_Delimited (ID) then
                     return Null_Buffer_Region;
                  end if;

                  if Tree.Lexer.Same_Block_Delimiters (ID) then
                     if Tree.Lexer.New_Line_Is_End_Delimiter (ID) then
                        --  A string-double-one-line or similar
                        return Tree.Byte_Region_Of_Line_Region (Error_Ref);
                     else
                        --  Any inserted or deleted matching delimeter affects all text from
                        --  the edit point to the end of text.
                        return
                          (Tree.Byte_Region (Tree.SOI, Trailing_Non_Grammar => True).First,
                           Tree.Byte_Region (Tree.EOI, Trailing_Non_Grammar => True).Last);
                     end if;
                  else
                     raise SAL.Not_Implemented with "FIXME: need test cases.";
                     return Null_Buffer_Region;
                  end if;
               end Find_Edit_Region;

            begin
               if Err in Lexer_Error then
                  --  We don't know Shift_Bytes yet, so we can't find Scan_Start_Node or Scan_End.
                  Lexer_Errors.Append
                    ((Error_Ref.Node,
                      Edit_Region => Find_Edit_Region,
                      others => <>));
                  Tree.Next_Error (Err_Ref);
               else
                  if Trace_Incremental_Parse > Detail then
                     Tree.Lexer.Trace.Put_Line ("delete " & Err.Image (Tree, Error_Ref.Node));
                  end if;
                  Tree.Delete_Error (Err_Ref);
               end if;
            end;
         end loop;
      end;

      --  Now process source edits. We have to start with SOI to handle
      --  edits in leading non-grammar. test_incremental.adb Edit_Comment_12
      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));

      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Edits (KMN_Node);

            Stable_Region : constant Buffer_Region := -- Not shifted
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Stable_Region_Chars : constant Buffer_Region :=
              (Old_Char_Pos + 1, Old_Char_Pos + KMN.Stable_Chars);

            Deleted_Region : constant Buffer_Region := -- Not shifted.
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);
            --  Inserted_Region.First is the first char after the stable region in
            --  the edited text (which means it is shifted).
            --
            --  If Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
            --  then this is the final stable region

            Inserted_Region_Chars : constant Buffer_Region :=
              (New_Char_Pos + KMN.Stable_Chars + 1, New_Char_Pos + KMN.Stable_Chars + KMN.Inserted_Chars);

            Next_KMN : constant WisiToken.Parse.KMN :=
              (if KMN_Node = Edits.Last
               then Invalid_KMN
               else Edits (KMN_Lists.Next (KMN_Node)));

            Next_KMN_Stable_First : constant Buffer_Pos := Stable_Region.Last + KMN.Deleted_Bytes + 1;
            Next_KMN_Stable_Last  : constant Buffer_Pos := Next_KMN_Stable_First - 1 + Next_KMN.Stable_Bytes;

            Deleted_Shift_Lines : Base_Line_Number_Type := 0;
            --  When a non_grammar is deleted by Delete_Deleted_Loop below, if
            --  Terminal remains before the deleted non_grammar
            --  Deleted_Shift_Lines is incremented instead of Shift_Lines. Then
            --  Shift_Lines is correct when computing a scan start point. After
            --  the scan, Deleted_Shift_Lines is added to Shift_Lines.

         begin
            --  Parser.Lexer contains the edited text, so we can't check that
            --  stable, deleted are inside the initial text. Caller should use
            --  Validate_KMN.

            if Trace_Incremental_Parse > Detail then
               Tree.Lexer.Trace.New_Line;
               Tree.Lexer.Trace.Put_Line
                 ("KMN: " & Image (Stable_Region) & Image (Inserted_Region) & Image (Deleted_Region));
               Tree.Lexer.Trace.Put_Line ("old  :" & Old_Byte_Pos'Image & Old_Char_Pos'Image);
               Tree.Lexer.Trace.Put_Line
                 ("shift:" & Shift_Bytes'Image & " " & Shift_Chars'Image & " " & Shift_Lines'Image);
               Tree.Lexer.Trace.Put_Line ("scanned_byte_pos:" & Scanned_Byte_Pos'Image);
               Tree.Lexer.Trace.Put_Line
                 ("stream:" & Tree.Image
                    (Stream,
                     Children    => True,
                     Non_Grammar => True,
                     Augmented   => True));

               Tree.Lexer.Trace.Put_Line
                 ("terminal: " & Tree.Image (Terminal, Non_Grammar => True, Node_Numbers => True));
               if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                  Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
               end if;

               if Floating_Non_Grammar.Length > 0 then
                  Tree.Lexer.Trace.Put_Line
                    ("floating_non_grammar: " & Lexer.Full_Image (Floating_Non_Grammar, Tree.Lexer.Descriptor.all));
                  if Delayed_Floating_Index /= Positive_Index_Type'Last then
                     Tree.Lexer.Trace.Put_Line ("delayed_floating_index:" & Delayed_Floating_Index'Image);
                  end if;
               end if;

               Tree.Lexer.Trace.New_Line;
            end if;

            if not Contains (Outer => Parser.Tree.Lexer.Buffer_Region_Byte, Inner => Inserted_Region) then
               raise User_Error with "KMN insert region " & Image (Inserted_Region) & " outside edited source text " &
                 Image (Parser.Tree.Lexer.Buffer_Region_Byte);
            end if;

            --  Decide which Lexer_Errors are possibly affected by this KMN
            declare
               use Lexer_Error_Data_Lists;
               Cur : Cursor := Lexer_Errors.First;
            begin
               loop
                  exit when Cur = No_Element;

                  declare
                     Node    : constant Valid_Node_Access := Lexer_Errors (Cur).Node;
                     Node_ID : constant Token_ID          := Tree.ID (Node);
                  begin
                     if KMN.Inserted_Bytes = 0 and KMN.Deleted_Bytes = 0 then
                        --  last KMN
                        null;

                     elsif Tree.Lexer.Is_Block_Delimited (Node_ID) then
                        --  test_incremental.adb Edit_String_09, _13, _14, Lexer_Errors_03.
                        declare
                           Edit_Region      : constant Buffer_Region := Lexer_Errors (Cur).Edit_Region;
                           Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Node, Trailing_Non_Grammar => False);
                        begin
                           if Edit_Region.First > Next_KMN_Stable_First then
                              --  This error may be affected by a following KMN; no following lexer
                              --  error can be affected by this KMN.
                              exit;

                           elsif KMN.Inserted_Bytes > 0 then
                              if Tree.Lexer.Same_Block_Delimiters (Node_ID) then
                                 if Invalid_Buffer_Pos /= Tree.Lexer.Contains_End_Delimiter (Node_ID, Inserted_Region)
                                   and then
                                   Contains (Edit_Region + Shift_Bytes, Inserted_Region.First)
                                 then
                                    --  This error is affected by this edit.
                                    declare
                                       Prev_Terminal : constant Stream_Node_Ref :=
                                         Tree.Prev_Terminal (Tree.To_Stream_Node_Ref (Stream, Node));
                                       Data : Lexer_Error_Data renames Lexer_Errors (Cur);
                                    begin
                                       Data.Scan_End := Tree.Lexer.Find_Scan_End
                                         (Tree.ID (Node), Node_Byte_Region + Shift_Bytes +
                                            (if Node_Byte_Region.First > Stable_Region.Last
                                             then 0
                                             else KMN.Inserted_Bytes),
                                          Inserted  => True,
                                          Start     => True);

                                       if Tree.ID (Prev_Terminal.Node) = Tree.ID (Node) and then
                                         Tree.Byte_Region (Prev_Terminal, Trailing_Non_Grammar => False).Last + 1 =
                                         Node_Byte_Region.First and then
                                         Tree.Lexer.Escape_Delimiter_Doubled (Node_ID)
                                       then
                                          --  Prev, Node look like:
                                          --
                                          --   "foo""bar"
                                          --
                                          --  Need to scan both. test_incremental.adb Edit_String_12
                                          Data.Scan_Start_Node := Prev_Terminal.Node;
                                       end if;
                                    end;
                                 end if;
                              else
                                 --  Not same delimiters.
                                 raise SAL.Not_Implemented with "FIXME: need test cases";
                              end if;
                           end if;

                           if KMN.Deleted_Bytes > 0 then
                              if Overlaps (Node_Byte_Region, Deleted_Region) then
                                 --  test_incremental.adb Edit_String_09
                                 if Lexer_Errors (Cur).Scan_End /= Invalid_Buffer_Pos then
                                    raise SAL.Not_Implemented with "FIXME: insert and delete both set scan_end";
                                 end if;
                                 Lexer_Errors (Cur).Scan_End := Tree.Lexer.Find_Scan_End
                                   (Tree.ID (Node), Node_Byte_Region + Shift_Bytes +
                                      (if Node_Byte_Region.First > Stable_Region.Last
                                       then 0
                                       else KMN.Inserted_Bytes),
                                    Inserted  => True,
                                    Start     => True);
                              end if;
                           end if;
                        end;

                     else
                        --  Not block delimited.
                        declare
                           Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Node, Trailing_Non_Grammar => True);
                        begin
                           if Node_Byte_Region.First in Stable_Region.First .. Next_KMN_Stable_First then
                              --  Now we know Shift for this lexer error. Node has not yet been
                              --  shifted, but we must include the shift for the current KMN if Node
                              --  is after Stable_Region.Last; test_incremental.adb Lexer_Errors_07.

                              --  The lexer error occurred while scanning the token or one of the
                              --  following non_grammars. test_incremental.adb Lexer_Errors_04.
                              Lexer_Errors (Cur).Scan_End := Node_Byte_Region.Last +
                                (if Node_Byte_Region.First > Stable_Region.Last
                                 then KMN.Inserted_Bytes - KMN.Deleted_Bytes
                                 else 0);
                           else
                              --  This error may be affected by a following KMN; no following lexer
                              --  error can be affected by this KMN.
                              exit;
                           end if;
                        end;
                     end if;

                     if Lexer_Errors (Cur).Scan_End <= Stable_Region.Last + Shift_Bytes then
                        --  This lexer error is not fixed by these edits.
                        declare
                           To_Delete : Cursor := Cur;
                        begin
                           Next (Cur);
                           Lexer_Errors.Delete (To_Delete);
                        end;
                     else
                        --  We must scan from this lexer error to find out if it is fixed.
                        if Trace_Lexer > Outline then
                           declare
                              Data : Lexer_Error_Data renames Lexer_Errors (Cur).Element.all;
                           begin
                              Tree.Lexer.Trace.Put_Line
                                ("lexer error on " & Tree.Image (Data.Node, Node_Numbers => True) &
                                   " possibly fixed by this KMN; scan end" & Data.Scan_End'Image);
                           end;
                        end if;
                        Next (Cur);
                     end if;
                  end;
               end loop;
            end;

            if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
               --  We only shift EOI after all KMN are processed; it may need to be
               --  shifted for more than one edit point. test_incremental.adb
               --  Edit_Comment_3.
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("nothing left to shift; terminal:" & Tree.Image
                       (Terminal, Non_Grammar => True, Augmented => True));
               end if;

            elsif not Delayed_Scan then
               --  If there was a Delayed_Scan, some changed tokens may be before
               --  Stable_Region, so we don't do Unchanged_Loop.
               --
               --  It is tempting to skip Unchanged_Loop if Shift_Bytes = 0 and
               --  Shift_Chars = 0 and Shift_Lines = 0. But we need to scan all
               --  Non_Grammar for Floating_Non_Grammar, which changes Shift_Lines.
               --  IMPROVEME: only need to scan trailing stable terminal?

               Unchanged_Loop :
               loop
                  exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
                  exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  --  All virtuals were deleted above by removing error corrections.
                  pragma Assert (Tree.Label (Terminal.Node) = Syntax_Trees.Source_Terminal);

                  if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                     --  Exit when Terminal may be changed by the current KMN edit; it is
                     --  partly past or adjacent to Stable_Region.Last. Also exit when last
                     --  KMN is done.
                     exit Unchanged_Loop when
                       Tree.ID (Terminal.Node) /= Tree.Lexer.Descriptor.SOI_ID and then
                       (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                        then Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last >
                          Stable_Region.Last -- Last KMN
                        else Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last >=
                          Stable_Region.Last);

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("stable shift " & Tree.Image
                             (Terminal.Node,
                              Non_Grammar => True, Terminal_Node_Numbers => True, Augmented => True));
                     end if;

                     --  Tree.Shift sets Terminal_Non_Grammar_Next to the first non-grammar
                     --  that may be modified.
                     Tree.Shift
                       (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines,
                        Last_Stable_Byte =>
                          (if KMN.Inserted_Bytes = 0 and KMN.Deleted_Bytes = 0
                           then Buffer_Pos'Last --  ada_mode-interactive_02.adb
                           else Stable_Region.Last),
                        Non_Grammar_Next => Terminal_Non_Grammar_Next);

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("  => " & Tree.Image
                             (Terminal.Node, Non_Grammar => True, Terminal_Node_Numbers => True,
                              Augmented => True));
                     end if;

                     if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                        Terminal_Shifted := True;
                        exit Unchanged_Loop;
                     else
                        Tree.Next_Terminal (Terminal);
                     end if;

                  else
                     --  The previous KMN left Terminal_Non_Grammar_Next /= No_Index
                     Terminal_Shifted := True;

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
                     end if;

                     --  Shift remaining non_grammar in Stable_Region
                     Non_Grammar_Loop :
                     loop
                        declare
                           Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        begin
                           exit Unchanged_Loop when Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.Last >
                             Stable_Region.Last;

                           Lexer.Shift (Non_Grammar (Terminal_Non_Grammar_Next), Shift_Bytes, Shift_Chars, Shift_Lines);

                           Terminal_Non_Grammar_Next := @ + 1;

                           if Terminal_Non_Grammar_Next = Non_Grammar.Last_Index then
                              Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                              Tree.Next_Terminal (Terminal);
                              exit Non_Grammar_Loop;
                           end if;
                        end;
                     end loop Non_Grammar_Loop;

                     if Trace_Incremental_Parse > Detail then
                        if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next cleared");
                        else
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                     end if;
                  end if;
               end loop Unchanged_Loop;
            end if;

            --  Unchanged_Loop exited because Terminal or Terminal.Non_Grammar is
            --  at least partly out of Stable_Region or adjacent to the edit
            --  start, or because it reached Tree.EOI. Therefore the edit start is
            --  in Terminal, or in a non-grammar token or whitespace before
            --  Terminal (or after Tree.EOI), or after Terminal if Terminal is
            --  deleted.

            if KMN.Deleted_Bytes > 0 then
               --  Delete tokens deleted by this KMN, preserving Terminal if
               --  necessary. test_incremental.adb Edit_Code_03, _04.
               --
               --  If deleting a grammar token, delete start is before the token,
               --  delete end may be in its non_grammar; check for deleted comment
               --  start and float non-deleted non_grammar. If not deleting a grammar
               --  token, delete start is in its non_grammar, delete end may be in or
               --  after its non_grammar; check for deleted comment start and end
               --  (there can be one of each) and float non-deleted non_grammar. To
               --  simplify the code, we always check for comment start and comment
               --  end deleted.

               declare
                  --  If we do Breakdown on a copy of Terminal, Terminal may become
                  --  invalid because Terminal.Element is deleted. So before the first
                  --  delete, we save the previous terminal.
                  Saved_Prev_Terminal : Terminal_Ref := Invalid_Stream_Node_Ref;

                  Check_Deleted : Terminal_Ref :=
                    (if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index or
                       Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last < Deleted_Region.First
                     then Tree.Next_Terminal (Terminal)
                     else Terminal);

                  Terminal_Is_Check_Deleted : Boolean;

                  Keep_Terminal : constant Boolean :=
                    Tree.ID (Terminal.Node) in Tree.Lexer.Descriptor.SOI_ID | Tree.Lexer.Descriptor.EOI_ID or
                    Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index or -- comment is modified
                    Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First <
                    Deleted_Region.First; -- terminal is modified

                  procedure Check_Scan_End
                    (ID           : in Token_ID;
                     Start_Region : in Buffer_Region;
                     End_Region   : in Buffer_Region)
                  --  Check if the start or end delimiter is deleted or modified.
                  is
                     Start_Changed : constant Boolean := Deleted_Region.First <= Start_Region.Last and
                       Deleted_Region.Last >= Start_Region.First; -- start delimiter deleted or modified

                     End_Changed : constant Boolean := Deleted_Region.Last >= End_Region.Last and
                          Deleted_Region.First <= End_Region.First; -- end delimiter deleted or modified
                  begin
                     if Start_Changed and End_Changed then
                        return;

                     elsif Start_Changed or End_Changed then
                        --  test_incremental.adb Delete_Comment_Start_*, Edit_String_*, Edit_Comment_16,
                        --  not ada_mode-interactive_01.adb "-- ada_identifier"
                        Scan_End := Tree.Lexer.Find_Scan_End
                          (ID,
                           (Start_Region.First + Shift_Bytes +
                              --  If not Start_Changed, start delimiter is before the current KMN
                              (if Start_Changed then KMN.Inserted_Bytes - KMN.Deleted_Bytes else 0),
                            End_Region.Last    + Shift_Bytes + KMN.Inserted_Bytes - KMN.Deleted_Bytes),
                           Inserted => False,
                           Start    => Start_Changed);

                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
                             ("start or end delimiter deleted or modified:" &
                                Start_Region.First'Image & " .." & Scan_End'Image);
                        end if;
                     end if;
                  end Check_Scan_End;

                  procedure Check_Scan_End (Token : in Lexer.Token)
                  with Pre => Tree.Lexer.Is_Block_Delimited (Token.ID)
                  --  Token is modified; check if the start or end delimiter is deleted or modified.
                  is
                  begin
                     Check_Scan_End
                       (ID           => Token.ID,
                        Start_Region =>
                          (Token.Byte_Region.First,
                           Token.Byte_Region.First + Buffer_Pos (Tree.Lexer.Start_Delimiter_Length (Token.ID)) - 1),
                        End_Region   =>
                          (Token.Byte_Region.Last - Buffer_Pos (Tree.Lexer.End_Delimiter_Length (Token.ID)) + 1,
                           Token.Byte_Region.Last));
                  end Check_Scan_End;

                  procedure Check_Scan_End (Node : in Valid_Node_Access)
                  with Pre => Tree.Lexer.Is_Block_Delimited (Tree.ID (Node))
                  --  Check if the start delimiter is deleted.
                  is
                     ID          : constant Token_ID      := Tree.ID (Node);
                     Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node, Trailing_Non_Grammar => False);
                  begin
                     Check_Scan_End
                       (ID,
                        Start_Region =>
                          (Byte_Region.First,
                           Byte_Region.First + Buffer_Pos (Tree.Lexer.Start_Delimiter_Length (ID)) - 1),
                        End_Region =>
                          (Byte_Region.Last - Buffer_Pos (Tree.Lexer.End_Delimiter_Length (ID)) + 1,
                           Byte_Region.Last));
                  end Check_Scan_End;

                  procedure Handle_Non_Grammar
                    (Non_Grammar    : in out Lexer.Token_Arrays.Vector;
                     Delete_Grammar : in     Boolean;
                     Floating       : in     Boolean)
                  --  Delete start and/or end is in Non_Grammar. Check if it has any
                  --  partly or not deleted tokens, and if it has a deleted comment end
                  --  with a remaining comment start and vice versa. If Delete_Grammar,
                  --  the grammar token that owns Non_Grammar is being deleted.
                  is
                     type Action_Type is (Keep, Delete, Float);

                     --  If the KMN deletes from the middle of Non_Grammar, and not
                     --  Delete_Grammar, we can have actions like:
                     --  Keep, Keep, Delete, Keep, Keep.
                     --
                     --  If Delete_Grammar, that could be:
                     --  Keep, Keep, Delete, Keep, Float
                     --
                     --  Where token 4 is modified. There are two ranges to delete.
                     --
                     --  For the cases we do handle, any deletes will always be contiguous,
                     --  and floats will immediately follow the deletes.

                     Delete_First : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Delete_Last  : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Float_First  : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Float_Last   : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                  begin
                     --  First decide what to keep and delete, and float the ones that need to be floated
                     for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                        declare
                           Token  : Lexer.Token renames Non_Grammar (I);
                           Action : Action_Type := Keep;
                        begin
                           if Token.ID in Tree.Lexer.Descriptor.SOI_ID | Tree.Lexer.Descriptor.EOI_ID then
                              null;

                           elsif Token.Byte_Region.First >= Deleted_Region.First and
                             Token.Byte_Region.Last <= Deleted_Region.Last
                           then
                              --  Token is deleted.
                              Action := Delete;

                           elsif (Deleted_Region.First <= Token.Byte_Region.First and
                                    Deleted_Region.Last >= Token.Byte_Region.First and
                                    Deleted_Region.Last < Token.Byte_Region.Last) or
                             (Token.Byte_Region.First < Deleted_Region.First and
                                Token.Byte_Region.Last >= Deleted_Region.First)
                           then
                              --  Token is modified; it needs to be scanned.
                              if Delete_Grammar then
                                 --  If this edit also modified a precending grammar token, the scan
                                 --  will start there, and include this modified non_grammar.
                                 --  test_incremental.adb Delete_Comment_Start_05.
                                 --
                                 --  Otherwise, the scan will start at the end of the deleted text, and
                                 --  include this non_grammar; test_incremental.adb
                                 --  Delete_Comment_Start_06.
                                 Action := Float;

                              else
                                 pragma Assert (if Floating then Delayed_Scan); -- test_incremental.adb Edit_Comment_02

                                 --  If Floating, Non_Grammar was edited by a previous change,
                                 --  Delayed_Scan is true, so it will be scanned. We leave it in
                                 --  Non_Grammar to be consistent. If not Floating, we leave it in
                                 --  Check_Deleted.Non_Grammar so it is included in the scan start
                                 --  compute below.
                                 Action := Keep;
                              end if;

                              if Tree.Lexer.Is_Block_Delimited (Token.ID) then
                                 Check_Scan_End (Token);
                              end if;

                           else
                              --  Token is neither deleted nor modified.
                              if Floating then
                                 Action := Keep;
                              elsif Delete_Grammar then
                                 Action := Float;
                              else
                                 Action := Keep;
                              end if;
                           end if;

                           case Action is
                           when Keep =>
                              null;

                           when Delete =>
                              if Delete_First = Non_Grammar.First_Index - 1 then
                                 Delete_First := I;
                              end if;
                              Delete_Last := I;

                              if not Floating then
                                 --  Floating_Non_Grammar lines are included in Shift_Lines when
                                 --  floated.

                                 if Keep_Terminal then
                                    --  test_incremental.adb Edit_Code_09
                                    Deleted_Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                                 else
                                    Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                                 end if;
                              end if;

                              if Trace_Incremental_Parse > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("delete deleted " & (if Floating then "floating " else "") &
                                      "non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                              end if;

                           when Float =>
                              pragma Assert (not Floating);
                              if Keep_Terminal then
                                 --  test_incremental.adb Edit_Code_09
                                 Deleted_Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                              else
                                 Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                              end if;

                              Floating_Non_Grammar.Append (Token);
                              if Float_First = Non_Grammar.First_Index - 1 then
                                 Float_First := I;
                              end if;
                              Float_Last := I;

                              if Trace_Incremental_Parse > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("float non_grammar.1 " & Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                              end if;
                           end case;
                        end;
                     end loop;

                     --  Delete the deleted and floated.
                     declare
                        procedure Delete_Range (First, Last : in SAL.Base_Peek_Type)
                        is begin
                           if First < Non_Grammar.First_Index then
                              null;

                           elsif First = Non_Grammar.First_Index then
                              if Last = Non_Grammar.Last_Index then
                                 Non_Grammar.Clear;
                              else
                                 Non_Grammar.Set_First_Last (Last + 1, Non_Grammar.Last_Index);
                              end if;

                           elsif Last = Non_Grammar.Last_Index then
                              Non_Grammar.Set_First_Last (Non_Grammar.First_Index, First - 1);

                           else
                              --  Delete slice from middle
                              declare
                                 New_Non_Grammar : Lexer.Token_Arrays.Vector;
                              begin
                                 for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                    if I < First then
                                       New_Non_Grammar.Append (Non_Grammar (I));
                                    elsif I > Last then
                                       New_Non_Grammar.Append (Non_Grammar (I));
                                    end if;
                                 end loop;
                                 Non_Grammar := New_Non_Grammar;
                              end;
                           end if;
                        end Delete_Range;
                     begin
                        Delete_Range (Delete_First, Delete_Last);
                        Delete_Range (Float_First, Float_Last);
                     end;
                  end Handle_Non_Grammar;

                  procedure Do_Delete
                  is
                  begin
                     if Terminal_Is_Check_Deleted then
                        Terminal := Invalid_Stream_Node_Ref; -- allow deleting Terminal.Element via Check_Deleted
                     end if;

                     if Saved_Prev_Terminal = Invalid_Stream_Node_Ref and not Terminal_Is_Check_Deleted then
                        if Terminal.Element = Check_Deleted.Element then
                           Check_Deleted.Element := Invalid_Stream_Index;

                           Breakdown (Terminal, To_Single => False);

                           --  Find the stream element that contains Check_Deleted_Node.
                           Check_Deleted.Element := Terminal.Element;
                           loop
                              pragma Assert
                                (Tree.ID (Tree.Get_Node (Check_Deleted.Stream, Check_Deleted.Element)) /=
                                   Tree.Lexer.Descriptor.EOI_ID);

                              if Tree.Is_Descendant_Of
                                (Root => Tree.Get_Node (Check_Deleted.Stream, Check_Deleted.Element),
                                 Descendant => Check_Deleted.Node)
                              then
                                 exit;
                              end if;
                              Check_Deleted.Element := Tree.Stream_Next (Check_Deleted.Stream, Check_Deleted.Element);
                           end loop;
                           if Terminal.Element = Check_Deleted.Element then
                              --  Check_Deleted.Element was not deleted.
                              Saved_Prev_Terminal := Tree.Prev_Terminal (Terminal);
                              Terminal := Invalid_Stream_Node_Ref;
                           end if;
                        end if;
                     end if;

                     pragma Assert
                       (Terminal_Is_Check_Deleted or else
                        (if Saved_Prev_Terminal = Invalid_Stream_Node_Ref
                         then Terminal.Element /= Check_Deleted.Element
                         else Saved_Prev_Terminal.Element /= Check_Deleted.Element));

                     Breakdown (Check_Deleted, To_Single => True);

                     declare
                        To_Delete : Stream_Node_Ref := Check_Deleted;
                     begin
                        Tree.Next_Terminal (Check_Deleted);
                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
                             ("delete deleted " &
                                Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => False));
                        end if;

                        Maybe_Delete_Lexer_Errors (To_Delete.Node);

                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (To_Delete.Node)) then
                           Check_Scan_End (To_Delete.Node);
                        end if;

                        --  FIXME: if terminal_is_check_deleted, we already did this
                        Handle_Non_Grammar
                          (Tree.Non_Grammar_Var (To_Delete.Node), Delete_Grammar => True, Floating => False);

                        pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                        Tree.Stream_Delete (Stream, To_Delete.Element);
                     end;

                     if Terminal_Is_Check_Deleted then
                        Terminal := Check_Deleted;
                     end if;
                  end Do_Delete;

               begin
                  if Floating_Non_Grammar.Length > 0 then
                     --  test_incremental.adb Edit_Comment_3
                     Handle_Non_Grammar (Floating_Non_Grammar, Delete_Grammar => False, Floating => True);
                  end if;

                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     Handle_Non_Grammar
                       (Tree.Non_Grammar_Var (Terminal.Node), Delete_Grammar => False, Floating => False);
                  end if;

                  Delete_Deleted_Loop :
                  loop
                     Terminal_Is_Check_Deleted := Terminal = Check_Deleted;

                     exit Delete_Deleted_Loop when Check_Deleted = Invalid_Stream_Node_Ref;
                     --  Happens when Terminal is EOI. test_incremental.adb Edit_Comment_3

                     exit Delete_Deleted_Loop when Tree.ID (Check_Deleted.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;
                     --  FIXME: exit when check_deleted outside KMN?

                     if Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).First >
                       Deleted_Region.Last + 1
                     then
                        --  Check_Deleted is not deleted or modified
                        exit Delete_Deleted_Loop;

                     elsif Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).First <
                       Deleted_Region.First or
                       Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).Last > Deleted_Region.Last
                     then
                        --  Check_Deleted is not deleted, but potentially modified.
                        --  test_incremental.adb Edit_Code_04, Edit_Code_05.
                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Check_Deleted.Node)) then
                           --  test_incremental.adb Edit_String_01
                           Check_Scan_End (Check_Deleted.Node);
                        end if;

                        Handle_Non_Grammar
                          (Tree.Non_Grammar_Var (Check_Deleted.Node), Delete_Grammar => False, Floating => False);

                        if Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).Last >
                          Deleted_Region.Last
                        then
                           --  test_incremental.adb Edit_Comment_10
                           exit Delete_Deleted_Loop;
                        else
                           --  test_incremental.adb Delete_Comment_Start_05
                           Tree.Next_Terminal (Check_Deleted);
                        end if;

                     else
                        Do_Delete;
                     end if;
                  end loop Delete_Deleted_Loop;

                  if Keep_Terminal then
                     if Saved_Prev_Terminal /= Invalid_Stream_Node_Ref then
                        Terminal := Tree.Next_Terminal (Saved_Prev_Terminal);
                        --  Terminal_Non_Grammar_Next is unchanged.
                     end if;
                  else
                     Terminal := Check_Deleted;
                  end if;
                  if Trace_Incremental_Parse > Extra then
                     Tree.Lexer.Trace.Put_Line
                       ("terminal: " & Tree.Image (Terminal, Non_Grammar => True, Node_Numbers => True));
                     Tree.Lexer.Trace.Put_Line ("deleted_shift_lines:" & Deleted_Shift_Lines'Image);
                  end if;
               end;
            end if;

            --  Now decide what to scan.
            --
            --  If two edit regions affect the same token, scanning the first will
            --  also scan the second.
            declare
               Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                 (Terminal.Node, Trailing_Non_Grammar => Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index);

               Do_Scan : Boolean := False;

               Lex_Start_Byte : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Char : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Line : Line_Number_Type := Line_Number_Type'Last;

               Last_Grammar       : Stream_Node_Ref := Invalid_Stream_Node_Ref;
               Last_Scanned_Token : Lexer.Token;

               procedure Check_Scan_End
                 (ID     : in Token_ID;
                  Region : in Buffer_Region)
               --  Check if Inserted_Region inserts an end delimiter for ID in
               --  Region.
               is
                  Shift : constant Base_Buffer_Pos := KMN.Inserted_Bytes - KMN.Deleted_Bytes +
                    (if Delayed_Scan then Shift_Bytes else 0);
               begin
                  if KMN.Inserted_Bytes > 0 and then
                    Inserted_Region.First <= Region.Last
                  then
                     declare
                        Delimiter_Pos : constant Base_Buffer_Pos := Tree.Lexer.Contains_End_Delimiter
                          (ID, Inserted_Region);
                     begin
                        if Delimiter_Pos /= Invalid_Buffer_Pos then
                           --  test_incremental.adb Edit_Comment_5, _12, Edit_String_*, ada_mode-interactive_02.adb
                           Scan_End := Tree.Lexer.Find_Scan_End
                             (ID, (Delimiter_Pos, Region.Last + Shift), Inserted => True, Start => False);

                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("end delimiter inserted:" &
                                   Region.First'Image & " .." &
                                   Scan_End'Image);
                           end if;
                        end if;
                     end;
                  end if;
               end Check_Scan_End;

               procedure Check_Scan_End (Token : in Lexer.Token)
               with Pre => Tree.Lexer.Is_Block_Delimited (Token.ID)
               --  Check if Inserted_Region inserts an end delimiter in Token,
               --  exposing the rest of Token as code.
               is begin
                  Check_Scan_End (Token.ID, Token.Byte_Region);
               end Check_Scan_End;

               procedure Check_Scan_End (Node : in Valid_Node_Access)
               with Pre => Tree.Lexer.Is_Block_Delimited (Tree.ID (Node))
               --  Check if Inserted_Region inserts an end delimiter in Node,
               --  exposing the rest of Node as code.
               is begin
                  Check_Scan_End (Tree.ID (Node), Tree.Byte_Region (Node, Trailing_Non_Grammar => False));
               end Check_Scan_End;

            begin
               if Lexer_Errors.Length > 0 and then
                 Lexer_Errors (Lexer_Errors.First).Scan_End /= Invalid_Buffer_Pos
               then
                  --  Lexer_Errors is set above to contain lexer errors that may be
                  --  affected by this KMN. test_incremental.adb Edit_String_06,
                  --  Lexer_Errors_nn.
                  declare
                     Data : Lexer_Error_Data renames Lexer_Errors (Lexer_Errors.First);
                     Ref : Stream_Node_Ref;

                     procedure Delete_Node_To_Terminal
                     --  Delete terminals Ref thru prev (Terminal); normally scanned
                     --  tokens get deleted in Delete_Scanned_Loop below, but that only
                     --  deletes tokens Terminal and after.
                     is begin
                        loop
                           Breakdown (Ref, To_Single => True);
                           declare
                              To_Delete : Stream_Node_Ref := Ref;
                           begin
                              Tree.Next_Terminal (Ref);
                              Tree.Stream_Delete (Stream, To_Delete.Element);
                           end;
                           exit when Ref.Node = Terminal.Node;
                        end loop;
                     end Delete_Node_To_Terminal;

                  begin
                     --  FIXME: handle lexer error in non_grammar

                     if Data.Scan_Start_Node /= Invalid_Node_Access then
                        --  Breakdown Terminal so we can delete terminals before Terminal.
                        Breakdown (Terminal);
                        Ref := Tree.To_Stream_Node_Ref (Stream, Data.Scan_Start_Node);
                        Delete_Node_To_Terminal;

                        Do_Scan        := True;
                        Lex_Start_Byte := Tree.Byte_Region (Data.Scan_Start_Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Char := Tree.Char_Region (Data.Scan_Start_Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Line := Tree.Line_Region (Ref, Trailing_Non_Grammar => False).First;
                        Scan_End       := Data.Scan_End;

                     elsif Data.Node = Terminal.Node then
                        --  Data.Node is not shifted, and Err may be before or after
                        --  Terminal.Byte_Region.
                        declare
                           Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Data.Node, Trailing_Non_Grammar => False);
                        begin
                           if Inserted_Region.First < Terminal_Byte_Region.First then
                              --  test_incremental.adb Lexer_Errors_07
                              Do_Scan        := True;
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Tree.Line_At_Node (Terminal, Tree.Shared_Stream);
                              Scan_End       := Data.Scan_End;
                           else
                              Do_Scan        := True;
                              Lex_Start_Byte := Terminal_Byte_Region.First;
                              Lex_Start_Char := Tree.Char_Region (Data.Node, Trailing_Non_Grammar => False).First;
                              Lex_Start_Line := Tree.Line_At_Node (Terminal, Tree.Shared_Stream);
                              Scan_End       := Data.Scan_End;
                           end if;
                        end;

                        if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                           Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                        end if;

                     elsif Tree.Byte_Region (Data.Node, Trailing_Non_Grammar => False).First <
                       Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First
                     then
                        --  Data.Node is shifted.

                        --  Breakdown Terminal so it does not share a stream element with
                        --  elements being deleted, and we can delete terminals before
                        --  Terminal.
                        Breakdown (Terminal);
                        Ref := Tree.To_Stream_Node_Ref (Stream, Data.Node);
                        Delete_Node_To_Terminal;

                        Do_Scan        := True;
                        Lex_Start_Byte := Tree.Byte_Region (Data.Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Char := Tree.Char_Region (Data.Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Line := Tree.Line_At_Node (Ref, Tree.Shared_Stream);
                        Scan_End       := Data.Scan_End;

                        if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                           Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                        end if;

                     else
                        --  Scan start determined by Terminal, Terminal_Non_Grammar_Next below.
                        --  test_incremental.adb Lexer_Errors_03
                        Scan_End := Data.Scan_End;
                     end if;
                  end;

                  Lexer_Errors.Delete_First;
               end if;

               if Do_Scan then
                  --  Set from a Lexer_Error
                  null;

               elsif Delayed_Scan then
                  --  A previous edit start affected Terminal or Floating_Non_Grammar
                  --  (Delayed_Floating_Index). test_incremental.adb Edit_String_07
                  declare
                     Token_ID : constant WisiToken.Token_ID :=
                       (if Delayed_Floating_Index = Positive_Index_Type'Last
                        then Tree.ID (Terminal.Node)
                        else Floating_Non_Grammar (Delayed_Floating_Index).ID);

                     Token_Byte_Region : constant Buffer_Region :=
                       (if Delayed_Floating_Index = Positive_Index_Type'Last
                        then Tree.Byte_Region (Terminal)
                        else Floating_Non_Grammar (Delayed_Floating_Index).Byte_Region);
                  begin
                     if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                       Next_KMN_Stable_First < Token_Byte_Region.Last
                     then
                        --  Next change also edits the token; more delay.
                        null;
                     else
                        Do_Scan        := True;
                        Lex_Start_Byte := Delayed_Lex_Start_Byte;
                        Lex_Start_Char := Delayed_Lex_Start_Char;
                        Lex_Start_Line := Delayed_Lex_Start_Line;

                        if Tree.Lexer.Is_Block_Delimited (Token_ID) then
                           if Delayed_Floating_Index = Positive_Index_Type'Last then
                              Check_Scan_End (Terminal.Node);
                           else
                              Check_Scan_End (Floating_Non_Grammar (Delayed_Floating_Index));
                           end if;
                        end if;

                        Delayed_Scan := False;
                     end if;
                  end;

               elsif KMN.Inserted_Bytes = 0 and KMN.Deleted_Bytes = 0 then
                  --  Nothing to scan; last KMN
                  null;

               elsif Next_KMN_Stable_First + Shift_Bytes < Scanned_Byte_Pos + Base_Buffer_Pos (KMN.Deleted_Bytes) then
                  --  All of current edit has been scanned, and scan end is
                  --  not adjacent to KMN end. test_incremental.adb Edit_Comment_2
                  null;

               elsif Terminal_Byte_Region.Last + Shift_Bytes +
                 (if Inserted_Region.First <= Terminal_Byte_Region.Last + Shift_Bytes
                  then Base_Buffer_Pos (KMN.Inserted_Bytes)
                  else 0) > Scanned_Byte_Pos
                  --  Else Terminal was scanned by a previous KMN. test_incremental.adb
                  --  Edit_Code_12, _14, _16, ada_mode-recover_align_1.adb,
                  --  ada_mode-interactive_02.adb

               then
                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     --  Edit start is in Terminal_Non_Grammar_Next.
                     --  test_incremental.adb Edit_Comment*

                     declare
                        Non_Grammar  : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        Last_Floated : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
                     begin
                        declare
                           Token : Lexer.Token renames Non_Grammar (Terminal_Non_Grammar_Next);
                        begin
                           if Token.Byte_Region.First + Shift_Bytes >
                             Inserted_Region.First
                           then
                              --  Edit start is in whitespace preceding Token
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line :=
                                (if Terminal_Non_Grammar_Next > Non_Grammar.First_Index
                                 then Non_Grammar (Terminal_Non_Grammar_Next - 1).Line_Region.Last
                                 else Tree.Line_At_Node (Terminal, Tree.Shared_Stream));
                              Do_Scan := True;
                           else
                              --  Edit start is in or just after Token
                              Lex_Start_Byte := Token.Byte_Region.First + Shift_Bytes;
                              Lex_Start_Char := Token.Char_Region.First + Shift_Chars;
                              Lex_Start_Line := Token.Line_Region.First + Shift_Lines;
                              Do_Scan := True;

                              if Tree.Lexer.Is_Block_Delimited (Token.ID) then
                                 Check_Scan_End (Token);
                              end if;
                           end if;
                        end;

                        --  Remaining Non_Grammar will either be scanned, or moved to a new
                        --  grammar token, so delete or move to floating now.
                        for I in Terminal_Non_Grammar_Next .. Non_Grammar.Last_Index loop
                           declare
                              Token       : Lexer.Token renames Non_Grammar (I);
                              Byte_Region : Buffer_Region renames Token.Byte_Region;
                           begin
                              if (KMN.Deleted_Bytes > 0 and then Byte_Region.First <= Deleted_Region.Last)
                                or
                                (KMN.Inserted_Bytes > 0 and then
                                   Byte_Region.First + Shift_Bytes <= Inserted_Region.Last)
                              then
                                 if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                                   Next_KMN_Stable_Last < Byte_Region.Last
                                 then
                                    --  Next change is an actual change (not just last placeholder KMN),
                                    --  and it also overlaps this token. It may insert or delete a comment
                                    --  end, so we don't know when to end a scan; handle it then.
                                    --  test_incremental.adb Edit_Comment_03, _07, ada_mode-partial_parse.adb.
                                    Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    Floating_Non_Grammar.Append (Non_Grammar (I));
                                    Last_Floated := I;
                                    Do_Scan := False;
                                    Delayed_Scan := True;
                                    Delayed_Floating_Index := Floating_Non_Grammar.Last_Index;
                                    Delayed_Lex_Start_Byte := Lex_Start_Byte;
                                    Delayed_Lex_Start_Char := Lex_Start_Char;
                                    Delayed_Lex_Start_Line := Lex_Start_Line;
                                    if Trace_Incremental_Parse > Detail then
                                       Tree.Lexer.Trace.Put_Line
                                         ("scan delayed 1" & Lex_Start_Byte'Image &
                                            (if Scan_End /= Invalid_Buffer_Pos
                                             then " .." & Scan_End'Image
                                             else ""));
                                       if Trace_Incremental_Parse > Extra then
                                          Tree.Lexer.Trace.Put_Line
                                            ("float non_grammar.2" & I'Image & ":" &
                                               Lexer.Full_Image (Non_Grammar (I), Tree.Lexer.Descriptor.all));
                                       end if;
                                    end if;
                                 else
                                    --  Token overlaps or is adjacent to the change region; it will be
                                    --  rescanned. Delete it here (ie don't copy to floating). It may
                                    --  contain New_Lines. test_incremental.adb Delete_Comment_End.
                                    if Trace_Incremental_Parse > Extra then
                                       Tree.Lexer.Trace.Put_Line
                                         ("delete non_grammar" & I'Image & ":" &
                                            Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                                    end if;
                                    declare
                                       New_Line_Count : constant Base_Line_Number_Type := WisiToken.New_Line_Count
                                         (Non_Grammar (I).Line_Region);
                                    begin
                                       Shift_Lines := @ - New_Line_Count;
                                    end;
                                 end if;
                              else
                                 --  Token does not overlap the edit region; handle it later.
                                 Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                 Floating_Non_Grammar.Append (Non_Grammar (I));
                                 if Trace_Incremental_Parse > Extra then
                                    Tree.Lexer.Trace.Put_Line
                                      ("float non_grammar.3" & I'Image & ":" &
                                         Lexer.Full_Image (Non_Grammar (I), Tree.Lexer.Descriptor.all));
                                 end if;
                                 Last_Floated := I;
                              end if;
                           end;
                        end loop;

                        if Terminal_Non_Grammar_Next = Non_Grammar.First_Index then
                           Non_Grammar.Clear;
                        else
                           Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Terminal_Non_Grammar_Next - 1);
                        end if;

                        if Trace_Incremental_Parse > Detail then
                           if Last_Floated /= Lexer.Token_Arrays.No_Index then
                              Tree.Lexer.Trace.Put_Line
                                ("float non_grammar.4" & Terminal_Non_Grammar_Next'Image & " .." &
                                   Last_Floated'Image);
                           end if;
                        end if;

                        Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;

                        Tree.Next_Terminal (Terminal);
                     end;

                  elsif Terminal_Byte_Region.First + Shift_Bytes < Inserted_Region.First then
                     --  Edit start is in Terminal, not at first byte. test_incremental.adb
                     --  Edit_Code_10, _11.

                     if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
                        if Length (Inserted_Region) > 0 then
                           --  Scan new text inserted at EOI.
                           Do_Scan        := True;
                           Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                           Lex_Start_Char := Tree.Char_Region (Terminal.Node, Trailing_Non_Grammar => False).First +
                             Shift_Chars;

                           Lex_Start_Line := Tree.Line_At_Node (Terminal, Tree.Shared_Stream);
                        else
                           --  We never re-scan eoi; we just shift it.
                           null;
                        end if;
                     else
                        Do_Scan        := True;
                        Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                        Lex_Start_Char := Tree.Char_Region (Terminal.Node, Trailing_Non_Grammar => False).First +
                          Shift_Chars;

                        Lex_Start_Line := Tree.Line_At_Node (Terminal, Tree.Shared_Stream);

                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Terminal.Node)) then
                           Check_Scan_End (Terminal.Node);
                        end if;
                     end if;

                  else
                     --  Edit start is in or adjacent to some non_grammar token or
                     --  whitespace preceding Terminal (including at Terminal first byte);
                     --  delete non_grammar tokens adjacent to, containing or after the
                     --  edit start; they will be rescanned (the scan loop exits on
                     --  terminals, not non_grammars). Deleted New_Lines decrement
                     --  Shift_Lines.

                     declare
                        procedure In_Whitespace
                        is begin
                           --  Edit start is in whitespace before Terminal.
                           --  test_incremental.adb Edit_Code_01, Edit_Whitespace_1, _2
                           --  ada_mode-incremental_04.adb
                           Lex_Start_Byte := Buffer_Pos'Max (Scanned_Byte_Pos + 1, Inserted_Region.First);
                           Lex_Start_Char := Buffer_Pos'Max (Scanned_Char_Pos + 1, Inserted_Region_Chars.First);

                           declare
                              Prev_Non_Grammar : Terminal_Ref := Tree.Prev_Terminal (Terminal);
                           begin
                              --  We can't use Tree.Line_Region (Prev) here, because if Prev has no
                              --  non_grammar, it uses the following non_grammar for result.last,
                              --  and that's not shifted yet. ada_mode-incremental_02.adb
                              if Tree.Non_Grammar_Const (Prev_Non_Grammar.Node).Length = 0 then
                                 Tree.Prev_Non_Grammar (Prev_Non_Grammar);
                              end if;
                              declare
                                 Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const
                                   (Prev_Non_Grammar.Node);
                              begin
                                 Lex_Start_Line := Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
                              end;
                           end;
                           Do_Scan := True;
                        end In_Whitespace;

                        procedure Handle_Non_Grammar
                          (Non_Grammar : in out WisiToken.Lexer.Token_Arrays.Vector;
                           Floating    : in     Boolean)
                        is
                           Last_Byte : constant Buffer_Pos :=
                             (if Non_Grammar.Length = 0
                              then Buffer_Pos'Last
                              else Non_Grammar (Non_Grammar.Last_Index).Byte_Region.Last +
                                (if Floating then Shift_Bytes else 0));

                           Delete : SAL.Base_Peek_Type := 0;
                        begin
                           if Non_Grammar.Length = 0 or else
                             Non_Grammar (Non_Grammar.Last_Index).ID = Tree.Lexer.Descriptor.SOI_ID
                           then
                              In_Whitespace;

                           elsif Last_Byte <= Scanned_Byte_Pos +
                             (if Deleted_Region.First <= Last_Byte
                              then Base_Buffer_Pos (KMN.Deleted_Bytes)
                              else 0)
                           then
                              --  All of Non_Grammar has been scanned already.
                              --  test_incremental.adb Edit_Comment_10, _17.

                              if (KMN.Inserted_Bytes = 0 or else
                                    (Inserted_Region.Last <= Scanned_Byte_Pos and
                                       Inserted_Region.Last < Terminal_Byte_Region.First + Shift_Bytes - 1)) and
                                (KMN.Deleted_Bytes = 0 or else
                                   (Deleted_Region.Last + Shift_Bytes <=
                                      Scanned_Byte_Pos + Base_Buffer_Pos (KMN.Deleted_Bytes) and
                                      Deleted_Region.Last < Terminal_Byte_Region.First - 1))
                              then
                                 --  Inserted and Deleted have been scanned already, and are not
                                 --  adjacent to Terminal. test_incremental.adb Edit_Code_14,
                                 --  Edit_Comment_10 ada_mode-interactive_02.adb
                                 null;

                              else
                                 In_Whitespace;
                              end if;
                           else
                              for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                 declare
                                    Byte_Last : constant Buffer_Pos := Non_Grammar (I).Byte_Region.Last +
                                      (if Floating then Shift_Bytes else 0);
                                 begin
                                    if Byte_Last + 1 >= Inserted_Region.First then
                                       --  We don't need to check Scanned_Byte_Pos here; we always scan all
                                       --  consecutive non_grammars, and we checked Scanned_Byte_Pos above.
                                       --  ada_mode-recover_align_1.adb, test_incremental.adb Edit_Comment_2
                                       Delete  := I;
                                       Do_Scan := True;
                                       exit;
                                    end if;
                                 end;
                              end loop;

                              if Delete > 0 and then Non_Grammar (Delete).ID = Tree.Lexer.Descriptor.SOI_ID then
                                 if Delete = Non_Grammar.Last_Index then
                                    Delete := 0;
                                 else
                                    Delete := Delete + 1;
                                 end if;
                              end if;

                              if Delete > 0 then
                                 --  Edit is in or before Non_Grammar (Delete) (ie a comment); set
                                 --  Lex_Start_* to scan from edit start or start of Token, whichever
                                 --  is earlier.

                                 declare
                                    Token : WisiToken.Lexer.Token renames Non_Grammar (Delete);
                                 begin
                                    if Tree.Lexer.Is_Block_Delimited (Token.ID) and
                                      Inserted_Region.First < Token.Byte_Region.Last
                                      --  Inserting in middle of Token, not adding to end.
                                    then
                                       declare
                                          Delimiter_Pos : constant Base_Buffer_Pos := Tree.Lexer.Contains_End_Delimiter
                                            (Token.ID, Inserted_Region);
                                       begin
                                          if Delimiter_Pos /= Invalid_Buffer_Pos then
                                             --  A new end delimiter is inserted in Token, exposing the rest of
                                             --  Token as code. test_incremental.adb Edit_Comment_4, Edit_Comment_7
                                             Scan_End := Tree.Lexer.Find_Scan_End
                                               (Token.ID,
                                                (Delimiter_Pos, Invalid_Buffer_Pos),
                                                Inserted => True,
                                                Start    => False);

                                             if Trace_Incremental_Parse > Detail then
                                                Tree.Lexer.Trace.Put_Line
                                                  ("end delimiter inserted:" &
                                                     Token.Byte_Region.First'Image & " .." &
                                                     Scan_End'Image);
                                             end if;
                                          end if;
                                       end;
                                    end if;

                                    Lex_Start_Byte := Buffer_Pos'Min
                                      (Token.Byte_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region.First);

                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region_Chars.First);

                                    if Floating then
                                       --  Tokens Delete .. Non_Grammar.Last contributed to Shift_Lines;
                                       --  ignore that contribution because they are after the lex start.
                                       --  test_incremental.adb Edit_Code_10
                                       --  ada_mode-interactive_10.adb
                                       --  ada_mode-recover_14 comment after extra 'begin'.
                                       declare
                                          Temp_Shift_Lines : Base_Line_Number_Type := Shift_Lines;
                                       begin
                                          for I in Delete .. Non_Grammar.Last_Index loop
                                             Temp_Shift_Lines := @ + New_Line_Count (Non_Grammar (I).Line_Region);
                                          end loop;
                                          Lex_Start_Line := Token.Line_Region.First + Temp_Shift_Lines;
                                       end;
                                    else
                                       Lex_Start_Line := Token.Line_Region.First;
                                    end if;
                                 end;

                                 if Trace_Incremental_Parse > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ((if Floating
                                        then "delete floating_non_grammar"
                                        else "delete non_grammar") &
                                         Delete'Image & " .." & Non_Grammar.Last_Index'Image);
                                 end if;

                                 if not Floating then
                                    for I in Delete .. Non_Grammar.Last_Index loop
                                       Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    end loop;
                                 end if;

                                 Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Delete - 1);
                              else
                                 In_Whitespace;
                              end if;
                           end if;
                        end Handle_Non_Grammar;
                     begin
                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Terminal.Node)) then
                           Check_Scan_End (Terminal.Node);
                        end if;

                        if Floating_Non_Grammar.Length > 0 and then
                          Floating_Non_Grammar (Floating_Non_Grammar.First_Index).Byte_Region.First + Shift_Bytes <=
                          Inserted_Region.First
                        then
                           --  The edit start is in a floated non_grammar.
                           --  test_incremental.adb Edit_Comment_7, Edit_Code_10, _17
                           --  ada_mode-recover_14.adb comment after deleted "begin".
                           Handle_Non_Grammar (Floating_Non_Grammar, Floating => True);
                        elsif Tree.ID (Terminal.Node) /= Tree.Lexer.Descriptor.SOI_ID then
                           Handle_Non_Grammar
                             (Tree.Non_Grammar_Var (Tree.Prev_Terminal (Terminal).Node), Floating => False);
                        end if;
                     end;
                  end if;
               end if;

               if Do_Scan then
                  if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                    (Next_KMN_Stable_Last < Terminal_Byte_Region.Last or
                       (Scan_End /= Invalid_Buffer_Pos and Next_KMN_Stable_Last + Shift_Bytes < Scan_End))
                  then
                     --  Next change is an actual change (not just last placeholder KMN),
                     --  and it also overlaps this token. It may insert or delete a delimiter
                     --  end, so we don't know when to end a scan; handle it then.
                     --  test_incremental.adb Edit_String_07.
                     --
                     --  Or Scan_End is past next KMN, so we don't know shift_bytes for
                     --  Delete_Scanned_Loop. test_incremental.adb Edit_String_15,
                     --  ada_mode-recover_incremental_03.adb
                     Do_Scan                := False;
                     Delayed_Scan           := True;
                     Delayed_Floating_Index := Positive_Index_Type'Last;
                     Delayed_Lex_Start_Byte := Lex_Start_Byte;
                     Delayed_Lex_Start_Char := Lex_Start_Char;
                     Delayed_Lex_Start_Line := Lex_Start_Line;

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line ("scan delayed 2");
                     end if;
                  end if;
               end if;

               if Do_Scan then
                  Last_Scanned_Token := (others => <>);

                  if Trace_Incremental_Parse > Detail then
                     Tree.Lexer.Trace.Put_Line
                       ("lexer.set_position" & Lex_Start_Byte'Image & Lex_Start_Char'Image & Lex_Start_Line'Image);
                     if Scan_End /= Invalid_Buffer_Pos then
                        Tree.Lexer.Trace.Put_Line ("scan_end " & Scan_End'Image);
                     end if;
                  end if;

                  Parser.Tree.Lexer.Set_Position
                    (Byte_Position => Lex_Start_Byte,
                     Char_Position => Lex_Start_Char,
                     Line          => Lex_Start_Line);

                  --  Ensure Terminal.Node is first in Terminal.Element, so we can
                  --  insert before it.
                  Breakdown (Terminal);

                  Last_Grammar := Tree.Prev_Terminal (Terminal);

                  Scan_Changed_Loop :
                  loop
                     declare
                        Token       : Lexer.Token;
                        Error_Count : constant Natural := Tree.Lexer.Find_Next (Token);
                        Ref         : Terminal_Ref;
                        Scan_Errors : Error_Data_Lists.List;
                     begin
                        if Trace_Lexer > Outline then
                           Tree.Lexer.Trace.Put_Line ("lex: " & Lexer.Image (Token, Parser.Tree.Lexer.Descriptor.all));
                        end if;

                        if Error_Count > 0 then
                           declare
                              Cur : WisiToken.Lexer.Error_Lists.Cursor := Tree.Lexer.Errors.Last;
                           begin
                              for I in 1 .. Error_Count - 1 loop
                                 WisiToken.Lexer.Error_Lists.Previous (Cur);
                              end loop;
                              for I in 1 .. Error_Count loop
                                 declare
                                    Error : Lexer.Error renames Tree.Lexer.Errors (Cur);
                                 begin
                                    Scan_Errors.Append (Lexer_Error'(Error => Error));

                                    if Trace_Lexer > Outline then
                                       Tree.Lexer.Trace.Put_Line
                                         (" ... error: " & Error.Char_Pos'Image &
                                            (if Error.Recover_Char (1) /= ASCII.NUL
                                             then "'" & Error.Recover_Char (1) & "'"
                                             else ""));
                                    end if;
                                 end;
                                 WisiToken.Lexer.Error_Lists.Next (Cur);
                              end loop;
                           end;
                        end if;

                        exit Scan_Changed_Loop when Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal and
                          Scan_End /= Invalid_Buffer_Pos
                        then
                           if Token.Byte_Region.First > Scan_End then
                              --  test_incremental.adb Edit_Comment_4, _5, _7, Delete_Comment_Start,
                              --  edit_String_*
                              --
                              --  We set Scan_End invalid after using it in Delete_Scanned_Loop below.
                              --  test_incremental.adb Edit_String_14.
                              exit Scan_Changed_Loop;
                           end if;
                        else
                           exit Scan_Changed_Loop when
                             Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal and then
                             not (Token.Byte_Region.First - Shift_Bytes <= Stable_Region.Last or
                                    --  Token started in stable region

                                    (KMN.Inserted_Bytes > 0 and then
                                       Token.Byte_Region.First <= Inserted_Region.Last + 1
                                       --  Token starts in or immediately after inserted region
                                       --  test_incremental.adb Edit_Code_4 '1 +', Edit_Code_8 ';'

                                    ) or
                                    (KMN.Deleted_Bytes > 0 and then
                                       Token.Byte_Region.First - (Shift_Bytes + KMN.Inserted_Bytes) =
                                       Deleted_Region.First
                                       --  Previously existing token starts immediately after deleted region;
                                       --  it may have been truncated (test_incremental.adb Edit_Code_4 'Cc')
                                    ));
                        end if;

                        Scanned_Byte_Pos   := Token.Byte_Region.Last;
                        Scanned_Char_Pos   := Token.Char_Region.Last;
                        Last_Scanned_Token := Token;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal then
                           --  grammar token
                           Ref := Tree.Insert_Source_Terminal
                             (Stream, Token,
                              Before => Terminal.Element,
                              Errors => Scan_Errors);

                           Process_Grammar_Token (Parser, Token, Ref.Node);
                           Last_Grammar := Ref;

                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line ("scan new " & Tree.Image (Ref));
                           end if;

                        else
                           --  non_grammar token
                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("scan new " & Lexer.Full_Image (Token, Parser.Tree.Lexer.Descriptor.all));
                           end if;

                           if Error_Count > 0 then
                              --  test_incremental.adb Lexer_Errors_04
                              Tree.Add_Errors (Tree.Shared_Stream, Last_Grammar.Node, Scan_Errors);
                           end if;
                           Process_Non_Grammar_Token (Parser, Last_Grammar.Node, Token);
                           Shift_Lines := @ + New_Line_Count (Token.Line_Region);
                        end if;

                     end;
                  end loop Scan_Changed_Loop;
               end if;

               --  Do this here so Shift_Bytes is correct in Delete_Scanned_Loop.
               --
               --  However, if Terminal is before the edit region, the previous
               --  shift applies. test_incremental.adb Edit_Whitespace
               Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
               Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

               Shift_Lines := @ + Deleted_Shift_Lines;

               Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
               Old_Char_Pos := Stable_Region_Chars.Last + KMN.Deleted_Chars;
               New_Byte_Pos := Inserted_Region.Last;
               New_Char_Pos := Inserted_Region_Chars.Last;
               pragma Assert (New_Byte_Pos - Old_Byte_Pos = Shift_Bytes);

               if Last_Scanned_Token.ID /= Invalid_Token_ID then
                  --  If Last_Scanned_Token.ID = Invalid_Token_ID, only whitespace was
                  --  scanned; Edit_Comment_8
                  --
                  --  We don't check Do_Scan, because we may be processing the next KMN
                  --  covered by a previous scan.
                  --
                  --  Delete tokens in the current KMN that were replaced by the scan.
                  --
                  --  If a scan covers more than one KMN, we can't process more than the
                  --  first because Shift_* is not known. test_incremental.adb
                  --  Edit_Code_4, Edit_Comment_13, ada_skel.adb ada-skel-return.
                  --
                  --  If the last token scanned was a comment created by an inserted
                  --  comment start or extended by a deleted comment end then we must
                  --  delete all tokens are now part of by the comment.
                  --  test_incremental.adb Delete_Comment_End, Edit_Comment_9,
                  --  Insert_Comment_Start
                  Delete_Scanned_Loop :
                  loop
                     exit Delete_Scanned_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

                     exit Delete_Scanned_Loop when Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First >
                       (if Scan_End = Invalid_Buffer_Pos
                        then Next_KMN_Stable_Last
                        else Scan_End - (if Terminal_Shifted then 0 else Shift_Bytes));
                     --  Terminal is in next KMN; we don't know Shift_Bytes to compare to
                     --  Scanned_Byte_Pos. test_incremental.adb Edit_Comment_13

                     Terminal_Shifted := False;

                     exit Delete_Scanned_Loop when Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First + Shift_Bytes -
                       (if Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First <= Stable_Region.Last
                        then -KMN.Deleted_Bytes + KMN.Inserted_Bytes else 0) > Scanned_Byte_Pos;

                     if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.SOI_ID then
                        Tree.Next_Terminal (Terminal);

                     else
                        --  Ensure Terminal is Single, so we can delete it.
                        Breakdown (Terminal, To_Single => True);

                        declare
                           To_Delete : Stream_Node_Ref := Terminal;
                        begin
                           Tree.Next_Terminal (Terminal);
                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("delete modified " &
                                   Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => True));
                           end if;

                           --  We always scan all non_grammar immediately following a scanned
                           --  terminal.
                           for Token of Tree.Non_Grammar_Const (To_Delete.Node) loop
                              Shift_Lines := @ - New_Line_Count (Token.Line_Region);

                              if (Token.Byte_Region.Last + Shift_Bytes <= Scanned_Byte_Pos and
                                    Token.Byte_Region.First + Shift_Bytes <= Next_KMN_Stable_Last + Shift_Bytes)
                                --  Token was scanned, and is in current KMN
                                or
                                Token.Byte_Region.Last <= Deleted_Region.Last
                                --  token was deleted. test/ada_mode-interactive_03.adb delete text end of buffer.
                              then
                                 if Trace_Incremental_Parse > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ("delete non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                              else
                                 if Trace_Incremental_Parse > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ("float non_grammar.5 " & Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                                 Floating_Non_Grammar.Append (Token);
                              end if;
                           end loop;

                           pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                           Tree.Stream_Delete (Stream, To_Delete.Element);
                           Maybe_Delete_Lexer_Errors (To_Delete.Node);
                        end;
                     end if;
                  end loop Delete_Scanned_Loop;
                  Scan_End := Invalid_Buffer_Pos;
               end if;

               --  If any Floating_Non_Grammar are in this KMN's change region or next
               --  KMN stable, they can be handled here.
               declare
                  Last_Handled_Non_Grammar : SAL.Base_Peek_Type := Lexer.Token_Arrays.No_Index;

                  function Find_Element
                    (Target_Bytes : in Buffer_Pos;
                     After        : in Boolean)
                    return Terminal_Ref
                  --  If Target_Bytes < Terminal.Byte_Region.First: If After
                  --  is True, return terminal node that is after or contains
                  --  Target_Bytes, where prev terminal is before Target_Bytes. Else
                  --  return terminal that is before Target_Bytes, where next is after.
                  --
                  --  Otherwise similar, but searching forward.
                  --
                  --  Target_Bytes is unshifted.
                  is
                     Terminal_First : constant Buffer_Pos := Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First;
                     Searching_Back : constant Boolean := Terminal_First > Target_Bytes;

                     Before_1 : Terminal_Ref := -- before Before
                       (if Searching_Back
                        then Tree.Prev_Terminal (Terminal)
                        else Terminal);
                     Before      : Terminal_Ref :=
                       (if Searching_Back
                        then Terminal
                        else Tree.Next_Terminal (Terminal));
                  begin
                     loop
                        if Searching_Back then
                           if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.SOI_ID then
                              return Terminal;
                           end if;

                           declare
                              --  Target_Bytes is unshifted. If searching forward, all nodes are
                              --  also unshifted. If searching back, all nodes except Terminal are
                              --  shifted. Compare Target_Bytes to unshifted region.
                              --
                              --  region bounds test case: ada_mode-recover_partial_14.adb
                              --  contains; ada_mode-recover_42.adb lexer_error string_literal
                              Shift_First : constant Base_Buffer_Pos := -Shift_Bytes;
                              Shift_Last : constant Base_Buffer_Pos := (if Before = Terminal then 0 else -Shift_Bytes);

                              First : constant Buffer_Pos := Tree.Byte_Region
                                (Before_1.Node, Trailing_Non_Grammar => False).Last + 1 + Shift_First;
                              Last  : constant Buffer_Pos :=
                                (if After
                                 then Tree.Byte_Region (Before).Last
                                 else Tree.Byte_Region (Before).First - 1) + Shift_Last;
                           begin
                              exit when Target_Bytes in First .. Last;
                           end;
                        else
                           declare
                              First : constant Buffer_Pos := Tree.Byte_Region
                                (Before_1.Node, Trailing_Non_Grammar => False).Last + 1;
                              Last  : constant Buffer_Pos :=
                                (if After
                                 then Tree.Byte_Region (Before).Last
                                 else Tree.Byte_Region (Before).First - 1);
                           begin
                              exit when Target_Bytes in First .. Last;
                           end;
                        end if;
                        if Terminal_First > Target_Bytes then
                           Before := Before_1;
                           Tree.Prev_Terminal (Before_1);
                        else
                           Before_1 := Before;
                           Tree.Next_Terminal (Before);
                        end if;
                     end loop;
                     return (if After then Before else Before_1);
                  end Find_Element;

                  procedure Restore (I : in Positive_Index_Type)
                  --  Restore Floating_Non_Grammar (I)
                  is
                     Token : Lexer.Token renames Floating_Non_Grammar (I);

                     Containing_Terminal : constant Terminal_Ref := Find_Element
                       (Token.Byte_Region.First, After => False);

                     Old_Token : constant Lexer.Token := Token; -- for trace message

                     Temp_Shift_Lines : Base_Line_Number_Type := Shift_Lines;
                  begin
                     if Token.Byte_Region.First < Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First
                     then
                        --  Only shift if inserted before Terminal. ada_mode-recover_14
                        --
                        --  Ignore this and remaining Floating_Non_Grammar's contribution to
                        --  Shift_Lines; we are inserting it before those. (new_lines in
                        --  Floating_Non_Grammar were previously subtracted from Shift_Lines).
                        --  ada_mode-interactive_01.adb, ada_mode-recover_33.adb,
                        --  ada_mode-recover_extra_end_loop.adb
                        for J in I .. Floating_Non_Grammar.Last_Index loop
                           Temp_Shift_Lines := @ + New_Line_Count (Floating_Non_Grammar (J).Line_Region);
                        end loop;
                        Lexer.Shift (Token, Shift_Bytes, Shift_Chars, Temp_Shift_Lines);
                     end if;

                     Shift_Lines := @ + New_Line_Count (Token.Line_Region);

                     Tree.Non_Grammar_Var (Containing_Terminal.Node).Append (Token);

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("restore floating_non_grammar " & Lexer.Image (Old_Token, Tree.Lexer.Descriptor.all));
                        Tree.Lexer.Trace.Put_Line
                          (" ... to " & Tree.Image (Containing_Terminal, Non_Grammar => True));
                     end if;
                  end Restore;

               begin
                  if Delayed_Scan and then
                    (Delayed_Floating_Index = Positive_Index_Type'Last or
                       Floating_Non_Grammar.First_Index = Delayed_Floating_Index)
                  then
                     null;
                  else
                     for I in Floating_Non_Grammar.First_Index ..
                       (if Delayed_Scan then Delayed_Floating_Index - 1 else Floating_Non_Grammar.Last_Index)
                     loop
                        exit when Floating_Non_Grammar (I).Byte_Region.First > Next_KMN_Stable_Last;
                        --  If token is in next KMN edit region, shift_bytes is wrong here.

                        if Floating_Non_Grammar (I).Byte_Region.First + Shift_Bytes <= Scanned_Byte_Pos then
                           --  Non_grammar token was rescanned; delete the old one.
                           --  test case: ada_mode-recover_align_1.adb, test_incremental.adb Edit_Whitespace

                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("delete floating_non_grammar " & Lexer.Full_Image
                                   (Floating_Non_Grammar (I), Tree.Lexer.Descriptor.all));
                           end if;
                           Last_Handled_Non_Grammar := I;

                        elsif Floating_Non_Grammar (I).Byte_Region.Last <= Next_KMN_Stable_Last then
                           --  Non_Grammar is in next KMN stable region; find terminal to append
                           --  non_grammar to. ada_mode-recover_18.adb
                           Restore (I);
                           Last_Handled_Non_Grammar := I;
                        else
                           exit;
                        end if;
                     end loop;
                  end if;
                  if Last_Handled_Non_Grammar /= Lexer.Token_Arrays.No_Index then
                     if Last_Handled_Non_Grammar = Floating_Non_Grammar.Last_Index then
                        Floating_Non_Grammar.Clear;
                     else
                        Floating_Non_Grammar.Set_First_Last
                          (Last_Handled_Non_Grammar + 1, Floating_Non_Grammar.Last_Index);
                     end if;
                  end if;
               end;
            end;

            KMN_Node := KMN_Lists.Next (KMN_Node);

            if not KMN_Lists.Has_Element (KMN_Node) then
               --  Finally shift EOI.
               pragma Assert
                 (Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID and
                    Tree.Non_Grammar_Const (Terminal.Node).Length = 1); -- EOI_ID

               Tree.Shift
                 (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines, Buffer_Pos'Last, Terminal_Non_Grammar_Next);

               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line ("final shift " & Tree.Image (Terminal, Non_Grammar => True));
               end if;

               exit KMN_Loop;
            end if;
         end;
      end loop KMN_Loop;

      if Tree.ID (Terminal.Node) /= Parser.Tree.Lexer.Descriptor.EOI_ID then
         raise User_Error with "edit list does not cover entire tree";
      end if;

      if not Floating_Non_Grammar.Is_Empty then
         raise SAL.Programmer_Error with "floating_non_grammar not emptied: " & Lexer.Image
           (Floating_Non_Grammar, Tree.Lexer.Descriptor.all);
      end if;

      if Debug_Mode then
         declare
            Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
         begin
            Parser.Tree.Validate_Tree (Parser.User_Data.all, Error_Reported, Node_Index_Order => False);
            if Error_Reported.Count /= 0 then
               if Trace_Incremental_Parse > Outline then
                  Tree.Lexer.Trace.New_Line;
                  Tree.Lexer.Trace.Put_Line ("edit_tree: validate_tree failed; tree:");
                  Tree.Print_Streams (Children => True, Non_Grammar => True);
               end if;
               raise WisiToken.Parse_Error with "edit_tree: validate_tree failed";
            end if;
         end;
      end if;
   end Edit_Tree;

   overriding procedure Parse
     (Shared_Parser    : in out LR.Parser.Parser;
      Recover_Log_File : in     Ada.Text_IO.File_Type;
      Edits            : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited       : in     Boolean        := False)
   is separate;

end WisiToken.Parse.LR.Parser;
