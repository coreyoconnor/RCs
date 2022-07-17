--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Memory;
with WisiToken.Lexer;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
with WisiToken.Parse.LR.McKenzie_Recover.Explore;
with WisiToken.Parse.LR.McKenzie_Recover.Parse;
package body WisiToken.Parse.LR.McKenzie_Recover is

   procedure To_Recover
     (Parser_Stack : in     Syntax_Trees.Stream_ID;
      Tree         : in     Syntax_Trees.Tree;
      Stack        : in out Recover_Stacks.Stack;
      Input_Stream : in out Bounded_Streams.List)
   is
      Parser_Stack_Depth : constant SAL.Peek_Type := Tree.Stack_Depth (Parser_Stack);
   begin
      pragma Assert (Stack.Depth = 0);
      if Stack.Size < Parser_Stack_Depth then
         raise SAL.Programmer_Error with "recover stack needs more space;" & Parser_Stack_Depth'Image;
      end if;
      for I in reverse 1 .. Parser_Stack_Depth loop
         declare
            Element : constant Syntax_Trees.Stream_Index  := Tree.Peek (Parser_Stack, I);
            Node    : constant Syntax_Trees.Node_Access   := Tree.Get_Node (Parser_Stack, Element);
            Token   : constant Syntax_Trees.Recover_Token :=
              (if I = Parser_Stack_Depth
               then (others => <>)
               else Tree.Get_Recover_Token ((Parser_Stack, Element, Node)));
         begin
            Stack.Push ((Tree.State (Parser_Stack, Element), Token));
         end;
      end loop;

      if Tree.Stream_Input_Length (Parser_Stack) > 0 then
         --  Parse stream input has tokens from breakdown of a nonterm in
         --  Shared_Stream, or an error token.
         declare
            use Syntax_Trees;
            Index : Stream_Index := Tree.Stream_Next (Parser_Stack, Tree.Stack_Top (Parser_Stack));
         begin
            loop
               exit when Index = Invalid_Stream_Index;
               Input_Stream.Append (Tree.Get_Node (Parser_Stack, Index));
               Index := Tree.Stream_Next (Parser_Stack, Index);
            end loop;
         end;
      end if;
   end To_Recover;

   procedure Recover_Init
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Arrays;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Config     : Configuration;
      Error_Ref  : constant Syntax_Trees.Stream_Error_Ref  := Parser_State.Current_Error_Ref (Tree);
      Error      : constant Syntax_Trees.Error_Data'Class  := Syntax_Trees.Error (Error_Ref);
      Error_Node : constant Syntax_Trees.Valid_Node_Access := Syntax_Trees.Error_Node (Error_Ref);
   begin
      Parser_State.Recover.Enqueue_Count := @ + 1;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser " & Tree.Trimmed_Image (Parser_State.Stream) &
              ": State" & Tree.State (Parser_State.Stream)'Image &
              " Current_Token " & Tree.Image
                (Tree.Current_Token (Parser_State.Stream), Terminal_Node_Numbers => True));
         Trace.Put_Line
           (if Error in Parse_Error
            then "Parser_Action"
            elsif Error in In_Parse_Action_Error
            then "In_Parse_Action, " &
              Tree.Image (Tree.Stack_Top (Parser_State.Stream)) & " " &
              Error.Image (Tree, Error_Node)
            else raise SAL.Programmer_Error);
         if Trace_McKenzie > Detail then
            Trace.Put_Line ("parse stream:");
            Trace.Put_Line
              (Tree.Image
                 (Parser_State.Stream,
                  Children    => Trace_McKenzie > Extra,
                  Shared      => True,
                  Non_Grammar => True));
         end if;
         if Trace_McKenzie > Extra then
            Trace.New_Line;
         end if;
      end if;

      declare
         use Syntax_Trees;
         First_Current : constant Node_Access := Tree.First_Terminal
           (Tree.Current_Token (Parser_State.Stream).Node);
      begin
         if Tree.Shared_Token (Parser_State.Stream) = Invalid_Stream_Node_Ref then
            --  test_incremental.adb Preserve_Parse_Errors_1; EOI has error
            pragma Assert (First_Current /= Invalid_Node_Access);
            Config.Current_Shared_Token := Invalid_Stream_Node_Ref;
         else
            Config.Current_Shared_Token := Tree.First_Terminal_In_Node
              (Tree.Shared_Token (Parser_State.Stream));
         end if;

         declare
            Seq : constant Base_Sequential_Index := Tree.Get_Sequential_Index
              (if First_Current /= Invalid_Node_Access
               then First_Current
               else Config.Current_Shared_Token.Node);
         begin
            Config.Resume_Token_Goal :=
              (if Seq = Invalid_Sequential_Index then 0 else Seq) + -- Invalid on empty source text.
              Shared_Parser.Table.McKenzie_Param.Check_Limit;
         end;
      end;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      Config.Input_Stream.Initialize;
      To_Recover (Parser_State.Stream, Tree, Config.Stack, Config.Input_Stream);

      if Error in Parse_Error then
         Config.Error_Token := Tree.Get_Recover_Token (Error_Node);

         if Trace_McKenzie > Detail then
            Put ("enqueue", Tree, Parser_State.Stream, Config);
         end if;

      elsif Error in In_Parse_Action_Error then
         if Shared_Parser.Language_Fixes = null then
            --  The only fix is to ignore the error.
            if Trace_McKenzie > Detail then
               Config.Strategy_Counts (Ignore_Error) := 1;
               Put ("enqueue", Tree, Parser_State.Stream, Config);
            end if;

         else
            --  Undo the reduction that encountered the error, let Process_One
            --  enqueue possible solutions. We leave the cost at 0, since this is
            --  the root config. Later logic will enqueue the 'ignore error'
            --  solution; see McKenzie_Recover.Explore Process_One.

            --  Undo_Reduce can be invalid here; see ada-mode/test/ada_mode-recover_27.adb
            if Undo_Reduce_Valid (Super, Shared_Parser, Config) then
               Config.In_Parse_Action_Status := In_Parse_Action_Error (Error).Status;
               Config.Error_Token            := Config.Stack.Peek.Token;

               Unchecked_Undo_Reduce (Super, Shared_Parser, Config);

               Config.In_Parse_Action_Token_Count := Element (Config.Ops, Last_Index (Config.Ops)).Token_Count;

               if Trace_McKenzie > Detail then
                  Put
                    ("undo_reduce " & Image
                       (Tree.Element_ID (Config.Error_Token), Tree.Lexer.Descriptor.all),
                       Tree, Parser_State.Stream, Config);
               end if;
            else
               --  Ignore error
               if Trace_McKenzie > Detail then
                  Config.Strategy_Counts (Ignore_Error) := 1;
                  Put ("enqueue", Tree, Parser_State.Stream, Config);
               end if;
            end if;
         end if;

      else
         raise SAL.Programmer_Error;
      end if;

      Parser_State.Recover.Config_Heap.Add (Config);
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      Tree    : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Trace   : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Initial_Memory_Use : constant GNATCOLL.Memory.Watermark_Info := GNATCOLL.Memory.Get_Ada_Allocations;

      Skip_Next : Boolean := False;

      Super : Base.Supervisor (Parser_Count => Shared_Parser.Parsers.Count);

   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
      end if;

      Super.Initialize (Shared_Parser);

      for Parser_State of Parsers loop
         Recover_Init (Super, Shared_Parser, Parser_State);
      end loop;

      loop
         exit when Super.Done;
         Explore.Process_One (Super, Shared_Parser);
      end loop;

      Super.Finish (Shared_Parser);

      if Trace_Memory > Outline then
         declare
            use GNATCOLL.Memory;
            Memory_Use : constant Watermark_Info := Get_Ada_Allocations;
         begin
            Trace.Put_Line
              ("recover memory use: high" & Byte_Count'Image (Memory_Use.High - Initial_Memory_Use.High) &
                 " current" & Byte_Count'Image (Memory_Use.Current - Initial_Memory_Use.Current));
         end;
      end if;

      --  Spawn new parsers for multiple solutions.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course.
      --
      --  We could try to check here for redundant solutions; configs for a
      --  parser that have the same or "equivalent" ops. But those will be
      --  caught in the main parse by the check for duplicate state; doing
      --  the same check here is premature optimization.
      declare
         use Parser_Lists;

         Cur         : Cursor             := Parsers.First;
         Solutions   : SAL.Base_Peek_Type := 0;
         Spawn_Limit : SAL.Base_Peek_Type := Shared_Parser.Table.Max_Parallel;
      begin
         for Parser of Parsers loop
            if Parser.Recover.Success then
               Solutions := Solutions + Parser.Recover.Results.Count;
            end if;
         end loop;

         if Solutions > Shared_Parser.Table.Max_Parallel and Trace_McKenzie > Outline then
            Trace.Put_Line ("too many parallel parsers required in recover; dropping some solutions");
            Spawn_Limit := Shared_Parser.Table.Max_Parallel / Parsers.Count;
         end if;

         loop
            declare
               Data : McKenzie_Data renames Cur.State_Ref.Recover;
            begin
               if Data.Success then
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (" " & Tree.Trimmed_Image (Cur.Stream) &
                          ": succeed" & SAL.Base_Peek_Type'Image (Data.Results.Count) &
                          ", enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost: " & Integer'Image (Data.Results.Min_Key));
                  end if;

                  if Data.Results.Count > 1 then
                     for I in 1 .. SAL.Base_Peek_Type'Min (Spawn_Limit, Data.Results.Count - 1) loop
                        Parsers.Prepend_Copy
                          (Cur, Tree, Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data), Trace);
                        --  Does not copy recover.

                        if Trace_McKenzie > Outline or Trace_Parse > Outline then
                           Trace.Put_Line
                             ("spawn " & Tree.Trimmed_Image (Parsers.First.Stream) & " from " &
                                Tree.Trimmed_Image (Cur.Stream) & " (" &
                                Trimmed_Image (Integer (Parsers.Count)) &
                                " active)");
                           Put ("", Tree, Parsers.First.Stream, Data.Results.Peek, Strategy => True);
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline or Trace_Parse > Outline then
                     Put ("", Tree, Cur.Stream, Data.Results.Peek, Strategy => True);
                  end if;
               else
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (" " & Tree.Trimmed_Image (Cur.Stream) &
                          ": fail, enqueue" & Integer'Image (Data.Enqueue_Count) &
                          (if Data.Config_Full_Count > 0 then ", config_full" & Data.Config_Full_Count'Image else "") &
                          ", check " & Integer'Image (Data.Check_Count));
                  end if;
               end if;

               Data.Config_Heap.Clear;
            end;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      --  Edit Parser_State to apply solutions.

      --  We don't use 'for Parser_State of Parsers loop' here,
      --  because we might need to terminate a parser.
      declare
         Current_Parser : Parser_Lists.Cursor := Parsers.First;
      begin
         loop
            exit when Current_Parser.Is_Done;

            if Current_Parser.State_Ref.Recover.Success then
               begin
                  --  Can't have active 'renames State_Ref' when terminate a parser
                  declare
                     use Parser_Lists;
                     use Recover_Op_Arrays, Recover_Op_Array_Refs;
                     use Syntax_Trees;

                     Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;

                     Stack  : Stream_ID renames Parser_State.Stream;
                     Result : Configuration renames Parser_State.Recover.Results.Peek;

                     --  We have to use Tree.Update_Error to set components of the current
                     --  error, because different parsers can set different data in the
                     --  same error. We can't save a copy of the current error, because
                     --  Undo_Reduce changes an In_Parse_Action_Error to a Message_Error.
                     --  We keep a local recover_op_nodes here to accumulate ops until
                     --  we've applied all of Result.Ops, to avoid copying the error node
                     --  for each op.
                     Error_Recover_Ops : Recover_Op_Nodes_Arrays.Vector := To_Recover_Op_Nodes (Result.Ops);
                     --  WORKAROUND: GNAT Community 2021 is confused about this being constant
                     pragma Warnings (Off, Error_Recover_Ops);

                     Op_Index : SAL.Base_Peek_Type := No_Insert_Delete;
                     --  Current op in Error_Recover_Ops, for setting Ins_Node, Del_Node.

                     Insert_Delete_Matches_Ops : Boolean := True;
                     --  True if all insert/delete ops are performed here;
                     --  Parser_State.Recover_Insert_Delete_Current is left at No_Element.

                     Stack_Matches_Ops : Boolean := True;
                     First_Insert      : Boolean := True;

                     Last_Recover_Node_Index : Sequential_Index := Sequential_Index'First;

                     Pre_FF_Index : Base_Sequential_Index := Invalid_Sequential_Index;
                     --  The current token sequential_index before a fast_forward, when
                     --  Stack_Matches_Ops is true before the Fast_Forward.

                  begin
                     --  The verb will be reset by the main parser; just indicate the
                     --  parser recovered from the error.
                     Parser_State.Set_Verb (Shift);

                     Parser_State.Set_Current_Error_Features (Tree);

                     pragma Assert (Parser_State.Current_Recover_Op = No_Insert_Delete);

                     Parser_State.Total_Recover_Cost := @ + Result.Cost;
                     Parser_State.Max_Recover_Ops_Length := Ada.Containers.Count_Type'Max
                       (@, Length (Result.Ops));

                     Parser_State.Resume_Token_Goal := Result.Resume_Token_Goal;

                     if Trace_McKenzie > Extra then
                        Put_Line (Tree, Parser_State.Stream, "before Ops applied:");
                        Trace.Put_Line
                          ("   stack/stream:" & ASCII.LF & Tree.Image
                             (Parser_State.Stream, Stack => True, Input => True, Shared => True, Children => True));
                        Trace.Put_Line
                          ("   Shared_Token  " & Tree.Image (Tree.Shared_Token (Parser_State.Stream)));
                        Trace.Put_Line
                          ("   Current_Token " & Tree.Image (Tree.Current_Token (Parser_State.Stream)));
                     end if;

                     --  We don't apply all Ops to the parser stack here, because there can
                     --  be other input tokens between the inserts and deletes, and there
                     --  can be conflicts; we let the main parser handle that. We can apply
                     --  all ops up to the first insert.
                     --
                     --  Other than Add_Terminal, there's no need to modify Tree. Any tree
                     --  nodes created by the failed parse that are pushed back are useful
                     --  for error repair, and will just be ignored in future parsing. This
                     --  also avoids enlarging a non-flushed branched tree, which saves
                     --  time and space.
                     --
                     --  Language_Fixes may abuse the rules about adding Ops, so we check
                     --  that as much as is reasonable here.

                     for I in First_Index (Result.Ops) .. Last_Index (Result.Ops) loop
                        declare
                           Op : Recover_Op renames Constant_Ref (Result.Ops, I);

                           --  We don't declare Current_Token here, because Delete may need to
                           --  delete it.

                           procedure Raise_Bad_Config (Message : in String)
                           is begin
                              if Debug_Mode then
                                 raise SAL.Programmer_Error with Message;
                              end if;

                              if Trace_McKenzie > Outline then
                                 Put_Line (Tree, Parser_State.Stream, Message);
                              end if;
                              raise Bad_Config;
                           end Raise_Bad_Config;
                        begin
                           case Op.Op is
                           when Fast_Forward =>
                              declare
                                 Current_Token_Node : constant Node_Access := Tree.Current_Token
                                   (Parser_State.Stream).Node;
                              begin
                                 if Stack_Matches_Ops then
                                    if Op.FF_Next_Index = Tree.Get_Sequential_Index
                                      (Tree.First_Sequential_Terminal (Current_Token_Node))
                                    then
                                       --  Fast_Forward is a noop. test_mckenzie_recover String_Quote_5.
                                       null;

                                    else
                                       if Tree.Label (Current_Token_Node) = Nonterm then
                                          declare
                                             Target : Stream_Node_Parents;

                                             procedure Find_FF_Target (Ref : in Stream_Node_Ref)
                                             is begin
                                                Target := Tree.To_Stream_Node_Parents (Ref);
                                                Tree.First_Sequential_Terminal (Target, Following => True);
                                                loop
                                                   exit when Tree.Get_Sequential_Index (Target.Ref.Node) =
                                                     Op.FF_Next_Index;
                                                   Tree.Next_Sequential_Terminal (Target, Following => True);
                                                end loop;
                                             end Find_FF_Target;

                                          begin
                                             Find_FF_Target (Tree.Current_Token (Parser_State.Stream));

                                             if Tree.First_Terminal
                                               (Tree.Get_Node (Target.Ref.Stream, Target.Ref.Element)) /=
                                               Target.Ref.Node
                                             then
                                                --  Target is a nonterm that should not be shifted as a whole
                                                --  (otherwise FF index would be after Target), so break it down.
                                                --  ada_mode-recover_bad_char.adb
                                                if Target.Ref.Stream = Tree.Shared_Stream then
                                                   --  First we need to move all tokens Shared_Token .. Target
                                                   --  to the input stream. ada_mode-recover_10.adb
                                                   pragma Assert
                                                     (Tree.Shared_Token (Parser_State.Stream) =
                                                        Tree.Current_Token (Parser_State.Stream));

                                                   Tree.Move_Shared_To_Input
                                                     (First  => Tree.Current_Token (Parser_State.Stream),
                                                      Last   => Target.Ref,
                                                      Stream => Parser_State.Stream);

                                                   Find_FF_Target (Tree.First_Input (Parser_State.Stream));
                                                end if;
                                                Tree.Breakdown
                                                  (Target, Shared_Parser.Productions,
                                                   Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data),
                                                   First_Terminal => True);
                                             end if;
                                          end;
                                       end if;

                                       --  The parser would do shifts and reduces for the tokens we are
                                       --  skipping here
                                       Stack_Matches_Ops := False;

                                       Pre_FF_Index := Tree.Get_Sequential_Index
                                         (Tree.First_Sequential_Terminal (Current_Token_Node));
                                    end if;

                                 else
                                    Pre_FF_Index := Invalid_Sequential_Index;
                                 end if;
                              end;

                           when Undo_Reduce =>
                              --  If Stack_Matches_Ops, we must do the Stack.Pop and Pushes, and we
                              --  can use Stack.Peek to check if the Undo_Reduce is valid.
                              --
                              --  If not Stack_Matches_Ops, we have to assume Op.UR_Token_Index is correct.
                              --
                              --  See test_mckenzie_recover.adb Extra_Begin for an example of Undo_Reduce
                              --  after other ops.
                              --
                              --  We can't use McKenzie_Recover.Undo_Reduce_Valid here; that takes a
                              --  Config stack, not a parser stack. So we duplicate part of it.
                              if Stack_Matches_Ops then
                                 if not (Nonterm = Tree.Label (Tree.Peek (Stack)) and
                                           Op.Nonterm = Tree.ID (Parser_State.Stream, Tree.Peek (Stack)))
                                 then
                                    Raise_Bad_Config ("Undo_Reduce does not match stack top in apply config");
                                 end if;
                              end if;

                              if Stack_Matches_Ops then
                                 Parser_State.Undo_Reduce
                                   (Tree, Shared_Parser.Table.all,
                                    Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));
                              end if;

                           when Push_Back =>
                              --  If Stack_Matches_Ops, we must do the Tree.Push_Back.

                              --  If not Stack_Matches_Ops, we assume Push_Back_Valid ensures that
                              --  the Push_Back is indeed valid here, so the main parser will not
                              --  encounter an error; test_mckenzie_recover.adb Error_3.

                              if Stack_Matches_Ops then
                                 if not (Op.PB_ID = Tree.ID (Parser_State.Stream, Tree.Peek (Stack))) then
                                    Raise_Bad_Config
                                      ("Push_Back does not match stack top in apply config: " &
                                         Image (Op, Tree.Lexer.Descriptor.all));
                                 end if;
                                 Tree.Push_Back (Parser_State.Stream);
                              else
                                 pragma Assert (I > 1); --  else stack_matches_ops is true.
                                 declare
                                    Prev_Op : Recover_Op renames Constant_Ref (Result.Ops, I - 1);
                                 begin
                                    if (Prev_Op.Op = Fast_Forward and Op.PB_Token_Index /= Invalid_Sequential_Index)
                                      and then Pre_FF_Index = Op.PB_Token_Index
                                    then
                                       --  This Push_Back exactly cancels the previous Fast_Forward, so we
                                       --  must apply following insert/delete. test_mckenzie_recover.adb
                                       --  Push_Back_2.
                                       Stack_Matches_Ops := True;
                                    end if;
                                 end;
                                 Pre_FF_Index := Invalid_Sequential_Index;
                              end if;

                           when Insert =>
                              Op_Index := @ + 1;
                              if Parser_State.Current_Recover_Op = No_Insert_Delete then
                                 Parser_State.First_Recover_Op;
                              end if;

                              if First_Insert and Op.Ins_Before = Tree.Get_Sequential_Index
                                (Tree.First_Sequential_Terminal (Tree.Current_Token (Parser_State.Stream)).Node)
                              then
                                 --  We need First_Insert here, not just Stack_Matches_Ops, when the
                                 --  first insert is preceeded only by Push_Back and Undo_Reduce, with
                                 --  at least one Undo_Reduce (so Stack_Matches_Ops is False when we
                                 --  get here). See test_mckenzie_recover.adb Missing_Name_3

                                 First_Insert := False;

                                 Error_Recover_Ops (Op_Index).Ins_Node :=
                                   Tree.Insert_Virtual_Terminal (Parser_State.Stream, Op.Ins_ID).Node;
                                 --  Modifies Tree.Current_Token

                                 Parser_State.Next_Recover_Op (Tree);

                                 --  Normally Insert is completed by Stack.Push; we let the main parser
                                 --  do that.
                                 Stack_Matches_Ops := False;

                              else
                                 Insert_Delete_Matches_Ops := False;
                              end if;

                           when Delete =>
                              Op_Index := @ + 1;
                              if Parser_State.Current_Recover_Op = No_Insert_Delete then
                                 Parser_State.First_Recover_Op;
                              end if;

                              if Op.Del_Token_Index < Last_Recover_Node_Index then
                                 Raise_Bad_Config ("Delete is out of order");
                              end if;
                              Last_Recover_Node_Index := Op.Del_Token_Index;

                              --  We have to apply more than one delete here if they are
                              --  consecutive (for example, ada_mode-recover_extra_end_loop.adb
                              --  deletes "end loop ;"), because the main parser expects
                              --  Current_Token to be correct before checking for
                              --  Delete on return from Recover.
                              declare
                                 Deleted_Node : constant Valid_Node_Access := Tree.First_Terminal
                                   (Tree.Current_Token (Parser_State.Stream)).Node;
                              begin
                                 if Stack_Matches_Ops and Insert_Delete_Matches_Ops and
                                   Op.Del_Token_Index = Tree.Get_Sequential_Index (Deleted_Node)
                                 then
                                    Parser_State.Next_Recover_Op (Tree);

                                    declare
                                       --  WORKAROUND: GNAT Community 2021 reports "'Op' must be a variable"
                                       --  if we use this expression for the Op parameter.
                                       Op : Recover_Op_Nodes renames Error_Recover_Ops (Op_Index);
                                    begin
                                       Parser_State.Do_Delete
                                         (Tree, Op,
                                          User_Data_Access_Constant (Shared_Parser.User_Data));
                                    end;

                                 else
                                    Insert_Delete_Matches_Ops := False;
                                 end if;
                              end;
                           end case;
                        end;
                     end loop;

                     declare
                        Err : Error_Data'Class := Syntax_Trees.Error (Parser_State.Current_Error_Ref (Tree));
                     begin
                        Recover_Op_Array_Var_Ref (Err) := Error_Recover_Ops;

                        if Test_McKenzie_Recover then
                           Recover_Test_Var_Ref (Err) := new Recover_Test_Info'
                             (Ops           => Result.Ops,
                              Cost          => Result.Cost,
                              Enqueue_Count => Parser_State.Recover.Enqueue_Count,
                              Check_Count   => Parser_State.Recover.Check_Count);
                        end if;

                        Parser_State.Update_Error (Tree, Err, User_Data_Access_Constant (Shared_Parser.User_Data));
                     end;

                     if Trace_McKenzie > Extra then
                        Put_Line (Tree, Parser_State.Stream, "after Ops applied:");
                        Trace.Put_Line
                          ("   stack/stream:" & ASCII.LF & Tree.Image
                             (Parser_State.Stream, Stack => True, Input => True, Shared => True, Children => True));
                        Trace.Put_Line
                          ("   Shared_Token  " & Tree.Image (Tree.Shared_Token (Parser_State.Stream)));
                        Trace.Put_Line
                          ("   Current_Token " & Tree.Image (Tree.Current_Token (Parser_State.Stream)));
                        Trace.Put_Line
                          ("   remaining recover_insert_delete " & Parser_State.Recover_Image
                             (Tree, Current_Only => True));
                        Trace.Put_Line ("   resume_token_goal" & Parser_State.Resume_Token_Goal'Image);
                     end if;

                     Parser_State.Total_Recover_Cost := @ + Parser_State.Recover.Results.Min_Key;
                     Parser_State.Recover.Results.Clear;
                  end;
               exception
               when Invalid_Case =>
                  Parsers.Terminate_Parser (Current_Parser, Tree, "invalid config in recover", Trace);
                  --  Terminate advances Current_Parser
                  Skip_Next := True;

                  if Parsers.Count = 0 then
                     --  Oops. Just give up.
                     return Fail_Programmer_Error;
                  end if;

               when E : Bad_Config =>
                  if Debug_Mode then
                     Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
                  end if;

                  Parsers.Terminate_Parser (Current_Parser, Tree, "bad config in recover", Trace);
                  --  Terminate advances Current_Parser
                  Skip_Next := True;

                  if Parsers.Count = 0 then
                     --  Oops. Just give up.
                     return Fail_Programmer_Error;
                  end if;
               end;

            else
               --  Recover failed for this parser. Clear any previous recover
               --  information on the error that triggered error recovery.
               declare
                  use Syntax_Trees;
                  Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                  Error_Ref    : constant Stream_Error_Ref := Parser_State.Current_Error_Ref (Tree);
                  Error        : Error_Data'Class := Syntax_Trees.Error (Error_Ref);
                  Recover_Ops  : Recover_Op_Nodes_Arrays.Vector renames Recover_Op_Array_Var_Ref (Error);
               begin
                  Parser_State.Recover.Results.Clear;
                  Recover_Ops := Recover_Op_Nodes_Arrays.Empty_Vector;
                  Tree.Update_Error
                    (Parser_State.Stream, Error_Ref, Error,
                     User_Data_Access_Constant (Shared_Parser.User_Data));
               end;
            end if;
            if Skip_Next then
               Skip_Next := False;
            else
               Current_Parser.Next;
            end if;
         end loop;
      end;

      return Super.Recover_Result;

   exception
   when E : others =>
      Clear_Sequential_Index (Shared_Parser);
      if Debug_Mode then
         Trace.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
         raise SAL.Programmer_Error;
      else
         return Fail_Programmer_Error;
      end if;
   end Recover;

   ----------
   --  Spec private subprograms; for child packages. Declaration order

   function Peek_Sequential_Start
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in     Configuration)
     return Peek_Sequential_State
   is
      use all type WisiToken.Syntax_Trees.Stream_Node_Ref;
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      return State : Peek_Sequential_State (Config.Input_Stream'Access) do
         Parse.First_Sequential_Terminal (Super, Shared_Parser, State.Input_Terminal);

         if Config.Current_Shared_Token = Syntax_Trees.Invalid_Stream_Node_Ref then
            --  test_incremental.adb Preserve_Parse_Errors_1; EOI has error
            State.Sequential_Terminal := Syntax_Trees.Invalid_Stream_Node_Parents;
            --  This should be the only time we set State.Sequential_Terminal
            --  Invalid.
         else
            State.Sequential_Terminal := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);
            if Syntax_Trees.Rooted (State.Sequential_Terminal.Ref) or
              State.Sequential_Terminal.Ref.Node = Syntax_Trees.Invalid_Node_Access
              --  Ref is an empty nonterm. ada_mode-interactive_03.adb
            then
               Tree.First_Terminal (State.Sequential_Terminal, Following => True);
               Super.Extend_Sequential_Index
                 (Shared_Parser, Thru => State.Sequential_Terminal.Ref.Node, Positive => True);
               Tree.First_Sequential_Terminal (State.Sequential_Terminal, Following => True);
               pragma Assert (State.Sequential_Terminal.Ref /= Syntax_Trees.Invalid_Stream_Node_Ref);
            end if;
         end if;
      end return;
   end Peek_Sequential_Start;

   function Peek_Sequential_Terminal (State : in Peek_Sequential_State) return Syntax_Trees.Node_Access
   is begin
      if State.Input_Terminal.Node = Syntax_Trees.Invalid_Node_Access then
         return State.Sequential_Terminal.Ref.Node;

      else
         return State.Input_Terminal.Node;
      end if;
   end Peek_Sequential_Terminal;

   procedure Peek_Next_Sequential_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      State  : in out Peek_Sequential_State)
   is
      use Syntax_Trees;
   begin
      if State.Input_Terminal.Node = Invalid_Node_Access then
         Tree.Next_Sequential_Terminal (State.Sequential_Terminal, Following => True);

      else
         Parse.Next_Sequential_Terminal (Tree, State.Input_Terminal);

         --  if State.Input_Terminal.Node = Invalid_Node_Access then
         --  State.Sequential_Terminal is correct.
      end if;
   end Peek_Next_Sequential_Terminal;

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID; Descriptor : in WisiToken.Descriptor)
   is begin
      if ID /= Expected_ID then
         raise Bad_Config with "expected " & Image (Expected_ID, Descriptor) & " found " & Image (ID, Descriptor);
      end if;
   end Check;

   procedure Delete_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Node          : in     Syntax_Trees.Valid_Node_Access;
      Expected_ID   : in     Token_ID)
   is begin
      Super.Extend_Sequential_Index (Shared_Parser, Thru => Node, Positive => True);
      declare
         use Recover_Op_Arrays;
         Op : constant Recover_Op :=
           (Delete,
            (if Expected_ID = Invalid_Token_ID
             then Shared_Parser.Tree.ID (Node)
             else Expected_ID),
            Shared_Parser.Tree.Get_Sequential_Index (Node));
      begin
         if Expected_ID /= Invalid_Token_ID then
            Check (Shared_Parser.Tree.ID (Node), Expected_ID, Shared_Parser.Tree.Lexer.Descriptor.all);
         end if;
         if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
            raise Bad_Config;
         end if;
         Append (Config.Ops, Op);
         Append (Config.Insert_Delete, Op);
         Config.Current_Insert_Delete := 1;
      end;
   end Delete_Check;

   procedure Delete_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      ID            : in     Token_ID)
   is
      Node : constant Syntax_Trees.Valid_Node_Access := Parse.Peek_Current_First_Terminal (Shared_Parser.Tree, Config);
   begin
      Delete_Check (Super, Shared_Parser, Config, Node, ID);
   end Delete_Check;

   procedure Delete_Check
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in out Configuration;
      IDs           :         in     Token_ID_Array)
   is
      State : Peek_Sequential_State := Peek_Sequential_Start (Super, Shared_Parser, Config);
   begin
      for ID of IDs loop
         Delete_Check (Super, Shared_Parser, Config, Peek_Sequential_Terminal (State), ID);
         Peek_Next_Sequential_Terminal (Shared_Parser.Tree, State);
      end loop;
   end Delete_Check;

   procedure Delete_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Peek_State    : in out Peek_Sequential_State;
      ID            : in     Token_ID)
   is begin
      Delete_Check (Super, Shared_Parser, Config, Peek_Sequential_Terminal (Peek_State), ID);
      Peek_Next_Sequential_Terminal (Shared_Parser.Tree, Peek_State);
   end Delete_Check;

   procedure Do_Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use Syntax_Trees;
      Token : constant Recover_Token := Config.Stack.Pop.Token;
   begin
      Recover_Op_Arrays.Append
        (Config.Ops, (Push_Back, Tree.Element_ID (Token), Tree.Get_Sequential_Index (Tree.First_Terminal (Token))));

      if Token.Virtual then
         if Token.First_Terminal = Invalid_Node_Access then
            --  Token is an empty nonterm. Doing nothing is ok; the empty nonterm
            --  will be created by the parse process.
            null;
         else
            case Tree.Label (Token.First_Terminal) is
            when Terminal_Label =>
               Config.Input_Stream.Prepend (Token.First_Terminal);
            when Nonterm =>
               raise SAL.Programmer_Error;
            end case;
         end if;
      else
         Config.Input_Stream.Prepend (Token.Element_Node);
      end if;
   end Do_Push_Back;

   procedure Set_Initial_Sequential_Index
     (Parsers    : in out Parser_Lists.List;
      Tree       : in     Syntax_Trees.Tree;
      Streams    : in out     Syntax_Trees.Stream_ID_Array;
      Terminals  : in out Syntax_Trees.Stream_Node_Parents_Array;
      Initialize : in     Boolean)
   is
      use Syntax_Trees;

      --  The parsers may have different error points, and different parse
      --  stream input after the error point; we arbitrarily pick the last
      --  terminal in the first parser stack top as the origin for
      --  Sequential_Index. Because most terminal nodes are shared, we must
      --  set Sequential_Index consistently for all parsers, including in
      --  terminal tokens copied from Shared_Stream (for Set_Error or
      --  Add_Deleted). So we walk prev/next terminal for each parser.

      --  If not Initialize, we are clearing sequential_index. A node may
      --  have been copied from shared_stream into a parse stream after
      --  sequential_index was initialized, to set Deleted_After due to a
      --  Delete op (test_mckenzie_recover.adb Error_2). So we need to
      --  traverse the shared stream as well as the parse streams.

      Seq_Index : constant Base_Sequential_Index := (if Initialize then 1 else Invalid_Sequential_Index);
      I : Positive_Index_Type := 1; --  first parse stream
   begin
      --  First set starting point.
      for Parser_State of Parsers loop
         Streams (I) := Parser_State.Stream;

         --  Start with the stack top; it is the error token for
         --  In_Parse_Actions, is one stream element before the error token for
         --  Parse_Actions, is in the parse stream, and is SOI for an empty
         --  buffer. It may be a nonterm, possibly empty.
         Terminals (I) := Tree.To_Stream_Node_Parents
           (Tree.To_Rooted_Ref (Parser_State.Stream, Tree.Peek (Parser_State.Stream)));

         if not Initialize and I = 1 then
            --  Set Terminals (Terminals'Last), which must be in the shared stream.
            --  Current_Token is not invalid here; error recover succeeded.
            declare
               Current_Token : Rooted_Ref renames Tree.Current_Token (Parser_State.Stream);
            begin
               if Tree.ID (Current_Token.Node) = Tree.Lexer.Descriptor.EOI_ID then
                  --  test_mckenzie_recover.adb Empty_Comments
                  Terminals (Terminals'Last) := Tree.To_Stream_Node_Parents
                    (Tree.To_Rooted_Ref (Tree.Shared_Stream, Tree.Stream_Last (Tree.Shared_Stream, Skip_EOI => False)));
               else
                  if Current_Token.Stream /= Tree.Shared_Stream then
                     --  Current_Token is the error token, so it was copied to the parse
                     --  stream input. test_mckenzie_recover.adb Error_4.
                     Terminals (Terminals'Last) := Tree.To_Stream_Node_Parents
                       (Tree.To_Rooted_Ref
                          (Parser_State.Stream, Tree.Stream_Last (Parser_State.Stream, Skip_EOI => False)));
                     Tree.Next_Terminal (Terminals (Terminals'Last), Following => True);
                     if Terminals (Terminals'Last).Ref = Invalid_Stream_Node_Ref then
                        --  EOI was in stream input
                        Terminals (Terminals'Last) := Tree.To_Stream_Node_Parents
                          (Tree.To_Rooted_Ref
                             (Tree.Shared_Stream, Tree.Stream_Last (Tree.Shared_Stream, Skip_EOI => False)));
                     end if;
                     pragma Assert
                       (Terminals (Terminals'Last).Ref.Stream = Tree.Shared_Stream and
                          (Terminals (Terminals'Last).Ref.Node /= Invalid_Node_Access and then
                             Tree.Label (Terminals (Terminals'Last).Ref.Node) in Terminal_Label));

                  else
                     Terminals (Terminals'Last) := Tree.To_Stream_Node_Parents (Current_Token);
                     Tree.Last_Terminal (Terminals (Terminals'Last), Streams (Terminals'Last), Preceding => True);
                  end if;
               end if;
            end;
         end if;

         Tree.Last_Terminal (Terminals (I), Streams (I), Preceding => True);
         I := @ + 1;
      end loop;

      if not Initialize then
         Streams (Streams'Last) := Tree.Shared_Stream;
      end if;

      --  Get all Terminals to the same node. Terminals (1) is the
      --  "reference" terminal.
      for I in Terminals'First + 1 .. Terminals'Last loop
         if Terminals (I).Ref.Node /= Terminals (1).Ref.Node then
            --  There are several cases:
            --
            --  1. I node is copied
            --  2. Reference node is before or after I node.
            --  3. Reference node is deleted in I.
            --
            --  In case 3, the I node does not need Sequential_Index.
            --
            --  Note that the reference node cannot be inserted or deleted in the
            --  reference parser, because we start with stack_top,
            --  which is after any deleted tokens.

            declare
               Ref_Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
                 (Terminals (1), Parse_Stream => Streams (1), Trailing_Non_Grammar => True).First;
            begin
               loop
                  declare
                     Byte_Pos : Buffer_Pos := Tree.Byte_Region
                       (Terminals (I), Streams (I), Trailing_Non_Grammar => True).First;
                  begin
                     if Ref_Byte_Pos = Byte_Pos then
                        --  case 1.
                        Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Seq_Index);
                        exit;

                     elsif Ref_Byte_Pos < Byte_Pos then
                        if Tree.ID (Terminals (I).Ref.Node) = Tree.Lexer.Descriptor.SOI_ID then
                           Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Seq_Index);
                           exit;
                        else
                           Tree.Prev_Terminal (Terminals (I), Streams (I), Preceding => True);

                           Byte_Pos := Tree.Byte_Region
                             (Terminals (I), Parse_Stream => Streams (I), Trailing_Non_Grammar => True).First;

                           exit when Ref_Byte_Pos > Byte_Pos; -- case 3.
                        end if;
                     else
                        if Tree.ID (Terminals (I).Ref.Node) = Tree.Lexer.Descriptor.EOI_ID then
                           Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Seq_Index);
                           exit;
                        else
                           Tree.Next_Terminal (Terminals (I), Following => True);
                           Byte_Pos := Tree.Byte_Region
                             (Terminals (I), Parse_Stream => Streams (I), Trailing_Non_Grammar => True).First;
                           exit when Ref_Byte_Pos < Byte_Pos; -- case 3.
                        end if;
                     end if;
                  end;
               end loop;
            end;
         end if;
      end loop;

      for I in Terminals'Range loop
         Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Seq_Index);
      end loop;
   end Set_Initial_Sequential_Index;

   procedure Extend_Sequential_Index
     (Tree      : in     Syntax_Trees.Tree;
      Streams   : in     Syntax_Trees.Stream_ID_Array;
      Terminals : in out Syntax_Trees.Stream_Node_Parents_Array;
      Target    : in     Syntax_Trees.Base_Sequential_Index;
      Positive  : in     Boolean;
      Clear     : in     Boolean)
   is
      use Syntax_Trees;
      Index : Base_Sequential_Index :=
        (if Clear
         then Invalid_Sequential_Index
         elsif Tree.Get_Sequential_Index (Terminals (1).Ref.Node) /= Invalid_Sequential_Index
         then Tree.Get_Sequential_Index (Terminals (1).Ref.Node)
         else 1);

      Skip_Step_Reference : Boolean := False;
      Skip_Step_Terminals : array (2 .. Terminals'Last) of Boolean := (others => False);
      Target_Seen : array (1 .. Terminals'Last) of Boolean := (others => False);
   begin
      loop
         if Skip_Step_Reference then
            Skip_Step_Reference := False;
         else
            if Positive then
               if Tree.ID (Terminals (1).Ref.Node) = Tree.Lexer.Descriptor.EOI_ID then
                  Target_Seen (1) := True;
               else
                  Tree.Next_Terminal (Terminals (1), Following => True);
                  if Tree.Get_Sequential_Index (Terminals (1).Ref.Node) /= Invalid_Sequential_Index and
                    Tree.Get_Sequential_Index (Terminals (1).Ref.Node) >= Target
                  then
                     Target_Seen (1) := True;
                  end if;
               end if;
            else
               if Tree.ID (Terminals (1).Ref.Node) = Tree.Lexer.Descriptor.SOI_ID then
                  Target_Seen (1) := True;
               else
                  Tree.Prev_Terminal (Terminals (1), Streams (1), Preceding => True);
                  if Tree.Get_Sequential_Index (Terminals (1).Ref.Node) /= Invalid_Sequential_Index and
                    Tree.Get_Sequential_Index (Terminals (1).Ref.Node) <= Target
                  then
                     Target_Seen (1) := True;
                  end if;
               end if;
            end if;
         end if;

         if not Clear  then
            if Positive then
               Index := @ + 1;
            else
               Index := @ - 1;
            end if;
         end if;

         declare
            Ref_Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
              (Terminals (1), Streams (1), Trailing_Non_Grammar => True).First;
         begin
            for I in 2 .. Terminals'Last loop
               if Skip_Step_Terminals (I) then
                  Skip_Step_Terminals (I) := False;
               else
                  if Positive then
                     if Tree.ID (Terminals (I).Ref.Node) = Tree.Lexer.Descriptor.EOI_ID then
                        Target_Seen (I) := True;
                     else
                        Tree.Next_Terminal (Terminals (I), Following => True);
                        if Tree.Get_Sequential_Index (Terminals (I).Ref.Node) >= Target then
                           Target_Seen (I) := True;
                        end if;
                     end if;
                  else
                     if Tree.ID (Terminals (I).Ref.Node) = Tree.Lexer.Descriptor.SOI_ID then
                        Target_Seen (I) := True;
                     else
                        Tree.Prev_Terminal (Terminals (I), Streams (I), Preceding => True);
                        if Tree.Get_Sequential_Index (Terminals (I).Ref.Node) <= Target then
                           Target_Seen (I) := True;
                        end if;
                     end if;
                  end if;
               end if;

               declare
                  Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
                    (Terminals (I), Streams (1), Trailing_Non_Grammar => True).First;
               begin
                  if Terminals (I).Ref.Node = Terminals (1).Ref.Node then
                     --  Don't set this here; that will confuse the exit criteria for not
                     --  Initialize. It will be set via Terminals (1) below.
                     null;

                  elsif Ref_Byte_Pos = Byte_Pos then
                     Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Index);

                  else
                     if Positive then
                        if Ref_Byte_Pos > Byte_Pos then
                           --  Parser node is deleted in reference; wait for Parser to catch
                           --  up.
                           Skip_Step_Reference := True;
                           Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Index);

                        else
                           --  Ref_Byte_Pos < Byte_Pos
                           --  Reference node is deleted in Parser; wait for reference to catch up.
                           Skip_Step_Terminals (I) := True;
                        end if;

                     else -- Positive = False
                        if Ref_Byte_Pos < Byte_Pos then
                           --  Parser node is deleted in reference; wait for reference to catch
                           --  up.
                           Skip_Step_Reference := True;
                           Tree.Set_Sequential_Index (Terminals (I).Ref.Node, Index);

                        else
                           --  Ref_Byte_Pos > Byte_Pos
                           --  Reference node is deleted in Parser; wait for reference to catch up.
                           Skip_Step_Terminals (I) := True;
                        end if;
                     end if;
                  end if;
               end;
            end loop;
         end;

         if Clear then
            if (for all Seen of Target_Seen => Seen) and then
              (for all Term of Terminals =>
                 Tree.Label (Term.Ref.Node) = Source_Terminal and then
                   Tree.Get_Sequential_Index (Term.Ref.Node) = Invalid_Sequential_Index)
            then
               exit;
            end if;

            if not Skip_Step_Reference then
               Tree.Set_Sequential_Index (Terminals (1).Ref.Node, Index);
            end if;

         else
            if not Skip_Step_Reference then
               Tree.Set_Sequential_Index (Terminals (1).Ref.Node, Index);
            end if;

            exit when
              (if Positive
               then Index >= Target
               else Index <= Target) and
              (for all Term of Terminals => Tree.Label (Term.Ref.Node) = Source_Terminal and then
                 Tree.Get_Sequential_Index (Term.Ref.Node) /= Invalid_Sequential_Index);

            exit when
              (for all Term of Terminals =>
                 Tree.ID (Term.Ref.Node) =
                   (if Positive
                    then Tree.Lexer.Descriptor.EOI_ID
                    else Tree.Lexer.Descriptor.SOI_ID));
         end if;

      end loop;
   end Extend_Sequential_Index;

   procedure Clear_Sequential_Index (Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser)
   is
      Streams       : Syntax_Trees.Stream_ID_Array (1 .. Shared_Parser.Parsers.Count + 1);
      Min_Terminals : Syntax_Trees.Stream_Node_Parents_Array (1 .. Shared_Parser.Parsers.Count + 1);
      Max_Terminals : Syntax_Trees.Stream_Node_Parents_Array (1 .. Shared_Parser.Parsers.Count + 1);
   begin
      if Shared_Parser.Parsers.Count = 0 then
         --  We get here when recover fails by terminating all parsers.
         return;
      end if;

      Set_Initial_Sequential_Index
        (Shared_Parser.Parsers, Shared_Parser.Tree, Streams, Max_Terminals, Initialize => False);
      Min_Terminals := Max_Terminals;
      Extend_Sequential_Index
        (Shared_Parser.Tree, Streams, Max_Terminals, Positive => True,
         Target => Shared_Parser.Max_Sequential_Index, Clear => True);
      Extend_Sequential_Index
        (Shared_Parser.Tree, Streams, Min_Terminals, Positive => False,
         Target => Shared_Parser.Min_Sequential_Index, Clear => True);

      if Debug_Mode then
         Shared_Parser.Tree.Sequential_Index_Cleared;
      end if;
   end Clear_Sequential_Index;

   function Find_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration;
      ID     : in Token_ID)
     return Boolean
   is begin
      for I in 1 .. Config.Stack.Depth - 1 loop
         --  Depth has Invalid_Token_ID
         if ID = Tree.Element_ID (Config.Stack.Peek (I).Token) then
            return True;
         end if;
      end loop;
      return False;
   end Find_ID;

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Stack_ID : Token_ID renames Tree.Element_ID (Config.Stack.Peek (Matching_Index).Token);
         begin
            exit when Stack_ID = ID;
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            ID : Token_ID renames Tree.Element_ID (Config.Stack.Peek (Matching_Index).Token);
         begin
            exit when ID in IDs'First .. IDs'Last and then IDs (ID);
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_Descendant_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      ID_Set         : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is
      function Found return Boolean
      is
         use Syntax_Trees;
         Token renames Config.Stack.Peek (Matching_Index).Token;
      begin
         return
           Tree.Element_ID (Token) in ID_Set'Range and then
           (ID_Set (Tree.Element_ID (Token)) and
              (not Token.Virtual and then
                 Tree.Find_Descendant (Token.Element_Node, ID) /=
                 Invalid_Node_Access));
      end Found;
   begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         exit when Found;

         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_Descendant_ID;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      loop
         exit when Matching_Name_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Syntax_Trees.Recover_Token renames Config.Stack.Peek (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region := Tree.Name (Token);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Name_Region))
               else Tree.Lexer.Buffer_Text (Name_Region));

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Count := 0;

      loop
         exit when Matching_Name_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Syntax_Trees.Recover_Token renames Config.Stack.Peek (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region := Tree.Name (Token);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Name_Region))
               else Tree.Lexer.Buffer_Text (Name_Region));

            if Other_ID = Tree.Element_ID (Token) then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      ID            : in     Token_ID)
   is begin
      Insert
        (Super, Shared_Parser, Config, Parse.Peek_Current_First_Sequential_Terminal (Super, Shared_Parser, Config), ID);
   end Insert;

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      IDs           : in     Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Super, Shared_Parser, Config, ID);
      end loop;
   end Insert;

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Before        : in     Syntax_Trees.Valid_Node_Access;
      ID            : in     Token_ID)
   is begin
      Super.Extend_Sequential_Index (Shared_Parser, Thru => Before, Positive => True);
      declare
         use Recover_Op_Arrays;
         Op : constant Recover_Op := (Insert, ID, Shared_Parser.Tree.Get_Sequential_Index (Before));
      begin
         if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
            raise Bad_Config;
         end if;
         Append (Config.Ops, Op);
         Append (Config.Insert_Delete, Op);
         Config.Current_Insert_Delete := 1;
      end;
   end Insert;

   function Undo_Reduce_Op_Order_Valid (Ops : in Recover_Op_Arrays.Vector) return Boolean
   --  Subset of checks in Push_Back_Undo_Reduce_Valid, when the target nonterm is empty.
   is
      use Recover_Op_Arrays;
   begin
      declare
         Op : Recover_Op renames Element (Ops, Last_Index (Ops));
      begin
         case Op.Op is
         when Fast_Forward =>
            --  Normally any Undo_Reduce must be done before Insert and after
            --  Delete, to eliminate duplicate results from push_back/reduce
            --  before and after delete (see test_mckenzie_recover.adb
            --  Extra_Begin, ada_mode-recover_extra_end_loop.adb with incremental
            --  parse). Fast_Forward resets that.
            return True;

         when Undo_Reduce | Push_Back =>
            return True;

         when Insert =>
            return False;

         when Delete =>
            return True;
         end case;
      end;
   end Undo_Reduce_Op_Order_Valid;

   function Push_Back_Undo_Reduce_Valid
     (Super                 : in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Target_Op             : in     Recover_Op_Label;
      Target_Node           : in     Syntax_Trees.Node_Access;
      Ops                   : in     Recover_Op_Arrays.Vector;
      Last_Op_Index         : in     Positive_Index_Type;
      Push_Back_Undo_Reduce : in     Boolean)
     return Boolean
   --  Target_Node is the first terminal in a token that is the object of
   --  Target_Op; a Push_Back or Undo_Reduce that will be the next Op
   --  after Last_Op. Return True if that respects restrictions on Op
   --  order.
   --
   --  Language_Fixes may set Push_Back_Undo_Reduce True; other callers
   --  must set it False.
   is
      use Syntax_Trees;
      use Recover_Op_Arrays;

      Tree : Syntax_Trees.Tree  renames Shared_Parser.Tree;

      Target_Index : Base_Sequential_Index :=
        (if Target_Node = Invalid_Node_Access
         then Invalid_Sequential_Index
         else Tree.Get_Sequential_Index (Target_Node));

      Fast_Forward_Seen        : Boolean               := False;
      Fast_Forward_First_Index : Base_Sequential_Index := Invalid_Sequential_Index;

      function Check_Insert_Delete (Op : in Insert_Delete_Op) return Boolean
      is begin
         return Fast_Forward_Seen and then
           (Target_Index = Invalid_Sequential_Index or
              (if Push_Back_Undo_Reduce
               then
                 (case Insert_Delete_Op_Label'(Op.Op) is
                  when Insert => Target_Index >= Op.Ins_Before,
                  when Delete => Target_Index > Op.Del_Token_Index)
               --  allow Language_Fixes (which sets Push_Back_Undo_Reduce) to insert
               --  more ops at a previous error location, but not cross Insert or
               --  Delete; that would cause out-of-order ops. ada_mode-recover_37.adb
               else Target_Index > Fast_Forward_First_Index
               --  test_mckenzie_recover.adb Push_Back_2, String_Quote_7
              ));
      end Check_Insert_Delete;

   begin
      if Target_Index = Invalid_Sequential_Index and Target_Node /= Invalid_Node_Access then
         Super.Extend_Sequential_Index (Shared_Parser, Target_Node, Positive => False);
         Target_Index := Tree.Get_Sequential_Index (Target_Node);
      end if;

      for I in reverse First_Index (Ops) .. Last_Op_Index loop
         declare
            Op : Recover_Op renames Element (Ops, I);
         begin
            case Op.Op is
            when Fast_Forward =>
               --  Normally any Push_Back must be done before any Insert or Delete,
               --  to eliminate duplicate results from push_back/reduce before and
               --  after delete (see test_mckenzie_recover.adb Extra_Begin).
               --  Fast_Forward resets that.
               --
               --  Push_Back/Undo_Reduce into a Fast_Forward region is ok, but not
               --  all of a Fast_Forward; that would just repeat the same ops.
               Fast_Forward_Seen        := True;
               Fast_Forward_First_Index := Op.FF_First_Index;

            when Undo_Reduce =>
               --  We allow mixing push_back and undo_reduce in any order, so we can
               --  get to an arbitrary point inside a nonterm to do insert/delete.

               if Op.UR_Token_Index = Invalid_Sequential_Index then
                  --  Undo_Reduced token was empty; need to see the next one.
                  null;
               else
                  if Target_Index = Invalid_Sequential_Index then
                     --  Target token is empty; it does not cross anything.
                     return True;
                  end if;

                  --  No point in checking Fast_Forward_Seen here; we don't have the
                  --  last terminal index of Op. So this allows Push_Back/Undo_Reduce of the
                  --  entire fast_forward.

                  case Target_Op is
                  when Undo_Reduce =>
                     --  Undo_Reduce after Undo_Reduce; must undo part or all of the original
                     --  nonterm; see test_mckenzie_recover.adb Missing_Name_2.
                     return Target_Index >= Op.UR_Token_Index;

                  when Push_Back =>
                     --  Push_Back after Undo_Reduce; must push back only part of the
                     --  unreduced nonterm, unless overridden by Language_Fixes.
                     --  test/ada_mode-recover_block_name_mismatch.adb
                     return
                       (if Push_Back_Undo_Reduce
                        then True
                        else Target_Index > Op.UR_Token_Index);

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end if;

            when Push_Back =>
               if Op.PB_Token_Index = Invalid_Sequential_Index then
                  --  Pushed_Back token was empty; need to see the next one.
                  null;
               else
                  if Target_Index = Invalid_Sequential_Index then
                     --  Target token is empty; it does not cross anything.
                     return True;
                  end if;

                  case Target_Op is
                  when Undo_Reduce =>
                     if Fast_Forward_Seen then
                        --  Unreducing a token somewhere in the push_back.
                        return Target_Index >= Op.PB_Token_Index;
                     else
                        --  Need to keep going to see if we cross a fast_forward
                        null;
                     end if;

                  when Push_Back =>
                     --  Between Fast_Forwards, successive non-empty Push_Back
                     --  have decreasing targets; see test_mckenzie_recover.adb
                     --  Missing_Name_0.
                     --
                     --  If the target push_back crosses a Fast_Forward, it must not cross
                     --  a preceding op; Target_Index must be >= Op.PB_Token_Index. See
                     --  ada-mode-recover_27.adb.
                     if Fast_Forward_Seen then
                        --  Target push_back/undo_reduce does not cross the previous
                        --  push_back/undo_reduce.
                        return Target_Index > Op.PB_Token_Index;

                     else
                        --  Need to keep going to see if we cross a fast_forward
                        null;
                     end if;

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end if;

            when Insert =>
               return Check_Insert_Delete (Op);

            when Delete =>
               return Check_Insert_Delete (Op);
            end case;
         end;
      end loop;

      --  We get here if we are looking for the next Push_Back or
      --  Undo_Reduce. In effect, Op.*_Token_Index is now 0, which means any
      --  Push_Back or Undo_Reduce is ok.
      return True;
   end Push_Back_Undo_Reduce_Valid;

   function Push_Back_Valid
     (Super                 : in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config                : in     Configuration;
      Push_Back_Undo_Reduce : in     Boolean)
     return Boolean
   is
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      if Config.Stack.Depth <= 1 then
         return False;
      end if;

      declare
         use Syntax_Trees;
         Token : Recover_Token renames Config.Stack.Peek.Token;
         First_Terminal : constant Node_Access := Tree.First_Terminal (Token);
      begin
         return
           --  Push_Back needs a terminal node or an empty nonterm.
           --  ada_mode-recover_38.adb partial parse,
           --  ada_mode-recover_indent_3.adb partial parse.
           (not Token.Virtual or else
              (Token.Virtual and then
                 ((Is_Terminal (Token.ID, Tree.Lexer.Descriptor.all) and
                    Token.First_Terminal /= Invalid_Node_Access) or
                    Is_Nonterminal (Token.ID, Tree.Lexer.Descriptor.all))))

           and then
           (Push_Back_Undo_Reduce or not Tree.Contains_Virtual_Terminal (Token)) and then
           --  Normally, if Contains_Virtual_Terminal, Token was inserted earlier
           --  in this or a previous recover session; no point in recomputing it.
           --  However, Language_Fixes can push back a virtual nonterm in order
           --  to insert something before it; ada_mode-interactive_01.adb

           --  We allow both Push_Back and Undo_Reduce of empty nonterms
           --  (First_Terminal = Invalid_Node_Access); Push_Back is easier to use
           --  in Language_Fixes, Undo_Reduce is required to change the stack
           --  state to allow completing a production with a non-empty nonterm.
           (Recover_Op_Arrays.Length (Config.Ops) = 0 or else
              Push_Back_Undo_Reduce_Valid
                (Super, Shared_Parser,
                 Push_Back,
                 First_Terminal,
                 Config.Ops,
                 Recover_Op_Arrays.Last_Index (Config.Ops),
                 Push_Back_Undo_Reduce));
      end;
   end Push_Back_Valid;

   procedure Push_Back
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Push_Back_Undo_Reduce : in     Boolean)
   is begin
      --  We relax the "don't push back into previous recover" restriction
      --  for Language_Fixes; see test_mckenzie_recover.adb Missing_Name_5.
      if not Push_Back_Valid (Super, Shared_Parser, Config, Push_Back_Undo_Reduce => Push_Back_Undo_Reduce) then
         raise Invalid_Case;
      end if;

      Do_Push_Back (Shared_Parser.Tree, Config);
   end Push_Back;

   procedure Push_Back_Check
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Expected_ID           : in     Token_ID;
      Push_Back_Undo_Reduce : in     Boolean)
   is begin
      Check
        (Shared_Parser.Tree.Element_ID (Config.Stack.Peek (1).Token),
         Expected_ID,
         Shared_Parser.Tree.Lexer.Descriptor.all);
      Push_Back (Super, Shared_Parser, Config, Push_Back_Undo_Reduce);
   end Push_Back_Check;

   procedure Push_Back_Check
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Expected              : in     Token_ID_Array;
      Push_Back_Undo_Reduce : in     Boolean)
   is begin
      for ID of Expected loop
         Push_Back_Check (Super, Shared_Parser, Config, ID, Push_Back_Undo_Reduce);
      end loop;
   end Push_Back_Check;

   procedure Put
     (Message      : in     String;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Config       : in     Configuration;
      Strategy     : in     Boolean := False)
   --  For debugging output
   is
      use Recover_Op_Array_Refs;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type Bounded_Streams.Cursor;
      use all type WisiToken.Syntax_Trees.In_Parse_Actions.Status_Label;
      use all type WisiToken.Syntax_Trees.Recover_Token;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        +" " & Tree.Trimmed_Image (Parser_Label) & ": " & --  leading space for consistency with existing tests.
        (if Message'Length > 0 then Message & ":" else "");
   begin
      Result := Result & Natural'Image (Config.Cost);
      if Strategy or Trace_McKenzie > Extra then
         Result := Result & ", (";
         for C of Config.Strategy_Counts loop
            Result := Result & Integer'Image (C);
         end loop;
         Result := Result & "), ";
      else
         Result := Result & ", ";
      end if;
      if Config.In_Parse_Action_Status.Label /= Ok then
         Result := Result & Config.In_Parse_Action_Status.Label'Image & " ";
      elsif Config.Error_Token /= Syntax_Trees.Invalid_Recover_Token then
         Result := Result & "Error " & Syntax_Trees.Image (Tree, Config.Error_Token) & " ";
      end if;
      Result := Result & Image (Config.Stack, Tree, Depth => 1);

      if Config.Current_Insert_Delete /= No_Insert_Delete then
         Result := Result & "/" & Trimmed_Image (Config.Current_Insert_Delete) & ":" &
           Image (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete), Descriptor) & "/";

      elsif Config.Input_Stream.First /= Bounded_Streams.No_Element then
         Result := Result & "\" & Tree.Image
           (Config.Input_Stream (Config.Input_Stream.First), Node_Numbers => True) & "\";

      else
         Result := Result & "|" & Tree.Image (Config.Current_Shared_Token, Terminal_Node_Numbers => True) & "|";
      end if;

      Result := Result & Image (Config.Ops, Descriptor);
      Tree.Lexer.Trace.Put_Line (-Result);
   end Put;

   procedure Put_Line
     (Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Message      : in     String)
   is begin
      Tree.Lexer.Trace.Put_Line (Tree.Trimmed_Image (Parser_Label) & ": " & Message);
   end Put_Line;

   function Undo_Reduce_Valid
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration)
     return Boolean
   is
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      if Config.Stack.Depth = 0 then
         return False;
      end if;

      declare
         use Recover_Op_Arrays;

         Token : Syntax_Trees.Recover_Token renames Config.Stack.Peek.Token;
      begin
         if Token.Virtual or else not Tree.Is_Nonterm (Token.Element_Node) then
            return False;

         elsif Length (Config.Ops) = 0 then
            return True;

         else
            --  Undo_Reduce needs to know what tokens the nonterm contains, to
            --  push them on the stack. Thus we need a valid Tree first terminal
            --  node, or an empty nonterm.
            return
              (Tree.Child_Count (Token.Node) = 0 and then
                 Undo_Reduce_Op_Order_Valid (Config.Ops))
              or else
              (Push_Back_Undo_Reduce_Valid
                 (Super, Shared_Parser, Undo_Reduce,  Tree.First_Sequential_Terminal (Token.Node), Config.Ops,
                  Last_Index (Config.Ops),
                  Push_Back_Undo_Reduce => False));
         end if;
      end;
   end Undo_Reduce_Valid;

   procedure Unchecked_Undo_Reduce
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration)
   is
      Table        : Parse_Table renames Shared_Parser.Table.all;
      Tree         : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Stack        : Recover_Stacks.Stack renames Config.Stack;
      Nonterm_Item : constant Recover_Stack_Item := Recover_Stacks.Pop (Stack);

      First_Terminal : constant Syntax_Trees.Node_Access := Tree.First_Source_Terminal
        (Nonterm_Item.Token.Element_Node, Trailing_Non_Grammar => False, Following => False);
      --  If First_Terminal (element) is virtual, it might be from current
      --  error recovery, not the shared_stream, so extend_sequential_index
      --  would not give it an index.

      Prev_State : State_Index                             := Stack.Peek.State;
      Children   : constant Syntax_Trees.Node_Access_Array := Tree.Children (Nonterm_Item.Token.Element_Node);
   begin
      --  We don't move an In_Parse_Action from Nonterm to First_Terminal
      --  here, since we are not updating the tree; that's done in Recover
      --  when the recover actions are applied to the parser state.
      for C of Children loop
         if Is_Terminal (Tree.ID (C), Tree.Lexer.Descriptor.all) then
            Prev_State := Shift_State (Action_For (Table, Prev_State, Tree.ID (C)));
         else
            Prev_State := Goto_For (Table, Prev_State, Tree.ID (C));
         end if;
         if Stack.Is_Full then
            raise Bad_Config;
         end if;
         Stack.Push ((Prev_State, Tree.Get_Recover_Token (C)));
      end loop;

      if First_Terminal /= Syntax_Trees.Invalid_Node_Access and then
        Tree.ID (First_Terminal) /= Tree.Lexer.Descriptor.SOI_ID
      then
         Super.Extend_Sequential_Index (Shared_Parser, First_Terminal, Positive => False);
      end if;

      Recover_Op_Arrays.Append
        (Config.Ops,
         (Undo_Reduce, Tree.ID (Nonterm_Item.Token.Element_Node), Children'Length,
         Tree.Get_Sequential_Index (First_Terminal)));
   end Unchecked_Undo_Reduce;

   procedure Undo_Reduce_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Expected      : in     Token_ID)
   is begin
      if not Undo_Reduce_Valid (Super, Shared_Parser, Config) then
         raise Invalid_Case;
      end if;
      Check
        (Shared_Parser.Tree.Element_ID (Config.Stack.Peek (1).Token), Expected,
         Shared_Parser.Tree.Lexer.Descriptor.all);
      Unchecked_Undo_Reduce (Super, Shared_Parser, Config);
   end Undo_Reduce_Check;

   procedure Undo_Reduce_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Expected      : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         Undo_Reduce_Check (Super, Shared_Parser, Config, ID);
      end loop;
   end Undo_Reduce_Check;

end WisiToken.Parse.LR.McKenzie_Recover;
