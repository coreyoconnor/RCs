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

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Bounded_Definite_Queues;
with WisiToken.Parse.LR.McKenzie_Recover.Parse;
with WisiToken.Parse.LR.Parser;
package body WisiToken.Parse.LR.McKenzie_Recover.Explore is

   procedure Do_Shift
     (Label             : in     String;
      Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      State             : in     State_Index;
      ID                : in     Token_ID;
      Cost_Delta        : in     Integer;
      Strategy          : in     Strategies)
   is
      use Recover_Op_Arrays;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Term : constant Syntax_Trees.Node_Access := Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Config);
      Op   : constant Recover_Op               := (Insert, ID, Shared.Tree.Get_Sequential_Index (Term));
   begin
      Config.Strategy_Counts (Strategy) := Config.Strategy_Counts (Strategy) + 1;

      if Is_Full (Config.Ops) then
         Super.Config_Full (Shared, "do_shift ops", Parser_Index);
         raise Bad_Config;
      else
         Append (Config.Ops, Op);
      end if;

      if Cost_Delta = 0 then
         Config.Cost := Config.Cost + McKenzie_Param.Insert (ID);
      else
         --  Cost_Delta /= 0 comes from Insert_Minimal_Complete_Actions. That
         --  doesn't mean it is better than any other solution, so don't let
         --  cost be 0.
         --
         --  We don't just eliminate all cost for Minimal_Complete_Actions;
         --  that leads to using it far too much at the expense of better
         --  solutions.
         Config.Cost := Integer'Max (1, Config.Cost + McKenzie_Param.Insert (ID) + Cost_Delta);
      end if;

      Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;
      Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

      if Config.Stack.Is_Full then
         Super.Config_Full (Shared, "do_shift stack", Parser_Index);
         raise Bad_Config;
      else
         Config.Stack.Push ((State, (Virtual => True, ID => ID, others => <>)));
      end if;
      if Trace_McKenzie > Detail then
         Super.Put
           (Shared,
            (if Label'Length > 0 then Label & ": " else "") & "insert " & Image (ID, Shared.Tree.Lexer.Descriptor.all),
            Parser_Index, Config);
      end if;

      Local_Config_Heap.Add (Config);
   end Do_Shift;

   procedure Do_Reduce_1
     (Label             : in     String;
      Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Action            : in     Reduce_Action_Rec;
      Do_Language_Fixes : in     Boolean := True)
   is
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Prev_State : constant Unknown_State_Index := Config.Stack.Peek.State;

      Descriptor : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;
      Nonterm    : Syntax_Trees.Recover_Token;
      New_State  : Unknown_State_Index;

      Status : constant Syntax_Trees.In_Parse_Actions.Status := Parse.Reduce_Stack
        (Shared, Config.Stack, Action, Nonterm);
   begin
      Config.In_Parse_Action_Status := Status;
      case Config.In_Parse_Action_Status.Label is
      when Ok =>
         null;

      when Syntax_Trees.In_Parse_Actions.Error =>
         Config.Error_Token                 := Nonterm;
         Config.In_Parse_Action_Token_Count := SAL.Base_Peek_Type (Action.Token_Count);

         if Do_Language_Fixes then
            if Shared.Language_Fixes /= null then
               Shared.Language_Fixes (Super, Shared, Parser_Index, Local_Config_Heap, Config);
            end if;
         end if;

         --  Finish the reduce; ignore the check fail.
         Config.Cost := @ + Table.McKenzie_Param.Ignore_Check_Fail;

         if Config.Stack.Depth < Config.In_Parse_Action_Token_Count then
            raise Bad_Config;
         else
            Config.Stack.Pop (Config.In_Parse_Action_Token_Count);
         end if;
         Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;
         Config.In_Parse_Action_Status := (Label => Ok);
      end case;

      if Config.Stack.Depth = 0 or else Config.Stack.Peek.State = Unknown_State then
         raise Bad_Config;
      end if;

      New_State := Goto_For (Table, Config.Stack.Peek.State, Action.Production.LHS);

      if New_State = Unknown_State then
         --  Bug in LALR parser generator; use LR1
         raise Invalid_Case;
      end if;

      Config.Stack.Push ((New_State, Nonterm));

      if Trace_McKenzie > Extra and Label'Length > 0 then
         Put_Line
           (Shared.Tree, Super.Stream (Parser_Index), Label &
              ": state" & State_Index'Image (Prev_State) & " reduce" &
              Ada.Containers.Count_Type'Image (Action.Token_Count) & " to " &
              Image (Action.Production.LHS, Descriptor) & ", goto" &
              State_Index'Image (New_State) & " via" & State_Index'Image (Config.Stack.Peek (2).State) &
              (case Status.Label is
               when Ok => "",
               when Syntax_Trees.In_Parse_Actions.Error => " " & Status.Label'Image));
      end if;
   end Do_Reduce_1;

   procedure Do_Reduce_2
     (Label             : in     String;
      Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Inserted_ID       : in     Token_ID;
      Cost_Delta        : in     Integer;
      Strategy          : in     Strategies)
   --  Perform reduce actions until shift Inserted_ID, add the final
   --  configuration to the heap. If a conflict is encountered, process the
   --  other actions the same way. If a user in-parse action fails, enqueue
   --  possible solutions. For parse table error or accept actions, or
   --  exception Bad_Config, do nothing.
   is
      Orig_Config : Configuration;
      Table       : Parse_Table renames Shared.Table.all;
      Next_Action : Parse_Action_Node_Ptr := Action_For (Table, Config.Stack.Peek.State, Inserted_ID);

      procedure Do_One (Config : in out Configuration; Action : in Parse_Action_Rec)
      is begin
         case Action.Verb is
         when Shift =>
            Do_Shift
              (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Action.State, Inserted_ID,
               Cost_Delta, Strategy);

         when Reduce =>
            Do_Reduce_1 (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Action);
            Do_Reduce_2
              (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Inserted_ID, Cost_Delta, Strategy);

         when Accept_It | Error =>
            --  Most likely a Minimal_Complete that doesn't work;
            --  test_mckenzie_recover.adb Empty_Comments.
            raise Invalid_Case;
         end case;
      end Do_One;
   begin
      if Next_Action.Next /= null then
         Orig_Config := Config;
      end if;

      Do_One (Config, Next_Action.Item);

      Next_Action := Next_Action.Next;

      loop
         exit when Next_Action = null;
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : Configuration := Orig_Config;
         begin
            Do_One (New_Config, Next_Action.Item);
         end;

         Next_Action := Next_Action.Next;
      end loop;
   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      end if;
   end Do_Reduce_2;

   function Edit_Point_Matches_Ops
     (Super  : in out Base.Supervisor;
      Shared : in out Parser.Parser;
      Config : in     Configuration)
     return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
      pragma Assert (Length (Config.Ops) > 0);
      Op : Recover_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
      Term : constant Syntax_Trees.Node_Access := Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Config);
   begin
      return Shared.Tree.Get_Sequential_Index (Term) =
        (case Op.Op is
         when Fast_Forward => Op.FF_Next_Index,
         when Undo_Reduce  => Op.UR_Token_Index,
         when Push_Back    => Op.PB_Token_Index,
         when Insert       => Op.Ins_Before,
         when Delete       => Op.Del_Token_Index + 1);
   end Edit_Point_Matches_Ops;

   procedure Fast_Forward
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   --  Apply the ops in Config.Insert_Delete; they were inserted by some
   --  fix. Leaves Config.Error_Token, Config.In_Parse_Action_Status set.
   --  If there are conflicts, all are parsed. All succeeding configs are
   --  enqueued in Local_Config_Heap.
   is
      use Parse.Parse_Item_Arrays;
      use Recover_Op_Arrays;

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;

      First_Node : constant Syntax_Trees.Valid_Node_Access := Parse.Peek_Current_First_Sequential_Terminal
        (Super, Shared, Config);

      Dummy : Boolean := Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config,
         Shared_Token_Goal => Syntax_Trees.Invalid_Sequential_Index,
         All_Conflicts     => True,
         Trace_Prefix      => "fast_forward");
   begin
      --  This solution is from Language_Fixes (see gate on call below); any
      --  cost increase is done there.
      --
      --  We used to handle the Parse_Items.Length = 1 case specially, and
      --  return Continue. Maintaining that requires too much code
      --  duplication.

      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
               --  Parse processed all Config.Insert_Delete without error;
               --  Item.Config.Error_Token.ID, Check_Status are correct.

               if not Edit_Point_Matches_Ops (Super, Shared, Item.Config) then

                  if Is_Full (Item.Config.Ops) then
                     Super.Config_Full (Shared, "fast_forward 1", Parser_Index);
                     raise Bad_Config;
                  else
                     declare
                        Next_Node : constant Syntax_Trees.Valid_Node_Access :=
                          Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Item.Config);
                     begin
                        Super.Extend_Sequential_Index (Shared, Thru => Next_Node, Positive => True);

                        Append
                          (Item.Config.Ops,
                           (Fast_Forward,
                            FF_First_Index => Shared.Tree.Get_Sequential_Index (First_Node),
                            FF_Next_Index  => Shared.Tree.Get_Sequential_Index (Next_Node)));
                     end;
                  end if;
               end if;

               Item.Config.Minimal_Complete_State := None;
               Item.Config.Matching_Begin_Done    := False;
               Local_Config_Heap.Add (Item.Config);

               if Trace_McKenzie > Detail then
                  Super.Put (Shared, "fast forward enqueue", Parser_Index, Item.Config);
               end if;
            end if;
         exception
         when Bad_Config =>
            if Debug_Mode then
               raise;
            else
               --  Process other Parse_Items.
               null;
            end if;
         end;
      end loop;
   end Fast_Forward;

   function Check
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Config            : in out Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
     return Check_Status
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
      use Parse.Parse_Item_Arrays;
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;

      First_Node : constant Syntax_Trees.Valid_Node_Access := Parse.Peek_Current_First_Sequential_Terminal
        (Super, Shared, Config);

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;

      procedure Enqueue (Item : in out Parse.Parse_Item)
      is begin
         --  Append or update a Fast_Forward to indicate the changed edit
         --  point.
         Item.Config.Minimal_Complete_State := None;
         Item.Config.Matching_Begin_Done    := False;

         if Last_Index (Item.Config.Ops) /= Recover_Op_Arrays.No_Index and then
           Constant_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).Op = Fast_Forward
         then
            --  Update the trailing Fast_Forward.
            Variable_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).FF_Next_Index :=
              Shared.Tree.Get_Sequential_Index
                (Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Item.Config));
         else
            if Is_Full (Item.Config.Ops) then
               Super.Config_Full (Shared, "check 1", Parser_Index);
               raise Bad_Config;
            else
               declare
                  Next_Node : constant Syntax_Trees.Node_Access := Parse.Peek_Current_First_Sequential_Terminal
                    (Super, Shared, Item.Config);
               begin
                  Append
                    (Item.Config.Ops,
                     (Fast_Forward,
                      FF_First_Index => Shared.Tree.Get_Sequential_Index (First_Node),
                      FF_Next_Index  => Shared.Tree.Get_Sequential_Index (Next_Node)));
               end;
            end if;
         end if;
         Local_Config_Heap.Add (Item.Config);
         if Trace_McKenzie > Detail then
            Base.Put (Super, Shared, "new error point ", Parser_Index, Item.Config);
         end if;
      end Enqueue;

      Abandon_If_Fail : Boolean := False;
   begin
      if Length (Config.Ops) > 0 then
         declare
            Op : Recover_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
         begin
            case Op.Op is
            when Push_Back =>
               --  Check would undo the Push_Back, leading to
               --  duplicate results. See test_mckenzie_recover.adb Do_Delete_First and
               --  three_action_conflict_lalr.parse_good for examples.
               return Continue;

            when Undo_Reduce =>
               if Config.In_Parse_Action_Status.Label /= Ok then
                  --  This is the "ignore error" solution for a check fail; check it.
                  Config.In_Parse_Action_Status := (Label => Ok);
                  Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;

               else
                  --  Check might just undo the Undo_Reduce, but sometimes it's the last
                  --  op required to succeed after Delete; test_mckenzie_recover.adb
                  --  Error_2, Extra_Begin_1.
                  Abandon_If_Fail := True;
               end if;

            when others =>
               --  Check it
               null;
            end case;
         end;
      end if;

      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config, Config.Resume_Token_Goal,
         All_Conflicts => False,
         Trace_Prefix  => "check")
      then
         Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
         if Trace_McKenzie > Extra then
            Put_Line (Shared.Tree, Super.Stream (Parser_Index), "check result: SUCCESS");
         end if;
         return Success;
      end if;

      if Abandon_If_Fail then
         return Abandon;
      end if;

      if Parse.Parse_Item_Arrays.Length (Parse_Items) = 1 then
         --  Return Abandon or Continue.
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref
              (Parse_Items, First_Index (Parse_Items));
         begin
            if Item.Config.In_Parse_Action_Status.Label /= Ok then
               Config.In_Parse_Action_Status := Item.Config.In_Parse_Action_Status;
               Config.Error_Token            := Item.Config.Error_Token;

               if Item.Shift_Count > 0 or Item.Reduce_Count > 0 then
                  --  Progress was made, so let Language_Fixes try again on the new
                  --  Config. Checking Reduce_Count > 0 is required for
                  --  test_mckenzie_recover.adb Missing_Name_6.
                  Enqueue (Item);
               end if;

               --  Explore cannot fix an In_Parse_Action fail; only Language_Fixes.
               --  The "ignore error" case is handled immediately on return from
               --  Language_Fixes in Process_One, below.
               return Abandon;

            else
               if Item.Shift_Count = 0 then
                  --  Parse did not process any Deletes from Insert_Delete; Fast_Forward
                  --  did that. So the very first token caused an error, and Config is
                  --  unchanged. Just set the error.
                  Config.Error_Token  := Item.Config.Error_Token;
                  Config.In_Parse_Action_Status := (Label => Ok);
                  return Continue;
               else
                  --  Item.Config differs from Config, so enqueue it.
                  Enqueue (Item);

                  --  Also Continue
                  --  Config; Explore might find something that will help (see
                  --  test_mckenzie_recover.adb Extra_Begin). On the other hand, this
                  --  can lead to lots of bogus configs (see If_In_Handler).
                  Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
                  Config.In_Parse_Action_Status   := (Label => Ok);

                  return Continue;
               end if;
            end if;
         end;
      end if;

      --  More than one Parse_Item, all failed, all made progress,
      --  so enqueue them.
      --
      --  We know they all made progress because not doing so means the
      --  first token encountered an error, there is no chance to encounter
      --  a conflict, and there can be only one Parse_Item, which is handled
      --  above.
      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            pragma Assert (Item.Parsed and Shared.Tree.Element_ID (Item.Config.Error_Token) /= Invalid_Token_ID);

            Enqueue (Item);
         end;
      end loop;
      return Abandon;

   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      else
         return Abandon;
      end if;
   end Check;

   function Check_Reduce_To_Start
     (Super        : in out Base.Supervisor;
      Shared       : in out Parser.Parser;
      Parser_Index : in     SAL.Base_Peek_Type;
      Orig_Config  : in     Configuration)
     return Boolean
      --  Returns True if Config reduces to the start nonterm.
   is
      Table : Parse_Table renames Shared.Table.all;

      function To_Reduce_Action (Item : in Minimal_Action) return Reduce_Action_Rec
      is begin
         return (Reduce, Item.Production, Item.Token_Count);
      end To_Reduce_Action;

      Local_Config_Heap : Config_Heaps.Heap_Type; -- never used, because Do_Language_Fixes is False.

      Config  : Configuration                := Orig_Config;
      Actions : Minimal_Action_Arrays.Vector := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
   begin
      loop
         case Actions.Length is
         when 0 =>
            if (for some Item of Table.States (Config.Stack.Peek.State).Kernel =>
                  Item.Production.LHS = Shared.Tree.Lexer.Descriptor.Accept_ID)
            then
               return True;
            else
               return False;
            end if;

         when 1 =>
            case Actions (Actions.First_Index).Verb is
            when Shift =>
               return False;

            when Reduce =>
               Do_Reduce_1
                 ("", Super, Shared, Parser_Index, Local_Config_Heap, Config,
                  To_Reduce_Action (Actions (Actions.First_Index)),
                  Do_Language_Fixes => False);

               Actions := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
            end case;

         when others =>
            return False;
         end case;

         --  loop only exits via returns above
      end loop;
   exception
   when Bad_Config =>
      --  From Do_Reduce_1
      if Debug_Mode then
         raise;
      else
         return False;
      end if;
   end Check_Reduce_To_Start;

   procedure Try_Push_Back
     (Super             : in out Base.Supervisor;
      Shared            : in Parser.Parser;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   --  Try pushing back the stack top, to allow operations at that point.
   --  We assume the caller used Push_Back_Valid.
   is
      use Recover_Op_Arrays;
      use Syntax_Trees;

      Token : constant Recover_Token := Config.Stack.Peek.Token;

      New_Config : Configuration := Config;
   begin
      --  Since we are not actually changing the source text, it is tempting
      --  to give this operation zero cost. But then we keep doing push_back
      --  forever, making no progress. So we give it a cost.

      New_Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;
      New_Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

      if Is_Full (New_Config.Ops) then
         Super.Config_Full (Shared, "push_back 1", Parser_Index);
         raise Bad_Config;
      end if;

      Do_Push_Back (Shared.Tree, New_Config);
      New_Config.Cost := @ + Shared.Table.McKenzie_Param.Push_Back (Shared.Tree.Element_ID (Token));
      New_Config.Strategy_Counts (Push_Back) := New_Config.Strategy_Counts (Push_Back) + 1;

      Local_Config_Heap.Add (New_Config);

      if Trace_McKenzie > Detail then
         Super.Put
           (Shared, "push_back " & Image (Shared.Tree.Element_ID (Token), Shared.Tree.Lexer.Descriptor.all),
            Parser_Index, New_Config);
      end if;
   end Try_Push_Back;

   function Just_Pushed_Back_Or_Deleted
     (Super  : in out Base.Supervisor;
      Shared : in out Parser.Parser;
      Config : in     Configuration;
      ID     : in     Token_ID)
     return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
      use Syntax_Trees;

      Target_Token_Index : Sequential_Index :=
        Shared.Tree.Get_Sequential_Index (Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Config));
      --  Next token; ID might be inserted before it (see Do_Shift).
   begin
      --  This function is called when considering whether to insert ID before
      --  Config.Current_Shared_Token.
      --
      --  We need to consider more than one recent op here; see test_mckenzie_recover.adb
      --  Check_Multiple_Delete_For_Insert. Checking only one op allows this solution there:
      --
      --  ...  (DELETE, END, 7), (DELETE, SEMICOLON, 8), (INSERT, END, 9), (INSERT, SEMICOLON, 9)
      --
      for I in reverse First_Index (Config.Ops) .. Last_Index (Config.Ops) loop
         declare
            Op : Recover_Op renames Constant_Ref (Config.Ops, I);
         begin
            case Op.Op is
            when Push_Back =>
               --  The case we are preventing for Push_Back is typically one of:
               --  (PUSH_BACK, Identifier, 2), (INSERT, Identifier, 2)
               --  (PUSH_BACK, Identifier, 2), (PUSH_BACK, END, 3), (INSERT, Identifier, 3), (INSERT, END, 3),
               if Op.PB_Token_Index = Target_Token_Index then
                  if Op.PB_ID = ID then
                     return True;
                  else
                     Target_Token_Index := Op.PB_Token_Index - 1;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Delete =>
               if Op.Del_Token_Index = Target_Token_Index - 1 then
                  if Op.Del_ID = ID then
                     return True;
                  else
                     Target_Token_Index := Op.Del_Token_Index;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Fast_Forward | Insert | Undo_Reduce =>
               return False;
            end case;
         end;
      end loop;
      return False;
   end Just_Pushed_Back_Or_Deleted;

   procedure Try_Undo_Reduce
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Config            : in     Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
   is
      use Recover_Op_Arrays;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;
      Token          : constant Syntax_Trees.Recover_Token := Config.Stack.Peek.Token;
      New_Config     : Configuration                       := Config;
   begin
      pragma Assert (not Token.Virtual); -- We assume caller used Undo_Reduce_Valid.

      New_Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
      New_Config.In_Parse_Action_Status   := (Label => Syntax_Trees.In_Parse_Actions.Ok);

      if not Shared.Tree.Is_Empty_Nonterm (Token.Element_Node) then
         --  Token is not empty.
         New_Config.Cost := New_Config.Cost + McKenzie_Param.Undo_Reduce (Shared.Tree.Element_ID (Token));
      end if;

      if Is_Full (New_Config.Ops) then
         Super.Config_Full (Shared, "undo_reduce 1", Parser_Index);
         raise Bad_Config;
      end if;

      Unchecked_Undo_Reduce (Super, Shared, New_Config);

      New_Config.Strategy_Counts (Undo_Reduce) := @ + 1;

      Local_Config_Heap.Add (New_Config);

      if Trace_McKenzie > Detail then
         Super.Put (Shared, "undo_reduce " & Image (Shared.Tree.Element_ID (Token), Shared.Tree.Lexer.Descriptor.all),
                   Parser_Index, New_Config);
      end if;
   end Try_Undo_Reduce;

   procedure Insert_From_Action_List
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Config            : in     Configuration;
      Minimal_Insert    : in     Token_ID_Arrays.Vector;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
   is
      Table      : Parse_Table renames Shared.Table.all;
      EOF_ID     : Token_ID renames Shared.Tree.Lexer.Descriptor.EOI_ID;
      Descriptor : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;

      --  Find terminal insertions from the current state's action_list to try.
      --
      --  We perform any needed reductions and one shift, so the config is
      --  in a consistent state, and enqueue the result. If there are any
      --  conflicts or semantic check fails encountered, they create other
      --  configs to enqueue.

      Current_First_Terminal_ID : constant Token_ID := Shared.Tree.ID
        (Parse.Peek_Current_First_Terminal (Shared.Tree, Config));

      Cached_Config : Configuration;
      Cached_Action : Reduce_Action_Rec;
      --  Most of the time, all the reductions in a state are the same. So
      --  we cache the first result. This includes one reduction; if an
      --  associated semantic check failed, this does not include the fixes.

      I : Parse_Action_Node_Ptr;
   begin
      for Node of Table.States (Config.Stack.Peek.State).Action_List loop
         I := Node.Actions;
         loop
            exit when I = null;
            declare
               ID     : constant Token_ID := Node.Symbol;
               Action : Parse_Action_Rec renames I.Item;
            begin
               if ID /= EOF_ID and then -- can't insert eof
                 ID /= Invalid_Token_ID -- invalid when Verb = Error
               then
                  if Just_Pushed_Back_Or_Deleted (Super, Shared, Config, ID) then
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Shared.Tree, Super.Stream (Parser_Index),
                           "Insert: abandon " & Image (ID, Descriptor) & ": undo push_back");
                     end if;
                  elsif ID = Current_First_Terminal_ID then
                     --  This is needed because we allow explore when the error is not at
                     --  the explore point; it prevents inserting useless tokens (ie
                     --  'identifier ;' in ada_lite).
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Shared.Tree, Super.Stream (Parser_Index),
                           "Insert: abandon " & Image (ID, Descriptor) & ": current token");
                     end if;

                  elsif (for some Minimal of Minimal_Insert => ID = Minimal) then
                     --  Was inserted by Insert_Minimal_Complete_Actions
                     null;

                  else
                     case Action.Verb is
                     when Shift =>
                        declare
                           New_Config : Configuration := Config;
                        begin
                           Do_Shift
                             ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, ID,
                              Cost_Delta => 0,
                              Strategy   => Insert);
                        end;

                     when Reduce =>
                        if not Equal (Action, Cached_Action) then
                           declare
                              New_Config : Configuration := Config;
                           begin
                              New_Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;
                              New_Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

                              Do_Reduce_1
                                ("Insert " & Image (ID, Descriptor), Super, Shared, Parser_Index, Local_Config_Heap,
                                 New_Config, Action);
                              Cached_Config := New_Config;
                              Cached_Action := Action;

                              Do_Reduce_2
                                ("Insert " & Image (ID, Descriptor), Super, Shared, Parser_Index, Local_Config_Heap,
                                 New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;

                        else
                           declare
                              New_Config : Configuration := Cached_Config;
                           begin
                              Do_Reduce_2
                                ("Insert " & Image (ID, Descriptor) & " (cached reduce)", Super, Shared, Parser_Index,
                                 Local_Config_Heap, New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;
                        end if;

                     when Accept_It =>
                        raise SAL.Programmer_Error with "found test case for Process_One Accept_It";

                     when Error =>
                        null;
                     end case;
                  end if;
               end if;
            exception
            when Invalid_Case =>
               --  Try other actions
               null;
            end;
            I := I.Next;
         end loop;
      end loop;
   end Insert_From_Action_List;

   function Insert_Minimal_Complete_Actions
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Orig_Config       : in out Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
     return Token_ID_Arrays.Vector
   --  Return tokens inserted (empty if none).
   is
      use Ada.Containers;

      Table         : Parse_Table renames Shared.Table.all;
      Descriptor    : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;
      Inserted      : Token_ID_Array (1 .. 10) := (others => Invalid_Token_ID);
      Inserted_Last : Integer                  := Inserted'First - 1;

      type Work_Item is record
         Action     : Minimal_Action;
         Cost_Delta : Integer;
         Config     : Configuration;
      end record;

      package Item_Queues is new SAL.Gen_Bounded_Definite_Queues (Work_Item);
      use Item_Queues;

      Work : Queue_Type (10);
      --  The required queue size depends on the number of multiple-item
      --  Minimal_Complete_Actions encountered. That is limited by compound
      --  statement nesting, and by the frequency of such actions.

      procedure Safe_Add_Work (Label : in String; Item : in Work_Item)
      is begin
         if Is_Full (Work) then
            Super.Config_Full (Shared, "Minimal_Complete_Actions " & Label, Parser_Index);
            raise Bad_Config;
         else
            Add (Work, Item);
         end if;
      end Safe_Add_Work;

      function To_Reduce_Action (Action : in Minimal_Action) return Reduce_Action_Rec
        is (Reduce, Action.Production, Action.Token_Count);

      procedure Minimal_Do_Shift
        (Action     : in     Minimal_Action;
         Cost_Delta : in     Integer;
         Config     : in out Configuration)
      is begin
         if Just_Pushed_Back_Or_Deleted (Super, Shared, Config, Action.ID) then
            if Trace_McKenzie > Extra then
               Put_Line
                 (Shared.Tree, Super.Stream (Parser_Index),
                  "Minimal_Complete_Actions: abandon " & Image (Action.ID, Descriptor) & ": undo push back");
            end if;
         else
            Config.In_Parse_Action_Status           := (Label => Syntax_Trees.In_Parse_Actions.Ok);
            Config.Minimal_Complete_State := Active;
            Inserted_Last                 := Inserted_Last + 1;
            if Inserted_Last <= Inserted'Last then
               Inserted (Inserted_Last)      := Action.ID;
            else
               Super.Config_Full (Shared, "minimal_do_shift Inserted", Parser_Index);
               raise Bad_Config;
            end if;

            Do_Shift
              ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Config,
               Action.State, Action.ID, Cost_Delta,
               Strategy => Minimal_Complete);
         end if;
      end Minimal_Do_Shift;

      procedure Enqueue_Min_Actions
        (Actions : in Minimal_Action_Arrays.Vector;
         Config  : in Configuration)
      is
         use SAL;
         Length : array (Actions.First_Index .. Actions.Last_Index) of Count_Type := (others => Count_Type'Last);

         Min_Length : Count_Type := Count_Type'Last;
      begin
         if Trace_McKenzie > Extra then
            Put_Line
              (Shared.Tree, Super.Stream (Parser_Index), "Minimal_Complete_Actions: " &
                 Image (Actions, Descriptor));
         end if;

         if Actions.Length = 0 then
            return;
         elsif Actions.Length = 1 then
            Safe_Add_Work
              ("1", (Actions (Actions.First_Index), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));
            return;
         end if;

         --  More than one minimal action in State; try to use next states to pick one.
         Actions_Loop :
         for I in Actions.First_Index .. Actions.Last_Index loop
            declare
               function Matches (Item : in Kernel_Info; Action : in Minimal_Action) return Boolean
               is begin
                  case Action.Verb is
                  when Shift =>
                     return Item.Before_Dot = Action.ID;
                  when Reduce =>
                     return Item.Before_Dot = Action.Production.LHS;
                  end case;
               end Matches;

               function Length_After_Dot
                 (Item   : in Kernel_Info;
                  Action : in Minimal_Action;
                  Stack  : in Recover_Stacks.Stack)
                 return Ada.Containers.Count_Type
               is
                  Match_ID   : Token_ID;
                  New_Stack  : Recover_Stacks.Stack      := Stack;
                  Next_State : Unknown_State_Index;
                  Result     : Ada.Containers.Count_Type;
                  Min_Result : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
               begin
                  case Action.Verb is
                  when Shift =>
                     New_Stack.Push
                       ((Action.State, (ID => Action.ID, others => <>)));
                     Next_State := Action.State;
                     Match_ID   := Action.ID;

                  when Reduce =>
                     New_Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
                     Next_State := Goto_For (Shared.Table.all, New_Stack.Peek.State, Action.Production.LHS);
                     if Next_State = Unknown_State then
                        --  We get here when Insert_From_Action_Table started us down a bad path
                        raise Invalid_Case;
                     end if;

                     New_Stack.Push
                       ((Next_State, (ID => Action.Production.LHS, others => <>)));
                     Match_ID   := Action.Production.LHS;
                  end case;

                  if Trace_McKenzie > Extra then
                     Shared.Tree.Lexer.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                  end if;

                  for Item of Shared.Table.States (Next_State).Kernel loop
                     if Item.Before_Dot = Match_ID then
                        if Item.Length_After_Dot = 0 then
                           Result := Length_After_Dot
                             (Item, (Reduce, Item.Reduce_Production, Item.Reduce_Count), New_Stack);
                        else
                           Result := Item.Length_After_Dot;
                        end if;
                     end if;

                     if Result < Min_Result then
                        Min_Result := Result;
                     end if;
                  end loop;
                  return Min_Result;
               end Length_After_Dot;

               Action     : constant Minimal_Action := Actions (I);
               Next_State : constant State_Index    :=
                 (case Action.Verb is
                  when Shift  => Action.State,
                  when Reduce => Goto_For
                    (Shared.Table.all,
                     Config.Stack.Peek (Base_Peek_Type (Action.Token_Count) + 1).State,
                     Action.Production.LHS));
            begin
               if Trace_McKenzie > Extra then
                  Put_Line
                    (Shared.Tree, Super.Stream (Parser_Index),
                     "Minimal_Complete_Actions: " & Image (Action, Descriptor));
               end if;

               for Item of Shared.Table.States (Next_State).Kernel loop

                  if Matches (Item, Action) then
                     --  For Action.Verb = Reduce, more than one item may match
                     if Item.Length_After_Dot = 0 then
                        --  Set Length from a non-zero-length non-recursive item.
                        Length (I) := Length_After_Dot (Item, Action, Config.Stack);

                     elsif Item.Length_After_Dot < Length (I) then
                        if Trace_McKenzie > Extra then
                           --  Length_After_Dot outputs this in other branch
                           Shared.Tree.Lexer.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                        end if;
                        Length (I) := Item.Length_After_Dot;

                     end if;

                     if Trace_McKenzie > Extra then
                        Shared.Tree.Lexer.Trace.Put (" length" & Length (I)'Image);
                     end if;
                     if Length (I) < Min_Length then
                        Min_Length := Length (I);
                     end if;
                  end if;
               end loop;
               if Trace_McKenzie > Extra then
                  Shared.Tree.Lexer.Trace.New_Line;
               end if;
            end;
         end loop Actions_Loop;

         for I in Length'Range loop
            if Length (I) = Min_Length then
               Safe_Add_Work ("2", (Actions (I), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));

            elsif Trace_McKenzie > Extra then
               Put_Line
                 (Shared.Tree, Super.Stream (Parser_Index), "Minimal_Complete_Actions: drop " &
                    Image (Actions (I), Descriptor) & " not minimal");
            end if;
         end loop;
      end Enqueue_Min_Actions;

   begin
      if Orig_Config.Stack.Depth = 1 then
         --  Get here with an empty source file, or a syntax error on the first
         --  token.
         return Token_ID_Arrays.Empty_Vector;

      elsif Orig_Config.Minimal_Complete_State = Done then
         if Trace_McKenzie > Extra then
            Put_Line (Shared.Tree, Super.Stream (Parser_Index), "Minimal_Complete_Actions: done");
         end if;
         return Token_ID_Arrays.Empty_Vector;
      end if;

      Enqueue_Min_Actions (Table.States (Orig_Config.Stack.Peek.State).Minimal_Complete_Actions, Orig_Config);

      loop
         exit when Is_Empty (Work);

         declare
            Item : Work_Item := Get (Work);
         begin
            if Trace_McKenzie > Extra then
               Put_Line
                 (Shared.Tree, Super.Stream (Parser_Index),
                  "Minimal_Complete_Actions: dequeue work item " &
                    Image (Item.Action, Descriptor));
            end if;

            case Item.Action.Verb is
            when Reduce =>
               --  Do a reduce, look at resulting state. Keep reducing until we can't
               --  anymore.
               declare
                  Reduce_Action : Reduce_Action_Rec := To_Reduce_Action (Item.Action);
                  Actions       : Minimal_Action_Arrays.Vector;
               begin
                  loop
                     Do_Reduce_1
                       ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Item.Config,
                        Reduce_Action,
                        Do_Language_Fixes => False);

                     Actions := Table.States (Item.Config.Stack.Peek.State).Minimal_Complete_Actions;

                     case Actions.Length is
                     when 0 =>
                        if Trace_McKenzie > Detail then
                           Put_Line
                             (Shared.Tree, Super.Stream (Parser_Index),
                              "Minimal_Complete_Actions state" & Item.Config.Stack.Peek.State'Image &
                                " abandoned: no actions");
                        end if;
                        exit;
                     when 1 =>
                        case Actions (Actions.First_Index).Verb is
                        when Shift =>
                           Minimal_Do_Shift (Actions (Actions.First_Index), Item.Cost_Delta, Item.Config);
                           exit;
                        when Reduce =>
                           Reduce_Action := To_Reduce_Action (Actions (Actions.First_Index));
                        end case;

                     when others =>
                        Enqueue_Min_Actions (Actions, Item.Config);
                        exit;
                     end case;
                  end loop;
               end;

            when Shift =>
               Minimal_Do_Shift (Item.Action, Item.Cost_Delta, Item.Config);
            end case;
         exception
         when Invalid_Case =>
            null;
         end;
      end loop;

      if Inserted_Last = Inserted'First - 1 then
         --  Nothing inserted this round.
         if Orig_Config.Minimal_Complete_State = Active then
            Orig_Config.Minimal_Complete_State := Done;
         end if;
      end if;

      return To_Vector (Inserted (1 .. Inserted_Last));
   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      else
         return Token_ID_Arrays.Empty_Vector;
      end if;
   end Insert_Minimal_Complete_Actions;

   procedure Insert_Matching_Begin
     (Super                 : in out Base.Supervisor;
      Shared                : in out Parser.Parser;
      Parser_Index          : in     SAL.Base_Peek_Type;
      Config                : in     Configuration;
      Local_Config_Heap     : in out Config_Heaps.Heap_Type;
      Matching_Begin_Tokens : in     Token_ID_Arrays.Vector)
   is
      Table      : Parse_Table renames Shared.Table.all;
      Descriptor : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;
   begin
      if Config.Matching_Begin_Done then
         if Trace_McKenzie > Extra then
            Put_Line (Shared.Tree, Super.Stream (Parser_Index), "Matching_Begin abandoned: done");
         end if;
         return;
      end if;

      if Just_Pushed_Back_Or_Deleted (Super, Shared, Config, Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index))
      then
         if Trace_McKenzie > Extra then
            Put_Line
              (Shared.Tree, Super.Stream (Parser_Index), "Matching_Begin abandoned " &
                 Image (Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index), Descriptor) & ": undo push_back");
         end if;
         return;
      end if;

      declare
         New_Config : Configuration := Config;
      begin
         if Undo_Reduce_Valid (Super, Shared, New_Config) then
            --  We may need Undo_Reduce to shift the matching token; see
            --  ada_mode-recover_40.adb
            Unchecked_Undo_Reduce (Super, Shared, New_Config);
         end if;

         for ID of Matching_Begin_Tokens loop
            Insert (Super, Shared, New_Config, ID);
         end loop;

         declare
            use Parse.Parse_Item_Arrays;
            Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
            Dummy : constant Boolean :=  Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, New_Config,
               Shared_Token_Goal => Syntax_Trees.Invalid_Sequential_Index,
               All_Conflicts     => True,
               Trace_Prefix      => "parse Matching_Begin");
         begin
            for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
               declare
                  Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
               begin
                  if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
                     Item.Config.Matching_Begin_Done := True;
                     Item.Config.Cost := Item.Config.Cost + Table.McKenzie_Param.Matching_Begin;
                     Item.Config.Strategy_Counts (Matching_Begin) := Item.Config.Strategy_Counts (Matching_Begin) + 1;
                     Item.Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
                     Item.Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

                     if Trace_McKenzie > Detail then
                        Super.Put
                          (Shared, "Matching_Begin: insert " & Image (Matching_Begin_Tokens, Descriptor),
                           Parser_Index, Item.Config);
                     end if;
                     Local_Config_Heap.Add (Item.Config);
                  else
                     if Trace_McKenzie > Detail then
                        Super.Put
                          (Shared,
                           "Matching_Begin: abandon " & Image (Matching_Begin_Tokens, Descriptor) & ": parse fail",
                           Parser_Index, Item.Config);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end;
   end Insert_Matching_Begin;

   procedure Try_Insert_Terminal
     (Super             :         in out Base.Supervisor;
      Shared            :         in out Parser.Parser;
      Parser_Index      :         in     SAL.Base_Peek_Type;
      Config            : aliased in out Configuration;
      Local_Config_Heap :         in out Config_Heaps.Heap_Type)
   is
      use all type WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Tokens                : Token_ID_Array_1_3;
      Matching_Begin_Tokens : Token_ID_Arrays.Vector;
      Forbid_Minimal_Insert : Boolean := False;

      Minimal_Inserted : Token_ID_Arrays.Vector;
   begin
      if Shared.Language_Matching_Begin_Tokens /= null then
         Parse.Current_Token_ID_Peek_3 (Super, Shared, Config, Tokens);

         if Tokens (1) /= Invalid_Token_ID then
            Shared.Language_Matching_Begin_Tokens
              (Super, Shared, Tokens, Config, Matching_Begin_Tokens, Forbid_Minimal_Insert);
         end if;
      end if;

      if not Forbid_Minimal_Insert then
         --  See test_mckenzie_recover.adb Forbid_Minimal_Insert for rationale.
         Minimal_Inserted := Insert_Minimal_Complete_Actions
           (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Matching_Begin_Tokens.Length > 0 then
         Insert_Matching_Begin (Super, Shared, Parser_Index, Config, Local_Config_Heap, Matching_Begin_Tokens);
      end if;

      --  We always do all three; Insert_Minimal_Complete (unless
      --  Forbid_Minimal_Insert), Insert_Matching_Begin,
      --  Insert_From_Action_List; in general it's not possible to tell when
      --  one will be better (see test_mckenzie_recover.adb
      --  Always_Minimal_Complete, Always_Matching_Begin).
      --  Insert_From_Action_List does not insert the Minimal_Inserted tokens,
      --  and it will never insert the Matching_Begin_Tokens, so there is no
      --  duplication. Insert_From_Action_List will normally be more
      --  expensive.
      Insert_From_Action_List (Super, Shared, Parser_Index, Config, Minimal_Inserted, Local_Config_Heap);

      --  It is tempting to use the Goto_List to find nonterms to insert.
      --  But that can easily lead to error states, and it turns out to be
      --  not useful, especially if the grammar has been relaxed so most
      --  expressions and lists can be empty.

   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      else
         null;
      end if;
   end Try_Insert_Terminal;

   procedure Try_Insert_Quote_1
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Current_Line      : in     Line_Number_Type;
      Lexer_Error_Node  : in     Syntax_Trees.Valid_Node_Access;
      Config            : in out Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
   is
      use Recover_Op_Arrays;
      use all type Parser.Language_String_ID_Set_Access;
      use WisiToken.Syntax_Trees;
      use Bounded_Streams;

      Tree        : Syntax_Trees.Tree renames Shared.Tree;
      Descriptor  : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;
      Check_Limit : Syntax_Trees.Sequential_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      function String_ID_Set (String_ID : in Token_ID) return Token_ID_Set
      is begin
         if Shared.Language_String_ID_Set = null then
            return (String_ID .. String_ID => True);
         else
            return Shared.Language_String_ID_Set (Descriptor, String_ID);
         end if;
      end String_ID_Set;

      procedure Delete_All_Pushed_Back
        (Label     : in     String;
         Config    : in out Configuration;
         Max_Index :    out Sequential_Index)
      with Post => Max_Index /= Sequential_Index'Last
      --  Delete all tokens from Config.Input_Stream.
      --  Max_Index is the last node_index deleted.
      is
         Stream    : Bounded_Streams.List renames Config.Input_Stream;
         To_Delete : Bounded_Streams.Cursor;
      begin
         Max_Index := Sequential_Index'Last; --  should be overwritten
         loop
            exit when Length (Stream) = 0;

            if Tree.Child_Count (Stream (First (Stream))) > 0 then
               Parse.Left_Breakdown (Tree, Config.Input_Stream);

            else
               To_Delete := First (Stream);

               declare
                  Node  : constant Node_Access           := Stream (To_Delete);
                  Index : constant Base_Sequential_Index := Tree.Get_Sequential_Index (Node);
               begin
                  if Index /= Invalid_Sequential_Index then
                     --  Node is not from Shared_Stream, so we don't need to tell main
                     --  Parser to delete it.
                     if Is_Full (Config.Ops) then
                        Super.Config_Full (Shared, "insert quote 2 a " & Label, Parser_Index);
                        raise Bad_Config;
                     end if;

                     Append (Config.Ops, (Delete, Shared.Tree.ID (Node), Index));

                     Max_Index := Index;
                  end if;
               end;

               --  non_grammar are moved during "apply ops".
               Delete (Config.Input_Stream, To_Delete);
            end if;
         end loop;
      end Delete_All_Pushed_Back;

      procedure Delete_Pushed_Back
        (Label      : in     String;
         Config     : in out Configuration;
         Target_Ref : in out Config_Stream_Parents;
         Max_Index  :    out Base_Sequential_Index)
      with Pre => Length (Config.Input_Stream) > 0 and Tree.Is_Terminal (Target_Ref.Node)
      --  Delete terminals in Config.Input_Stream, first terminal to
      --  Prev_Terminal (First_Terminal (Target_Ref)). Max_Index is the last
      --  Sequential_Index deleted; Invalid_Sequential_Index if none (ie
      --  Target_Ref.Node is first terminal in Input_Stream). Target_Ref is
      --  updated if Input_Stream is broken down to expose tokens.
      is
         Stream : Bounded_Streams.List renames Config.Input_Stream;

         procedure Delete_First
         is
            To_Delete : Bounded_Streams.Cursor := Stream.First;
         begin
            if Is_Full (Config.Ops) then
               Super.Config_Full (Shared, "insert quote 2 b " & Label, Parser_Index);
               raise Bad_Config;
            end if;

            pragma Assert (Is_Terminal (Shared.Tree.ID (Stream (To_Delete)), Shared.Tree.Lexer.Descriptor.all));
            pragma Assert (Tree.Get_Sequential_Index (Stream (To_Delete)) /= Invalid_Sequential_Index);
            Append
              (Config.Ops,
               (Delete,
                Shared.Tree.ID (Stream (To_Delete)),
                Tree.Get_Sequential_Index (Stream (To_Delete))));

            Max_Index := Tree.Get_Sequential_Index (Stream (To_Delete));

            --  non_grammar are moved during "apply ops".
            Delete (Stream, To_Delete);
         end Delete_First;
      begin
         Max_Index := Invalid_Sequential_Index;
         loop
            exit when Target_Ref.Element = Stream.First and Target_Ref.Node = Stream (Stream.First);

            if Tree.Label (Stream (Stream.First)) = Nonterm then
               Parse.Left_Breakdown (Tree, Stream);

               exit when Target_Ref.Node = Stream (Stream.First);

               --  Find new Target_Ref.Element
               Target_Ref.Element := Stream.First;
               loop
                  exit when
                    (for some I in 1 .. Target_Ref.Parents.Depth =>
                       Stream (Target_Ref.Element) = Target_Ref.Parents.Peek (I));

                  Target_Ref.Element := Stream.Next (Target_Ref.Element);
               end loop;

               exit when Target_Ref.Element = Stream.First;
               Delete_First;
            else
               Delete_First;
            end if;
         end loop;
      end Delete_Pushed_Back;

      procedure Delete_Pushed_Back
        (Label        : in     String;
         Config       : in out Configuration;
         Target_Index : in     Sequential_Index;
         Max_Index    :    out Base_Sequential_Index)
      --  Delete tokens from Config.Input_Stream to Target_Index or
      --  end of Input_Stream. Max_Index is the last node
      --  deleted; Invalid_Sequential_Index if none.
      is
         Stream         : Bounded_Streams.List renames Config.Input_Stream;
         Target_Element : constant Bounded_Streams.Cursor := First (Stream);
      begin
         if not Has_Element (Target_Element) then
            Max_Index := Invalid_Sequential_Index;
            return;
         end if;

         --  Find Target_Index in Config.Input_Stream
         declare
            Target_Ref : Config_Stream_Parents (Config.Input_Stream'Access);
         begin
            Parse.First_Sequential_Terminal (Super, Shared, Target_Ref);
            loop
               exit when Tree.Get_Sequential_Index (Target_Ref.Node) = Target_Index;

               Parse.Next_Sequential_Terminal (Tree, Target_Ref);
               exit when Target_Ref.Element = No_Element; --  Target_Index not in Input_Stream
            end loop;

            if Target_Ref.Element = Bounded_Streams.No_Element then
               Delete_All_Pushed_Back (Label, Config, Max_Index);
            else
               Delete_Pushed_Back (Label, Config, Target_Ref, Max_Index);
            end if;
         end;
      end Delete_Pushed_Back;

      procedure String_Literal_In_Stack
        (Label             : in     String;
         Config            : in out Configuration;
         Matching          : in     SAL.Peek_Type;
         String_Literal_ID : in     Token_ID)
      --  Matching is the peek index of a token in Config.Stack containing a
      --  string literal (possibly more than one). Push back thru that
      --  token, then delete all tokens after the string literal to Config
      --  current token.
      is
         String_Literal : Config_Stream_Parents (Config.Input_Stream'Access);

         Max_Deleted : Base_Sequential_Index;
      begin
         --  Mark the current start of Config.Input_Stream, so we can search
         --  new pushed_back tokens below. test_mckenzie_recover.adb
         --  Strinq_Quote_2.
         Parse.First_Terminal (Tree, String_Literal);

         if not Has_Space (Config.Ops, Ada.Containers.Count_Type (Matching)) then
            Super.Config_Full (Shared, "insert quote 1 " & Label, Parser_Index);
            raise Bad_Config;
         end if;
         for I in 1 .. Matching loop
            if not Push_Back_Valid (Super, Shared, Config, Push_Back_Undo_Reduce => False) then
               --  Probably pushing back thru a previously inserted token
               raise Invalid_Case;
            end if;
            Do_Push_Back (Tree, Config);
         end loop;

         --  Search the pushed_back tokens for the last string literal.
         if String_Literal.Element = No_Element then
            Parse.Last_Sequential_Terminal (Super, Shared, String_Literal);
         end if;
         loop
            exit when Shared.Tree.ID (String_Literal.Node) = String_Literal_ID;
            Parse.Prev_Sequential_Terminal (Tree, String_Literal);
         end loop;

         --  Delete pushed_back tokens before the string literal.
         Delete_Pushed_Back (Label, Config, String_Literal, Max_Deleted);

         --  Process the deletes so Config matches Ops. Also parse the string
         --  literal.
         declare
            First_Node  : constant Valid_Node_Access := Parse.Peek_Current_First_Sequential_Terminal
              (Super, Shared, Config);
            Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
         begin
            if Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, Config,
               Shared_Token_Goal => Tree.Get_Sequential_Index (String_Literal.Node),
               All_Conflicts     => False,
               Trace_Prefix      => "insert quote parse pushback " & Label)
            then
               --  The tokens parsed without error. We don't care if any conflicts
               --  were encountered; they were enqueued the first time this was
               --  parsed.
               Config := Parse.Parse_Item_Array_Refs.Constant_Ref (Parse_Items, 1).Config;
               Append
                 (Config.Ops,
                  (Fast_Forward,
                   FF_First_Index   => Tree.Get_Sequential_Index (First_Node),
                   FF_Next_Index    => Tree.Get_Sequential_Index
                     (Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, Config))));
            else
               raise SAL.Programmer_Error;
            end if;
         end;
      end String_Literal_In_Stack;

      procedure Push_Back_Tokens
        (Full_Label            : in     String;
         Config                : in out Configuration;
         Min_Pushed_Back_Index :    out Syntax_Trees.Sequential_Index)
      with Post => Min_Pushed_Back_Index /= Sequential_Index'Last
      --  Push back stack top; if it is empty, push back the next stack token.
      --
      --  Min_Pushed_Back_Index is sequential_index (first_sequential_terminal (pushed back token)).
      is
         Item  : Recover_Stack_Item;
         First : Node_Access;
      begin
         Min_Pushed_Back_Index := Sequential_Index'Last; -- Should be overwritten
         loop
            if not Push_Back_Valid (Super, Shared, Config, Push_Back_Undo_Reduce => False) then
               --  Probably pushing back thru a previously inserted token
               raise Invalid_Case;
            end if;

            if Is_Full (Config.Ops) then
               Super.Config_Full (Shared, Full_Label, Parser_Index);
               raise Bad_Config;
            end if;

            Item  := Config.Stack.Peek;
            First := Tree.First_Terminal (Item.Token);
            if First /= Invalid_Node_Access then
               First := Tree.First_Sequential_Terminal (First);
            end if;

            Do_Push_Back (Tree, Config);

            if First /= Invalid_Node_Access then
               Min_Pushed_Back_Index := Tree.Get_Sequential_Index (First);
               exit;
            end if;
         end loop;
      end Push_Back_Tokens;

      procedure Delete_Shared_Stream
        (Label       : in     String;
         Config      : in out Configuration;
         First, Last : in     Syntax_Trees.Sequential_Index)
      --  Delete tokens First .. Last from Tree Shared_Stream; caller has
      --  already done deletes Config.Input_Stream.
      --  Config.Current_Shared_Token must be in First .. Last + 1. Leave
      --  Current_Shared_Token at Last + 1.
      is
         Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);

         procedure Find_First
         is begin
            if First = Tree.Get_Sequential_Index (Ref.Ref.Node) then
               return;

            elsif First < Tree.Get_Sequential_Index (Ref.Ref.Node) then
               loop
                  exit when Tree.Get_Sequential_Index (Ref.Ref.Node) = First;
                  Tree.Prev_Sequential_Terminal (Ref, Parse_Stream => Invalid_Stream_ID, Preceding => True);
               end loop;

            else
               raise Bad_Config;
            end if;
         end Find_First;

      begin
         if not Has_Space (Config.Ops, Ada.Containers.Count_Type (Last - First + 1)) then
            Super.Config_Full (Shared, "insert quote 3 " & Label, Parser_Index);
            raise Bad_Config;
         end if;

         Find_First;

         for I in First .. Last loop
            if not (Tree.Label (Ref.Ref.Node) in Terminal_Label) then
               --  It is exceedingly unlikely that the words in a real user string
               --  will match a grammar production (unless we are writing a code
               --  generator like WisiToken.Output_Ada, sigh). So we just abandon
               --  this.
               raise Invalid_Case;
            end if;

            Append
              (Config.Ops,
               (Delete, Shared.Tree.ID (Ref.Ref.Node),
                Tree.Get_Sequential_Index (Ref.Ref.Node)));

            Tree.Next_Sequential_Terminal (Ref, Following => True);
         end loop;
         Config.Current_Shared_Token := Ref.Ref;
      end Delete_Shared_Stream;

      procedure Finish
        (Label       : in     String;
         Config      : in out Configuration;
         First, Last : in     Base_Sequential_Index)
      --  Delete  tokens First .. Last from Config.Input_Stream and/or Tree Shared_Stream.
      --  Either First - 1 or Last + 1 should be a String_Literal.
      --  Config.Current_Shared_Token must be in First .. Last + 1. Leave
      --  Current_Shared_Token at Last + 1.
      is
         Adj_First : constant Sequential_Index :=
           (if First = Invalid_Sequential_Index
            then (if Last = Invalid_Sequential_Index
                  then raise Bad_Config
                  else Last)
            else First);

         Adj_Last  : constant Sequential_Index := (if Last = Invalid_Sequential_Index then First else Last);

         Last_Deleted : Base_Sequential_Index := Invalid_Sequential_Index;
      begin
         if Adj_Last < Adj_First then
            raise Bad_Config;
         end if;

         if Length (Config.Input_Stream) > 0 then
            Delete_Pushed_Back (Label, Config, Target_Index => Adj_Last + 1, Max_Index => Last_Deleted);
         end if;

         if Last_Deleted = Adj_Last then
            --  First .. Last deleted from input_stream.
            null;
         else
            Delete_Shared_Stream
              (Label, Config,
               First =>
                 (if Last_Deleted = Invalid_Sequential_Index
                  then Adj_First
                  else Last_Deleted + 1),
               Last => Adj_Last);
         end if;

         Config.Error_Token  := Syntax_Trees.Invalid_Recover_Token;
         Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

         --  This is a guess, so we give it a nominal cost
         Config.Cost := Config.Cost + 1;

         --  Let explore do insert after these deletes.
         declare
            Target_Node : constant Valid_Node_Access := Parse.Peek_Current_First_Sequential_Terminal
              (Super, Shared, Config);
         begin
            Append
              (Config.Ops, (Fast_Forward, FF_First_Index | FF_Next_Index => Tree.Get_Sequential_Index (Target_Node)));

            if Config.Resume_Token_Goal - Check_Limit < Tree.Get_Sequential_Index (Target_Node)
            then
               Config.Resume_Token_Goal := Tree.Get_Sequential_Index (Target_Node) + Check_Limit;
               Super.Extend_Sequential_Index (Shared, Thru => Config.Resume_Token_Goal);

               if Trace_McKenzie > Extra then
                  Put_Line (Tree, Super.Stream (Parser_Index), "resume_token_goal:" & Config.Resume_Token_Goal'Image);
               end if;
            end if;
         end;

         Config.Strategy_Counts (String_Quote) := Config.Strategy_Counts (String_Quote) + 1;

         if Trace_McKenzie > Detail then
            Super.Put (Shared, "insert quote " & Label & " ", Parser_Index, Config);
         end if;
      exception
      when Bad_Config =>
         if Trace_McKenzie > Detail then
            Put_Line (Tree, Super.Stream (Parser_Index), "insert quote Bad_Config " & Label);
         end if;
         raise;
      end Finish;

   begin
      --  When the lexer finds an unbalanced quote, it inserts a virtual
      --  balancing quote at the same character position as the unbalanced
      --  quote, returning an empty string literal token there. The parser
      --  does not see that as an error; it encounters a syntax error
      --  before, at, or after that string literal.
      --
      --  Here we assume the parse error in Config.Error_Token is due to
      --  putting the balancing quote in the wrong place (although we also
      --  check that solution; see test_mckenzie_recover.adb
      --  String_Quote_6), and attempt to find a better place to put the
      --  balancing quote. Then all tokens from the balancing quote to the
      --  unbalanced quote are now part of a string literal, so delete them,
      --  leaving just the string literal created by Lexer error recovery.

      --  First we check to see if there is an unbalanced quote in the
      --  current line; if not, just return. Some lexer errors are for other
      --  unrecognized characters; see ada_mode-recover_bad_char.adb.
      --
      --  An alternate strategy is to treat the lexer error as a parse error
      --  immediately, but that complicates the parse logic.

      --  It is not possible to tell where the best place to put the
      --  balancing quote is, so we always try all reasonable places.
      declare
         Next_Line_Begin_Token : constant Valid_Node_Access := Tree.Line_Begin_Token
           (Current_Line + 1, Super.Stream (Parser_Index), Following_Source_Terminal => True);
         --  EOI if Current_Line is last line in source.
      begin
         Super.Extend_Sequential_Index (Shared, Next_Line_Begin_Token, Positive => True);

         if Tree.Byte_Region (Lexer_Error_Node, Trailing_Non_Grammar => False).First =
           Tree.Byte_Region (Config.Error_Token).First
         then
            --  The parse error token is the string literal at the lexer error.
            --
            --  case a: Insert the balancing quote somewhere before the error
            --  point. There is no way to tell how far back to put the balancing
            --  quote, so we just do one non-empty token. See
            --  test_mckenzie_recover.adb String_Quote_0. So far we have not found
            --  a test case for more than one token.
            declare
               New_Config            : Configuration := Config;
               Min_Pushed_Back_Index : Syntax_Trees.Sequential_Index;
            begin
               Push_Back_Tokens ("insert quote 4 a", New_Config, Min_Pushed_Back_Index);

               pragma Assert (not Config.Error_Token.Virtual);
               --  Error_Token can be a nonterm. ada_mode-recover_partial_09.adb
               Finish
                 ("a", New_Config,
                  First => Min_Pushed_Back_Index,
                  Last  => Tree.Get_Sequential_Index (Tree.First_Terminal (Config.Error_Token)) - 1);
               Local_Config_Heap.Add (New_Config);
            end;

            --  Note that it is not reasonable to insert a quote after the error
            --  in this case. If that were the right solution, the parser error
            --  token would not be the lexer repaired string literal, since a
            --  string literal would be legal here.

         elsif Tree.Element_ID (Config.Error_Token) = Invalid_Token_ID or else
           (Tree.Byte_Region (Lexer_Error_Node, Trailing_Non_Grammar => False).First <
              Tree.Byte_Region (Config.Error_Token).First and
              Tree.Get_Sequential_Index (Next_Line_Begin_Token) /= Invalid_Sequential_Index)
         then
            --  case b: the unbalanced quote is before the parse error token; see
            --  test_mckenzie_recover.adb String_Quote_2, String_Quote_5.
            --
            --  The missing quote belongs after the parse error token, before or
            --  at the end of the current line; try inserting it at the end of
            --  the current line.
            --
            --  The lexer repaired string literal may be in a reduced token on the
            --  stack.

            declare
               Matching : SAL.Peek_Type := 1;
            begin
               Find_Descendant_ID
                 (Tree, Config, Shared.Tree.ID (Lexer_Error_Node),
                  String_ID_Set (Shared.Tree.ID (Lexer_Error_Node)), Matching);

               if Matching = Config.Stack.Depth then
                  --  String literal is in a virtual nonterm; it is not from the lexer
                  --  error, so abandon this.
                  if Trace_McKenzie > Detail then
                     Put_Line (Tree, Super.Stream (Parser_Index), "insert quote b abandon; string literal in virtual");
                  end if;
                  return;
               end if;

               declare
                  New_Config : Configuration := Config;
               begin
                  String_Literal_In_Stack ("b", New_Config, Matching, Shared.Tree.ID (Lexer_Error_Node));

                  Finish
                    ("b", New_Config,
                     First => Shared.Tree.Get_Sequential_Index (Shared.Tree.First_Terminal (Config.Error_Token)),
                     Last  => Shared.Tree.Get_Sequential_Index (Next_Line_Begin_Token) - 1);
                  Local_Config_Heap.Add (New_Config);
               end;
            end;

         else
            --  The unbalanced quote is after the parse error token.

            --  case c: Assume a missing quote belongs immediately before the
            --  current token. See test_mckenzie_recover.adb String_Quote_3.
            declare
               New_Config : Configuration := Config;
            begin
               Finish
                 ("c", New_Config,
                  First => Shared.Tree.Get_Sequential_Index
                    (Parse.Peek_Current_First_Sequential_Terminal (Super, Shared, New_Config)),
                  Last  => Shared.Tree.Get_Sequential_Index (Lexer_Error_Node) - 1);
               Local_Config_Heap.Add (New_Config);
            end;

            --  case d: Assume a missing quote belongs somewhere farther before
            --  the current token; try one non-empty (as in case a above). See
            --  test_mckenzie_recover.adb String_Quote_4, String_Quote_6,
            --  test/ada_mode-recover_string_quote_1.adb.
            declare
               New_Config            : Configuration := Config;
               Min_Pushed_Back_Index : Syntax_Trees.Sequential_Index;
            begin
               Push_Back_Tokens ("insert quote 5 d", New_Config, Min_Pushed_Back_Index);

               Finish
                 ("d", New_Config,
                  First => Min_Pushed_Back_Index,
                  Last  => Tree.Get_Sequential_Index (Lexer_Error_Node) - 1);
               Local_Config_Heap.Add (New_Config);
            exception
            when SAL.Container_Empty =>
               --  From Stack.Pop
               raise Bad_Config;
            end;

            --  case e: Assume the actual error is an extra quote that terminates
            --  an intended string literal early, in which case there is a token
            --  on the stack containing the string literal that should be extended
            --  to the found quote. See test_mckenzie_recover.adb String_Quote_1.
            declare
               Matching : SAL.Peek_Type := 1;
            begin
               --  Lexer_Error_Node is a string literal; find a matching one.
               Find_Descendant_ID
                 (Tree, Config, Shared.Tree.ID (Lexer_Error_Node),
                  String_ID_Set (Shared.Tree.ID (Lexer_Error_Node)), Matching);

               if Matching = Config.Stack.Depth then
                  --  No matching string literal, so this case does not apply.
                  null;
               else
                  declare
                     New_Config : Configuration := Config;
                  begin
                     String_Literal_In_Stack ("e", New_Config, Matching, Shared.Tree.ID (Lexer_Error_Node));

                     Finish
                       ("e", New_Config,
                        First => Shared.Tree.Get_Sequential_Index (Config.Current_Shared_Token.Node),
                        Last  => Shared.Tree.Get_Sequential_Index (Lexer_Error_Node));
                     Local_Config_Heap.Add (New_Config);
                  end;
               end if;
            end;
         end if;
      end;
   exception
   when Invalid_Case =>
      null;

   when Bad_Config =>
      if Debug_Mode then
         raise;
      else
         null;
      end if;
   end Try_Insert_Quote_1;

   procedure Try_Insert_Quote
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Config            : in out Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
   is
      Tree : Syntax_Trees.Tree renames Shared.Tree;
      Current_Byte_Pos : constant Base_Buffer_Pos := Tree.Byte_Region (Config.Error_Token).Last;
   begin
      if Config.String_Quote_Checked_Byte_Pos /= Invalid_Buffer_Pos and then
        Current_Byte_Pos <= Config.String_Quote_Checked_Byte_Pos
      then
         return;
      else
         declare
            use Bounded_Streams;
            use Syntax_Trees;

            Current_Line : constant Base_Line_Number_Type :=
              (if Config.Input_Stream.First = No_Element
               then
                 (if Config.Current_Shared_Token.Node /= Invalid_Node_Access
                  then Tree.Line_Region (Config.Current_Shared_Token, Trailing_Non_Grammar => True).First
                  else Invalid_Line_Number)
               elsif not Config.Error_Token.Virtual and then
                 Config.Error_Token.Node = Config.Input_Stream (Config.Input_Stream.First)
               then Tree.Line_At_Node (Super.Stream (Parser_Index), Config.Error_Token)
               else Invalid_Line_Number);

         begin
            if Current_Line = Invalid_Line_Number or Current_Line = Null_Line_Region.First or not
              (Config.String_Quote_Checked_Line = Invalid_Line_Number or else
                 Config.String_Quote_Checked_Line < Current_Line)
            then
               return;
            elsif Config.Current_Shared_Token = Invalid_Stream_Node_Ref then
               --  Current token is in Config.Input_Stream, shared is past EOI. ada_mode-recover_partial_15.adb
               return;
            end if;

            Config.String_Quote_Checked_Line     := Current_Line;
            Config.String_Quote_Checked_Byte_Pos := Current_Byte_Pos;

            --  Find a recovered string quote lexer_error on the same line.
            declare
               Term  : Stream_Node_Parents := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);
               Found : Boolean             := False;

               procedure Search (Forward : in Boolean)
               is begin
                  loop
                     exit when Term.Ref = Invalid_Stream_Node_Ref;
                     --  Invalid when EOI or SOI has an error; test_incremental.adb Preserve_Parse_Errors_1

                     for Err of Tree.Error_List (Term.Ref.Node) loop
                        if Err in Lexer_Error then
                           declare
                              Lex_Err : WisiToken.Lexer.Error renames Lexer_Error (Err).Error;
                           begin
                              if Lex_Err.Recover_Char (1) in ''' | '"' and Lex_Err.Recover_Char (2) = ASCII.NUL then
                                 Found := True;
                                 return;
                              end if;
                           end;
                        end if;
                     end loop;

                     exit when Tree.ID (Term.Ref.Node) =
                       (if Forward
                        then Tree.Lexer.Descriptor.EOI_ID
                        else Tree.Lexer.Descriptor.SOI_ID);

                     exit when Tree.Line_At_Node (Term, Super.Stream (Parser_Index)) /= Current_Line;

                     if Forward then
                        Tree.Next_Terminal (Term, Following => True);
                     else
                        Tree.Prev_Terminal (Term, Super.Stream (Parser_Index), Preceding => True);
                     end if;
                  end loop;
               end Search;

            begin
               if Term.Ref.Node = Invalid_Node_Access then
                  --  Invalid when Current_Shared_Token is an empty nonterm.
                  Tree.Next_Terminal (Term, Following => True);
               end if;

               Search (Forward => True);
               if not Found then
                  Term := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);
                  Tree.Prev_Terminal (Term, Super.Stream (Parser_Index), Preceding => True);
                  Search (Forward => False);
               end if;

               if not Found then
                  return;
               end if;

               if Tree.ID (Term.Ref.Node) not in Tree.Lexer.Descriptor.String_1_ID |
                 Tree.Lexer.Descriptor.String_2_ID
               then
                  return;
               end if;

               Try_Insert_Quote_1 (Super, Shared, Parser_Index, Current_Line, Term.Ref.Node, Config, Local_Config_Heap);
            end;
         end;
      end if;
   end Try_Insert_Quote;

   procedure Try_Delete_Input
     (Super             : in out Base.Supervisor;
      Shared            : in out Parser.Parser;
      Parser_Index      : in     SAL.Base_Peek_Type;
      Config            : in     Configuration;
      Local_Config_Heap : in out Config_Heaps.Heap_Type)
   is
      --  Try deleting (= skipping) the current shared input token.

      use Recover_Op_Arrays, Recover_Op_Array_Refs;

      Check_Limit : constant Syntax_Trees.Sequential_Index := Shared.Table.McKenzie_Param.Check_Limit;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Next_Element_Node : constant Syntax_Trees.Node_Access := Parse.Peek_Current_Element_Node (Shared.Tree, Config);

      Next_Node : constant Syntax_Trees.Node_Access := Shared.Tree.First_Terminal (Next_Element_Node);

      Next_Index : constant Syntax_Trees.Sequential_Index :=
        (if Next_Node = Syntax_Trees.Invalid_Node_Access
         then Syntax_Trees.Sequential_Index'Last
         else Shared.Tree.Get_Sequential_Index (Next_Node));

      Next_ID : constant Token_ID :=
        (if Next_Node = Syntax_Trees.Invalid_Node_Access
         then Invalid_Token_ID
         else Shared.Tree.ID (Next_Node));
   begin
      if  Next_Node = Syntax_Trees.Invalid_Node_Access then
         --  Current token is an empty nonterm; we don't delete that here. It
         --  can be deleted by Parse, if it can't be shifted.
         return;

      elsif Next_ID in Shared.Tree.Lexer.Descriptor.EOI_ID | Invalid_Token_ID then
         --  can't delete EOI
         return;

      elsif Length (Config.Ops) > 0 and then
        (Equal (Constant_Ref (Config.Ops, Last_Index (Config.Ops)), (Insert, Next_ID, Next_Index)) or
           --  Don't delete an ID we just inserted; waste of time, leads to
           --  infinite loop.

           Constant_Ref (Config.Ops, Last_Index (Config.Ops)).Op = Undo_Reduce
           --  Only need Undo_Reduce to Push_Back part of it or allow Insert;
           --  allowing delete gives redundant configs.
           --  ada_mode-recover_extra_end_loop.adb with incremental parse.
        )
      then
         return;
      end if;

      declare
         New_Config : Configuration := Config;

         function Matching_Push_Back return Boolean
         is begin
            for I in reverse First_Index (New_Config.Ops) .. Last_Index (New_Config.Ops) loop
               declare
                  Op : Recover_Op renames Recover_Op_Array_Refs.Variable_Ref (New_Config.Ops, I).Element.all;
               begin
                  exit when not (Op.Op in Undo_Reduce | Push_Back | Delete);
                  if Op = (Push_Back, Next_ID, Next_Index) then
                     return True;
                  end if;
               end;
            end loop;
            return False;
         end Matching_Push_Back;
      begin
         New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (Next_ID);

         New_Config.Error_Token            := Syntax_Trees.Invalid_Recover_Token;
         New_Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);

         New_Config.Strategy_Counts (Delete) := Config.Strategy_Counts (Delete) + 1;

         if Matching_Push_Back then
            --  We are deleting a push_back; cancel the push_back cost, to make
            --  this the same as plain deleting.
            New_Config.Cost := Natural'Max (Natural'First, New_Config.Cost - McKenzie_Param.Push_Back (Next_ID));
         end if;

         if Is_Full (New_Config.Ops) then
            Super.Config_Full (Shared, "delete", Parser_Index);
            raise Bad_Config;
         else
            Append (New_Config.Ops, (Delete, Next_ID, Next_Index));
         end if;

         Parse.Do_Delete (Shared.Tree, New_Config);

         declare
            Node : constant Syntax_Trees.Valid_Node_Access := Parse.Peek_Current_First_Sequential_Terminal
              (Super, Shared, New_Config);
            New_Next_Index : Syntax_Trees.Sequential_Index;
         begin
            New_Next_Index := Shared.Tree.Get_Sequential_Index (Node);
            if New_Config.Resume_Token_Goal - Check_Limit < New_Next_Index then
               New_Config.Resume_Token_Goal := New_Next_Index + Check_Limit;
               Super.Extend_Sequential_Index (Shared, New_Config.Resume_Token_Goal);
            end if;
         end;

         Local_Config_Heap.Add (New_Config);

         if Trace_McKenzie > Detail then
            Super.Put
              (Shared, "delete " & Image (Next_ID, Shared.Tree.Lexer.Descriptor.all), Parser_Index, New_Config);
         end if;
      end;
   end Try_Delete_Input;

   procedure Process_One
     (Super  : in out Base.Supervisor;
      Shared : in out Parser.Parser)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Parser.Language_Fixes_Access;
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;

      Descriptor : WisiToken.Descriptor renames Shared.Tree.Lexer.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : aliased Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.
   begin
      Super.Get (Shared, Parser_Index, Config);

      if Parser_Index = SAL.Base_Peek_Type'First then
         --  No more configs.
         return;
      end if;

      if Trace_McKenzie > Detail then
         Super.Put (Shared, "dequeue", Parser_Index, Config);
      end if;

      --  Fast_Forward; parse Insert, Delete in Config.Ops that have not
      --  been parsed yet. 'parse' here means adjusting Config.Stack and
      --  Current_Terminal_Index. Code in this file always parses when
      --  adding ops to Config (except as noted); Language_Fixes should use
      --  McKenzie_Recover.Insert, Delete instead.
      if Config.Current_Insert_Delete = 1 then
         --  Config is from Language_Fixes.

         Fast_Forward (Super, Shared, Parser_Index, Local_Config_Heap, Config);
         Super.Put (Shared, Parser_Index, Local_Config_Heap);
         return;
      end if;

      pragma Assert (Config.Current_Insert_Delete = 0);
      --  Config.Current_Insert_Delete > 0 is a programming error.

      if Shared.Tree.Element_ID (Config.Error_Token) /= Invalid_Token_ID then
         if Shared.Language_Fixes = null then
            null;
         else
            begin
               Shared.Language_Fixes (Super, Shared, Parser_Index, Local_Config_Heap, Config);
            exception
            when Invalid_Case =>
               if Debug_Mode then
                  raise SAL.Programmer_Error with "Language_Fixes raised Invalid_Case; should handle that locally";
               end if;
            end;

            --  The solutions enqueued by Language_Fixes should be lower cost than
            --  others (typically 0), so they will be checked first.
         end if;

         if Config.In_Parse_Action_Status.Label = Ok then
            --  Parse table Error action.
            --
            --  We don't clear Config.Error_Token here, because
            --  Language_Use_Minimal_Complete_Actions needs it. We only clear it
            --  when a parse results in no error (or a different error), or a
            --  push_back moves the Current_Token.
            null;

         else
            --  Assume "ignore in_parse_action error" is a viable solution. But
            --  give it a cost, so a solution provided by Language_Fixes is
            --  preferred.

            declare
               New_State : Unknown_State_Index;
            begin
               Config.Cost := Config.Cost + Table.McKenzie_Param.Ignore_Check_Fail;
               Config.Strategy_Counts (Ignore_Error) := Config.Strategy_Counts (Ignore_Error) + 1;

               declare
                  use Recover_Op_Arrays, Recover_Op_Array_Refs;
                  Last : constant SAL.Base_Peek_Type := Last_Index (Config.Ops);
               begin
                  if Last /= SAL.Invalid_Peek_Index and then
                    Constant_Ref (Config.Ops, Last).Op = Undo_Reduce and then
                    Constant_Ref (Config.Ops, Last).Nonterm = Shared.Tree.Element_ID (Config.Error_Token)
                  then
                     --  We are ignoring this undo_reduce.
                     Delete_Last (Config.Ops);
                  end if;
               end;

               --  finish reduce.
               Config.Stack.Pop (Config.In_Parse_Action_Token_Count);

               New_State := Goto_For (Table, Config.Stack.Peek.State, Shared.Tree.Element_ID (Config.Error_Token));

               if New_State = Unknown_State then
                  if Config.Stack.Depth = 1 then
                     --  Stack is empty, and we did not get Accept; really bad syntax got
                     --  us here; abandon this config. See ada_mode-recover_bad_char.adb.
                     Super.Put (Shared, Parser_Index, Local_Config_Heap);
                     return;
                  else
                     raise SAL.Programmer_Error with
                       "process_one found test case for new_state = Unknown; old state " &
                       Trimmed_Image (Config.Stack.Peek.State) & " nonterm " & Image
                         (Shared.Tree.Element_ID (Config.Error_Token), Descriptor);
                  end if;
               end if;

               Config.Stack.Push ((New_State, Config.Error_Token));

               --  We _don't_ clear Check_Status and Error_Token here; Check needs
               --  them, and sets them as appropriate.

               if Trace_McKenzie > Detail then
                  Super.Put (Shared, "ignore in_parse_action error and continue", Parser_Index, Config);
               end if;
            end;
         end if;
      end if;

      --  Call Check to see if this config succeeds.
      case Check (Super, Shared, Parser_Index, Config, Local_Config_Heap) is
      when Success =>
         Super.Success (Shared, Parser_Index, Config, Local_Config_Heap);
         return;

      when Abandon =>
         Super.Put (Shared, Parser_Index, Local_Config_Heap);
         return;

      when Continue =>
         null;

      end case;

      if Trace_McKenzie > Detail then
         Super.Put (Shared, "continuing", Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Shared.Tree, Super.Stream (Parser_Index), "stack: " & LR.Image (Config.Stack, Shared.Tree));
         end if;
      end if;

      --  Grouping these operations (push_back, delete, insert) ensures that
      --  there are no duplicate solutions found. We reset the grouping
      --  after each fast_forward.
      --
      --  We do delete before insert so Insert_Matching_Begin can operate on
      --  the new next token, before Fast_Forwarding past it.
      --
      --  All possible permutations will be explored.

      pragma Assert (Config.Stack.Depth > 0);

      Try_Insert_Terminal (Super, Shared, Parser_Index, Config, Local_Config_Heap);

      declare
         Valid : constant Boolean := Push_Back_Valid (Super, Shared, Config, Push_Back_Undo_Reduce => False);
         Reduce_To_Start : constant Boolean := Check_Reduce_To_Start (Super, Shared, Parser_Index, Config);
      begin
         if Valid and then
           (not Shared.Tree.Is_Empty_Nonterm (Config.Stack.Peek.Token) and
              --  We only allow Push_Back of empty nonterm from Language_Fixes;
              --  otherwise it is usually redundant with Undo_Reduce.
              not Reduce_To_Start)
              --  If Config reduces to the start nonterm, there's no point in Push_Back or Undo_Reduce.
         then
            Try_Push_Back (Super, Shared, Parser_Index, Config, Local_Config_Heap);
         end if;
      end;

      if Undo_Reduce_Valid (Super, Shared, Config) then
         Try_Undo_Reduce (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if None_Since_FF (Config.Ops, Insert) then
         Try_Delete_Input (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      --  See if there is a mismatched quote. The solution is to delete
      --  tokens, nominally replacing them with an expanded string literal.
      --  So we try this when it is ok to try delete.
      --
      --  This is run once per input line, independent of what other ops
      --  have been done. Compare to Config.String_Quote_Checked is done in
      --  Try_Insert_Quote.
      if Config.In_Parse_Action_Status.Label = Ok and
        (Descriptor.String_1_ID /= Invalid_Token_ID or Descriptor.String_2_ID /= Invalid_Token_ID) and
        None_Since_FF (Config.Ops, Insert)
      then
         Try_Insert_Quote (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      Super.Put (Shared, Parser_Index, Local_Config_Heap);
   exception
   when Invalid_Case =>
      --  Just abandon this config; tell Super we are done.
      Super.Put (Shared, Parser_Index, Local_Config_Heap);

   when E : Bad_Config =>
      if Debug_Mode then
         --  Tell the developer about this bug.
         Shared.Tree.Lexer.Trace.Put_Line ("Process_One: Bad_Config: " & Standard.Ada.Exceptions.Exception_Message (E));
         Shared.Tree.Lexer.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         raise;
      else
         --  Just abandon this config; tell Super we are done.
         Super.Put (Shared, Parser_Index, Local_Config_Heap);
      end if;

   when E : others =>
      Super.Put (Shared, Parser_Index, Local_Config_Heap);
      if Debug_Mode then
         raise;
      elsif Trace_McKenzie > Outline then
         Put_Line
           (Shared.Tree, Super.Stream (Parser_Index),
            "Process_One: unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
      end if;
   end Process_One;

end WisiToken.Parse.LR.McKenzie_Recover.Explore;
