--  Abstract :
--
--  See spec
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

package body WisiToken.Parse.LR.McKenzie_Recover.Parse is

   procedure Compute_Nonterm
     (Tree    : in     Syntax_Trees.Tree;
      ID      : in     Token_ID;
      Stack   : in     Recover_Stacks.Stack;
      Tokens  : in out Syntax_Trees.Recover_Token_Array;
      Nonterm :    out Syntax_Trees.Virtual_Recover_Token)
   is
      use Syntax_Trees;

      First_Terminal_Set : Boolean := False;
   begin
      Nonterm :=
        (Virtual                   => True,
         ID                        => ID,
         Contains_Virtual_Terminal => False,
         others                    => <>);

      for I in Tokens'Range loop
         Tokens (I) := Stack.Peek (Tokens'Last - I + 1).Token;
      end loop;

      for T of Tokens loop
         Nonterm.Contains_Virtual_Terminal := @ or Tree.Contains_Virtual_Terminal (T);

         if not First_Terminal_Set then
            Nonterm.First_Terminal := Tree.First_Terminal (T);
            if Nonterm.First_Terminal /= Syntax_Trees.Invalid_Node_Access then
               First_Terminal_Set := True;
            end if;
         end if;
      end loop;

      for T of reverse Tokens loop
         Nonterm.Last_Terminal := Tree.Last_Terminal (T);
         if Nonterm.Last_Terminal /= Syntax_Trees.Invalid_Node_Access then
            exit;
         end if;
      end loop;
   end Compute_Nonterm;

   function Reduce_Stack
     (Shared_Parser : in out LR.Parser.Parser;
      Stack         : in out Recover_Stacks.Stack;
      Action        : in     Reduce_Action_Rec;
      Nonterm       :    out Syntax_Trees.Recover_Token)
     return Syntax_Trees.In_Parse_Actions.Status
   is
      use all type Syntax_Trees.In_Parse_Actions.In_Parse_Action;
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Syntax_Trees.Recover_Token_Array (1 .. Last);

      In_Parse_Action : constant Syntax_Trees.In_Parse_Actions.In_Parse_Action := Shared_Parser.Get_In_Parse_Action
        (Action.Production);
   begin
      if Stack.Depth <= Last then
         raise Bad_Config;
      end if;

      Compute_Nonterm (Shared_Parser.Tree, Action.Production.LHS, Stack, Tokens, Nonterm);

      if In_Parse_Action = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant Syntax_Trees.In_Parse_Actions.Status :=
           In_Parse_Action (Shared_Parser.Tree, Nonterm, Tokens, Recover_Active => True)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));

               --  We don't pop the stack for error, so Language_Fixes and other
               --  recover ops can access the child tokens.
            end if;
         end return;
      end if;
   end Reduce_Stack;

   procedure Left_Breakdown
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in out Bounded_Streams.List)
   is
      use Bounded_Streams;
      use Syntax_Trees;

      --  Derived from Syntax_Trees.Left_Breakdown. We do not delete virtual
      --  terminals, to allow insert before, delete.

      Cur          : Cursor            := Stream.First;
      To_Delete    : Cursor            := Cur;
      Node         : Valid_Node_Access := Stream (Cur);
      Next_I       : Positive_Index_Type;
      Next_Sibling : Cursor            := No_Element;
   begin
      loop
         Next_I := Positive_Index_Type'Last;

         for I in reverse 2 .. Tree.Child_Count (Node) loop
            declare
               Child : constant Valid_Node_Access := Tree.Child (Node, I);
            begin
               if Tree.Child_Count (Child) > 0 or Tree.Label (Child) in Terminal_Label then
                  Next_I := I;
               end if;

               Cur := Stream.Insert (Element => Child, Before => Cur);

               Next_Sibling := Cur;

               --  We don't do Tree.Clear_Parent (Child) here, because we are not
               --  editing the syntax tree. If this config succeeds,
               --  Tree.Left_Breakdown will be called.
            end;
         end loop;

         if Tree.Child_Count (Tree.Child (Node, 1)) > 0 or Tree.Label (Tree.Child (Node, 1)) in Terminal_Label then
            Next_I := 1;
         else
            --  Tree.Child (Node, 1) is an empty nonterm.
            if Next_I = Positive_Index_Type'Last then
               --  Node is an empty nonterm; move to first sibling below.
               null;

            elsif Next_I > 2 then
               --  First non_empty is in Node.Children (Next_I); delete other empty
               --  nonterms that were added to the stream.
               for I in 2 .. Next_I - 1 loop
                  declare
                     To_Delete_2 : Cursor := Cur;
                  begin
                     Stream.Next (Cur);
                     Stream.Delete (To_Delete_2);
                  end;
               end loop;
               pragma Assert (Stream.Element (Cur) = Tree.Child (Node, Next_I));

               --  Delete the nonterm that we were breaking down, and record the one
               --  we are now breaking down for deletion.
               raise SAL.Not_Implemented with "found test case for Next_I > 2";
               --  Stream.Delete (To_Delete);
               --  To_Delete := Cur;
            end if;
         end if;

         if Next_I = Positive_Index_Type'Last then
            --  Node is an empty nonterm; move to next sibling, which was pushed
            --  on Stream in an earlier loop. test_incremental.adb Recover_04.
            pragma Assert (Next_Sibling /= No_Element);
            Cur  := Next_Sibling;
            Node := Stream (Cur);
            if Tree.Label (Node) in Terminal_Label or Cur = To_Delete then
               null;
            else
               --  Delete the nonterm that we were breaking down, and record the one
               --  we are now breaking down for deletion.
               Stream.Delete (To_Delete);
               To_Delete := Cur;
            end if;
         else
            Node := Tree.Child (Node, Next_I);
         end if;

         if Tree.Label (Node) in Terminal_Label then
            if Next_I = 1 then
               Stream.Insert (Element => Node, Before => Cur);
            else
               --  already inserted above
               null;
            end if;

            Stream.Delete (To_Delete);
            exit;
         end if;
      end loop;

   exception
   when SAL.Container_Full =>
      --  From Stream.Insert. We don't raise Bad_Config here, because the
      --  only problem is the subtree is too large.
      raise Invalid_Case;
   end Left_Breakdown;

   procedure Current_Token_ID_Peek_3
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in     Configuration;
      Tokens        :            out Token_ID_Array_1_3)
   --  Return the current token from Config in Tokens (1). Return the two
   --  following tokens in Tokens (2 .. 3).
   is
      use Recover_Op_Arrays;
      use Recover_Op_Array_Refs;
      use Syntax_Trees;

      --  We can't use Parse.Get_Current_Token, Parse.Next_Token, because we
      --  are not allowed to modify Config. In particular, we cannot apply
      --  Breakdown to Config.Input_Stream on the second and third tokens,
      --  which is required if Delete applies to those tokens.
      --
      --  We can extract the first three IDs without modifying anything.
      --
      --  Fast_Forward applies the Delete to the current token, so we should
      --  not have to here, but rather than rely on that and special case it
      --  here, we handle all three in the same way.

      Tokens_Last           : Integer               := 0;
      Current_Insert_Delete : SAL.Base_Peek_Type    := Config.Current_Insert_Delete;
      Peek_State            : Peek_Sequential_State := Peek_Sequential_Start (Super, Shared_Parser, Config);
      Inc_Shared_Token      : Boolean               := True;
   begin
      loop -- three tokens, skip Op = Delete
         declare
            Next_Node : constant Valid_Node_Access := Peek_Sequential_Terminal (Peek_State);
            Index     : constant Sequential_Index  := Shared_Parser.Tree.Get_Sequential_Index (Next_Node);
         begin
            --  Sequential_Index may have gaps, so we must extend on each token.
            Super.Extend_Sequential_Index (Shared_Parser, Index + 1);

            if Current_Insert_Delete /= No_Insert_Delete and then
              Token_Index (Constant_Ref (Config.Insert_Delete, Current_Insert_Delete)) =
              Shared_Parser.Tree.Get_Sequential_Index (Next_Node)
            then
               Inc_Shared_Token := False;
               declare
                  Op : Insert_Delete_Op renames Constant_Ref (Config.Insert_Delete, Current_Insert_Delete);
               begin
                  case Insert_Delete_Op_Label (Op.Op) is
                  when Insert =>
                     Tokens_Last          := @ + 1;
                     Tokens (Tokens_Last) := ID (Op);

                  when Delete =>
                     Peek_Next_Sequential_Terminal (Shared_Parser.Tree, Peek_State);
                  end case;

                  Current_Insert_Delete := @ + 1;

                  if Current_Insert_Delete > Last_Index (Config.Insert_Delete) then
                     Current_Insert_Delete := No_Insert_Delete;
                  end if;
               end;
            else
               Inc_Shared_Token     := True;
               Tokens_Last          := @ + 1;
               Tokens (Tokens_Last) := Shared_Parser.Tree.ID (Next_Node);
            end if;
         end;

         exit when Tokens (Tokens_Last) = Shared_Parser.Tree.Lexer.Descriptor.EOI_ID or Tokens_Last = 3;

         if Inc_Shared_Token then
            Peek_Next_Sequential_Terminal (Shared_Parser.Tree, Peek_State);
         end if;
      end loop;

      for I in Tokens_Last + 1 .. 3 loop
         Tokens (I) := Invalid_Token_ID;
      end loop;
   end Current_Token_ID_Peek_3;

   function Peek_Current_Element_Node
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use all type Bounded_Streams.Cursor;
   begin
      return
        (if Config.Input_Stream.First = Bounded_Streams.No_Element
         then Tree.Get_Node (Tree.Shared_Stream, Config.Current_Shared_Token.Element)
         else Config.Input_Stream (Config.Input_Stream.First));
   end Peek_Current_Element_Node;

   function Peek_Current_First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      if Config.Input_Stream.First /= No_Element then
         declare
            Result : constant Node_Access := First_Terminal (Tree, Config.Input_Stream);
         begin
            if Result /= Invalid_Node_Access then
               return Result;
            end if;
         end;
      end if;

      return Tree.First_Terminal (Config.Current_Shared_Token).Node;
   end Peek_Current_First_Terminal;

   function Peek_Current_First_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in     Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use Syntax_Trees;
      Result : constant Valid_Node_Access := Peek_Current_First_Terminal (Shared_Parser.Tree, Config);
   begin
      Super.Extend_Sequential_Index (Shared_Parser, Thru => Result, Positive => True);
      return Result;
   end Peek_Current_First_Sequential_Terminal;

   procedure First_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Ref           :    out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
      First_Term : Node_Access;
   begin
      Ref.Element := Ref.Stream.First;
      Ref.Node    := Invalid_Node_Access;

      loop
         exit when not Has_Element (Ref.Element);
         First_Term := Tree.First_Terminal (Ref.Stream.Element (Ref.Element));
         if First_Term = Invalid_Node_Access then
            --  Ref.Element is an empty nonterm; skip it
            null;
         else
            Base.Extend_Sequential_Index
              (Super, Shared_Parser,
               Thru     => First_Term,
               Positive => True);
            Ref.Node := Tree.First_Sequential_Terminal (Ref.Stream.Element (Ref.Element), Ref.Parents);
            exit when Ref.Node /= Invalid_Node_Access;
         end if;

         Ref.Stream.Next (Ref.Element);
      end loop;
   end First_Sequential_Terminal;

   function First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Bounded_Streams.List)
     return Syntax_Trees.Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Cur  : Cursor      := Stream.First;
      Node : Node_Access := Invalid_Node_Access;
   begin
      loop
         exit when not Has_Element (Cur);

         Node := Tree.First_Terminal (Stream (Cur));
         exit when Node /= Invalid_Node_Access;

         Stream.Next (Cur);
      end loop;
      return Node;
   end First_Terminal;

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      Ref.Element := Ref.Stream.First;
      loop
         exit when not Has_Element (Ref.Element);

         Ref.Node := Tree.First_Terminal (Ref.Stream.all (Ref.Element));
         exit when Ref.Node /= Invalid_Node_Access;

         Ref.Stream.Next (Ref.Element);
      end loop;
   end First_Terminal;

   procedure Last_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Ref           : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Tree  : Syntax_Trees.Tree renames Shared_Parser.Tree;

   begin
      Ref.Element := Ref.Stream.Last;
      Ref.Node    := Invalid_Node_Access;

      loop
         exit when not Has_Element (Ref.Element);

         Base.Extend_Sequential_Index
           (Super, Shared_Parser,
            Thru     => Tree.Last_Terminal (Ref.Stream.Element (Ref.Element)),
            Positive => True);
         Ref.Node := Tree.Last_Sequential_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
         exit when Ref.Node /= Invalid_Node_Access;

         Ref.Stream.Previous (Ref.Element);
      end loop;
   end Last_Sequential_Terminal;

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
   begin
      Tree.Next_Sequential_Terminal (Ref.Node, Ref.Parents);

      loop
         exit when Ref.Node /= Syntax_Trees.Invalid_Node_Access;
         Ref.Element := Ref.Stream.Next (Ref.Element);
         if Ref.Element = No_Element then
            Ref.Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Ref.Node := Tree.First_Sequential_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
      end loop;
   end Next_Sequential_Terminal;

   procedure Prev_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
   begin
      Tree.Prev_Sequential_Terminal (Ref.Node, Ref.Parents);

      loop
         exit when Ref.Node /= Syntax_Trees.Invalid_Node_Access;
         Ref.Element := Ref.Stream.Previous (Ref.Element);
         if Ref.Element = No_Element then
            Ref.Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Ref.Node := Tree.Last_Sequential_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
      end loop;
   end Prev_Sequential_Terminal;

   procedure Do_Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use Syntax_Trees;
      use all type Bounded_Streams.Cursor;
   begin
      --  Handle skip empty nonterms, breakdown of non-empty nonterms.
      --  Config.Input_Stream can end in an empty nonterm;
      --  ada_mode-interactive_02.adb.
      loop
         if Config.Input_Stream.First = Bounded_Streams.No_Element then
            if Tree.Label (Config.Current_Shared_Token.Element) in Terminal_Label then
               Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
               return;
            else
               --  Current_Shared_Token needs Breakdown; move it to Config.Input_Stream.
               Config.Input_Stream.Append
                 (Tree.Get_Node (Config.Current_Shared_Token.Stream, Config.Current_Shared_Token.Element));
               Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
            end if;

         else
            declare
               Next_Node : constant Valid_Node_Access := Config.Input_Stream (Config.Input_Stream.First);
            begin
               if Tree.Label (Next_Node) in Terminal_Label then
                  Config.Input_Stream.Delete_First;
                  return;

               elsif Tree.Is_Empty_Nonterm (Next_Node) then
                  Config.Input_Stream.Delete_First;

               else
                  Left_Breakdown (Tree, Config.Input_Stream);
               end if;
            end;
         end if;
      end loop;
   end Do_Delete;

   function Get_Current_Token
     (Super                   : in out Base.Supervisor;
      Shared_Parser           : in out LR.Parser.Parser;
      Config                  : in out Configuration;
      Inc_Shared_Stream_Token :    out Boolean;
      Inc_Input_Stream_Token  :    out Boolean)
     return Syntax_Trees.Recover_Token
   --  Return the current token from Config. If a Delete op applies,
   --  Config is updated to reflect the delete. Otherwise Config is not
   --  changed; calling Get_Current_Token again on (a copy of) Config
   --  will return the same token as this call.
   --
   --  Use Peek_Current_Token_ID if Config may not change at all.
   --
   --  Inc_*_Token are for Next_Token.
   is
      use Recover_Op_Arrays;
      use Recover_Op_Array_Refs;
      use Syntax_Trees;
      use all type Bounded_Streams.Cursor;
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      loop -- Op = Delete requires loop
         if Config.Current_Insert_Delete /= No_Insert_Delete and then
           (declare
               Current_Node : constant Syntax_Trees.Valid_Node_Access := Peek_Current_First_Sequential_Terminal
                 (Super, Shared_Parser, Config);
            begin
               Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete)) =
                 Tree.Get_Sequential_Index (Current_Node))
         then
            declare
               Op : Insert_Delete_Op renames Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label (Op.Op) is
               when Insert =>
                  Inc_Shared_Stream_Token := False;
                  Inc_Input_Stream_Token  := False;
                  return (Virtual                   => True,
                          ID                        => ID (Op),
                          Contains_Virtual_Terminal => True,
                          others                    => <>);

               when Delete =>
                  pragma Assert (Is_Terminal (Op.Del_ID, Tree.Lexer.Descriptor.all));

                  Do_Delete (Tree, Config);

                  Config.Current_Insert_Delete := @ + 1;

                  if Config.Current_Insert_Delete > Last_Index (Config.Insert_Delete) then
                     Config.Current_Insert_Delete := No_Insert_Delete;
                     Clear (Config.Insert_Delete);
                  end if;
               end case;
            end;

         elsif Config.Input_Stream.First /= Bounded_Streams.No_Element then
            Inc_Shared_Stream_Token := False;
            Inc_Input_Stream_Token  := True;
            return Tree.Get_Recover_Token (Config.Input_Stream (Config.Input_Stream.First));

         else
            Inc_Shared_Stream_Token := True;
            return Tree.Get_Recover_Token (Config.Current_Shared_Token);
         end if;
      end loop;
   end Get_Current_Token;

   procedure Next_Token
     (Super                   :         in out Base.Supervisor;
      Shared_Parser           :         in out LR.Parser.Parser;
      Config                  : aliased in out Configuration;
      Inc_Shared_Stream_Token :         in     Boolean;
      Inc_Input_Stream_Token  :         in     Boolean)
   --  Increment the appropriate "current token" index in Config.
   --  Inc_*_Token are from Get_Current_Token.
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
      use all type Bounded_Streams.Cursor;
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      if Last_Index (Config.Insert_Delete) > 0 and then
        Config.Current_Insert_Delete = Last_Index (Config.Insert_Delete)
      then
         Config.Current_Insert_Delete := No_Insert_Delete;
         Clear (Config.Insert_Delete);
      else
         declare
            Current_Node : constant Syntax_Trees.Valid_Node_Access := Peek_Current_First_Sequential_Terminal
              (Super, Shared_Parser, Config);
         begin
            if Config.Current_Insert_Delete /= No_Insert_Delete and then
              Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete + 1)) =
              Tree.Get_Sequential_Index (Current_Node)
            then
               Config.Current_Insert_Delete := @ + 1;

            else
               if Config.Input_Stream.First = Bounded_Streams.No_Element then
                  if Inc_Shared_Stream_Token then
                     Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
                  end if;
               else
                  if Inc_Input_Stream_Token then
                     Config.Input_Stream.Delete_First;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Next_Token;

   function Parse_One_Item
     (Super             :         in out Base.Supervisor;
      Shared_Parser     :         in out LR.Parser.Parser;
      Parser_Index      :         in     SAL.Peek_Type;
      Parse_Items       : aliased in out Parse_Item_Arrays.Vector;
      Parse_Item_Index  :         in     Positive;
      Shared_Token_Goal :         in     Syntax_Trees.Base_Sequential_Index;
      Trace_Prefix      :         in     String)
     return Boolean
   --  Perform parse actions on Parse_Items (Parse_Item_Index), until it
   --  encounters an error (return False) or Shared_Token_Goal is shifted
   --  (return True), or if Shared_Token_Goal is
   --  Invalid_Sequential_Index, until Item.Config.Insert_Delete is
   --  empty.
   --
   --  We return Boolean, not Status, because Abandon and Continue
   --  are up to the caller.
   --
   --  If any actions have conflicts, append the conflict configs and actions to
   --  Parse_Items.

   is
      use Parse_Item_Arrays;
      use Recover_Op_Arrays;
      use all type Syntax_Trees.In_Parse_Actions.Status_Label;

      Tree       : WisiToken.Syntax_Trees.Tree renames Shared_Parser.Tree;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Descriptor : WisiToken.Descriptor renames Shared_Parser.Tree.Lexer.Descriptor.all;
      Table      : Parse_Table renames Shared_Parser.Table.all;

      Item        : Parse_Item renames Parse_Item_Array_Refs.Variable_Ref
        (Parse_Items, Parse_Item_Index).Element.all;
      Config      : Configuration renames Item.Config;
      Action_Cur  : Parse_Action_Node_Ptr renames Item.Action;
      Action      : Parse_Action_Rec;

      Inc_Shared_Stream_Token : Boolean;
      Inc_Input_Stream_Token  : Boolean;
      Current_Token           : Syntax_Trees.Recover_Token := Get_Current_Token
        (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

      procedure Get_Action
      is
         --  We use the incremental parse algorithm even if the main parse is
         --  full parse, because Push_Back places whole nonterms on
         --  Config.Input_Stream.
         --
         --  Same logic as in Parser.Get_Action, but this
         --  operates on Config.
      begin
         loop
            declare
               Current_State : constant State_Index := Config.Stack.Peek.State;
            begin
               if Is_Terminal (Tree.Element_ID (Current_Token), Descriptor) then
                  Action_Cur := Action_For (Table, Current_State, Tree.Element_ID (Current_Token));
                  Action     := Action_Cur.Item;
                  return;
               else
                  --  nonterminal.
                  declare
                     New_State : constant Unknown_State_Index := Goto_For
                       (Table, Current_State, Tree.Element_ID (Current_Token));

                     Dummy : Ada.Containers.Count_Type;
                     pragma Unreferenced (Dummy);
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
                           First_In_Current : constant Syntax_Trees.Node_Access := Shared_Parser.Tree.First_Terminal
                             (Current_Token);
                        begin
                           if First_In_Current = Syntax_Trees.Invalid_Node_Access then
                              --  Current_Token is an empty nonterm; skip it.
                              Next_Token
                                (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

                              Current_Token := Get_Current_Token
                                (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
                           else
                              Action_Cur := Action_For (Table, Current_State, Tree.ID (First_In_Current));
                              Action     := Action_Cur.Item;

                              case Action.Verb is
                              when Shift =>
                                 if Config.Input_Stream.Length = 0 then
                                    --  Current_Token is from Shared_Stream. We can't do Breakdown in
                                    --  Shared_Stream; that might invalidate other Config.Current_Token.
                                    --  So add token to Config.Input_Stream, then breakdown.
                                    Config.Input_Stream.Append (Current_Token.Element_Node);
                                    Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
                                 end if;

                                 Left_Breakdown (Tree, Config.Input_Stream);

                                 Current_Token := Get_Current_Token
                                   (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

                                 if Trace_McKenzie > Extra then
                                    Trace.Put_Line
                                      (Trace_Prefix & ": breakdown; input_stream: " & LR.Image
                                         (Config.Input_Stream, Tree));
                                    Trace.Put_Line (" ... current_token: " & Shared_Parser.Tree.Image (Current_Token));
                                 end if;
                                 return;

                              when Accept_It | Reduce =>
                                 return;

                              when Error =>
                                 --  We don't do Undo_Reduce here; Explore will do that with an appropriate cost.
                                 return;
                              end case;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Get_Action;

   begin
      if Trace_McKenzie > Detail then
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Trace_Prefix & ": stack: " & LR.Image (Config.Stack, Tree));
            if Config.Current_Insert_Delete /= No_Insert_Delete then
               Put_Line (Tree, Super.Stream (Parser_Index), Trace_Prefix & ": Insert_Delete: " &
                           Image (Config.Insert_Delete, Descriptor));
            end if;
            if Config.Input_Stream.Length > 0 then
               Put_Line (Tree, Super.Stream (Parser_Index), Trace_Prefix & ": input_stream: " &
                           LR.Image (Config.Input_Stream, Tree));
            end if;
         end if;

         if Shared_Token_Goal /= Syntax_Trees.Invalid_Sequential_Index then
            Put_Line (Tree, Super.Stream (Parser_Index), Trace_Prefix & ": Shared_Token_Goal :" &
                        Shared_Token_Goal'Image);
         end if;
      end if;

      Item.Parsed := True;

      if Action_Cur = null then
         --  Item is original Config; else Item is from a conflict
         Get_Action;
      else
         Action := Action_Cur.Item;
      end if;

      loop
         declare
            Conflict : constant Parse_Action_Node_Ptr := (if Action_Cur = null then null else Action_Cur.Next);
         begin
            --  We don't loop on Conflict here; if Conflict.Next is non null, it
            --  will be enqueued when Conflict is parsed.
            if Conflict /= null then
               if Is_Full (Parse_Items) then
                  if Trace_McKenzie > Outline then
                     Put_Line (Tree, Super.Stream (Parser_Index),
                               Trace_Prefix & ": too many conflicts; abandoning");
                     raise Bad_Config;
                  end if;
               else
                  if Trace_McKenzie > Detail then
                     Put_Line
                       (Tree, Super.Stream (Parser_Index), Trace_Prefix & ":" & State_Index'Image
                          (Config.Stack.Peek.State) & ": add conflict " &
                          Image (Conflict.Item, Descriptor));
                  end if;

                  Append
                    (Parse_Items,
                     (Config, Conflict,
                      Parsed       => False,
                      Shift_Count  => Item.Shift_Count,
                      Reduce_Count => Item.Reduce_Count));
               end if;
            end if;
         end;

         if Trace_McKenzie > Extra then
            Put_Line
              (Tree, Super.Stream (Parser_Index), Trace_Prefix & ":" &
                 Config.Stack.Peek.State'Image &
                 ":" & Syntax_Trees.Image (Tree, Current_Token) &
                 " : " & Image (Action, Descriptor) &
                 (if Action.Verb = Reduce
                  then " via" & Config.Stack.Peek (SAL.Peek_Type (Action.Token_Count + 1)).State'Image
                  else ""));
         end if;

         case Action.Verb is
         when Shift =>
            Item.Shift_Count := Item.Shift_Count + 1;

            Config.Stack.Push ((Action.State, Syntax_Trees.Make_Rooted (Current_Token)));

            Next_Token (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
            Current_Token := Get_Current_Token
              (Super, Shared_Parser, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

         when Reduce =>
            Item.Reduce_Count := @ + 1;
            declare
               Nonterm : Syntax_Trees.Recover_Token;
            begin
               Config.In_Parse_Action_Status := Reduce_Stack (Shared_Parser, Config.Stack, Action, Nonterm);

               case Config.In_Parse_Action_Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Production.LHS);

                  if New_State = Unknown_State then
                     if Trace_McKenzie > Outline then
                        Base.Put (Super, Shared_Parser, Trace_Prefix & ": Unknown_State: ", Parser_Index, Config);
                        Put_Line
                          (Tree, Super.Stream (Parser_Index), Trace_Prefix & ": stack: " &
                             LR.Image (Config.Stack, Tree));
                     end if;

                     --  We can't just return False here; user must abandon this config.
                     --  This is not always an error; it could be from an inappropriate
                     --  language fix or it could be the wrong branch of a conflict.
                     --  ada_mode-recover_partial_15.adb.
                     raise Invalid_Case;
                  end if;

                  Config.Stack.Push ((New_State, Nonterm));

               when Syntax_Trees.In_Parse_Actions.Error =>
                  Config.Error_Token                 := Nonterm;
                  Config.In_Parse_Action_Token_Count := SAL.Base_Peek_Type (Action.Token_Count);
                  Success                            := False;

                  if Trace_McKenzie > Extra then
                     Put_Line
                       (Tree, Super.Stream (Parser_Index), Trace_Prefix & ": in_parse_action fail " &
                          Config.In_Parse_Action_Status.Label'Image);
                  end if;
               end case;
            end;

         when Error =>

            Config.Error_Token := Current_Token;
            Success            := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Verb = Accept_It or
           (if Shared_Token_Goal = Syntax_Trees.Invalid_Sequential_Index
            then Length (Config.Insert_Delete) = 0
            else
              (declare Term : constant Syntax_Trees.Node_Access := Peek_Current_First_Sequential_Terminal
                 (Super, Shared_Parser, Config);
               begin Shared_Parser.Tree.Get_Sequential_Index (Term) > Shared_Token_Goal));

         Get_Action;
      end loop;

      return Success;
   end Parse_One_Item;

   function Parse
     (Super             :         in out Base.Supervisor;
      Shared_Parser     :         in out LR.Parser.Parser;
      Parser_Index      :         in     SAL.Peek_Type;
      Parse_Items       : aliased    out Parse_Item_Arrays.Vector;
      Config            :         in     Configuration;
      Shared_Token_Goal :         in     Syntax_Trees.Base_Sequential_Index;
      All_Conflicts     :         in     Boolean;
      Trace_Prefix      :         in     String)
     return Boolean
   is
      use Parse_Item_Arrays;
      Last_Parsed : Natural;
      Success     : Boolean;
   begin
      Clear (Parse_Items);
      Append (Parse_Items, (Config, Action => null, Parsed => False, Shift_Count => 0, Reduce_Count => 0));

      --  Clear any errors; so they reflect the parse result.
      declare
         Config : Configuration renames Parse_Item_Array_Refs.Variable_Ref
           (Parse_Items, First_Index (Parse_Items)).Config;
      begin
         Config.Error_Token        := Syntax_Trees.Invalid_Recover_Token;
         Config.In_Parse_Action_Status := (Label => Syntax_Trees.In_Parse_Actions.Ok);
      end;

      Last_Parsed := First_Index (Parse_Items);
      loop
         --  Loop over initial config and any conflicts.
         Success := Parse_One_Item
           (Super, Shared_Parser, Parser_Index, Parse_Items, Last_Parsed, Shared_Token_Goal, Trace_Prefix);

         exit when Last_Index (Parse_Items) = Last_Parsed;

         exit when Success and not All_Conflicts;

         Last_Parsed := Last_Parsed + 1;
         if Trace_McKenzie > Detail then
            Put_Line (Shared_Parser.Tree, Super.Stream (Parser_Index), Trace_Prefix & ": parse conflict");
         end if;
      end loop;

      return Success;
   end Parse;

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
