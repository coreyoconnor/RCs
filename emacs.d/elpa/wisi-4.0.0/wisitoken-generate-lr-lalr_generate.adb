--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2022 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Calendar;
with Ada.Containers;
with Ada.Text_IO;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted;
package body WisiToken.Generate.LR.LALR_Generate is

   type Item_ID is record
      State : Unknown_State_Index                   := Unknown_State;
      LHS   : Token_ID                              := Invalid_Token_ID;
      RHS   : Productions.RHS_Arrays.Extended_Index := Productions.RHS_Arrays.No_Index;
      Dot   : Token_ID_Arrays.Extended_Index        := Token_ID_Arrays.No_Index;
   end record;

   function Image (Item : in Item_ID) return String
     is ("(" & Item.State'Image & ", " & Trimmed_Image ((Item.LHS, Item.RHS)) & ")");

   function Compare (Left, Right : in Item_ID) return SAL.Compare_Result
     is (if Left.State < Right.State then SAL.Less
         elsif Left.State > Right.State then SAL.Greater
         elsif Left.LHS < Right.LHS then SAL.Less
         elsif Left.LHS > Right.LHS then SAL.Greater
         elsif Left.RHS < Right.RHS then SAL.Less
         elsif Left.RHS > Right.RHS then SAL.Greater
         elsif Left.Dot < Right.Dot then SAL.Less
         elsif Left.Dot > Right.Dot then SAL.Greater
         else SAL.Equal);

   package Item_ID_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted (Item_ID, Compare);

   type Item_Map is record
      From : Item_ID;
      To   : Item_ID_Lists.List;
   end record;

   function Compare (Left, Right : in Item_Map) return SAL.Compare_Result
     is (Compare (Left.From, Right.From));

   package Propagation_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted (Item_Map, Compare);

   function Item_Ref
     (Kernels : in out LR1_Items.Item_Set_List;
      ID      : in     Item_ID)
     return LR1_Items.Item_Lists.Variable_Reference_Type
     is (LR1_Items.Item_Lists.Variable_Ref
           (LR1_Items.Find (Prod => (ID.LHS, ID.RHS), Dot => ID.Dot, Set => Kernels (ID.State))));

   function Propagate_Lookahead (Descriptor : in WisiToken.Descriptor) return LR1_Items.Lookahead
   is begin
      return LR1_Items.To_Lookahead (Descriptor.Last_Lookahead);
   end Propagate_Lookahead;

   ----------
   --  Debug output

   procedure Put (Propagations : in Propagation_Lists.List)
   is
      use Item_ID_Lists;
   begin
      for Map of Propagations loop
         Ada.Text_IO.Put_Line ("From " & Image (Map.From));

         for ID of Map.To loop
            Ada.Text_IO.Put_Line ("To   " & Image (ID));
         end loop;
      end loop;
   end Put;

   ----------
   --  Generate utils

   function LALR_Goto_Transitions
     (Kernel            : in LR1_Items.Item_Set;
      Symbol            : in Token_ID;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   is
      use Token_ID_Arrays;
      use LR1_Items;
      use LR1_Items.Item_Lists;

      Goto_Set : Item_Set;
   begin
      for Item of Kernel.Set loop

         if Item.Dot /= No_Index then

            declare
               Tokens      : Token_ID_Arrays.Vector renames Productions.Constant_Ref_RHS
                 (Grammar, Item.Prod).Tokens;
               Dot      : constant Token_ID_Arrays.Cursor := Tokens.To_Cursor (Item.Dot);
               Dot_ID   : constant Token_ID               := Tokens (Dot);
               Next_Dot : constant Token_ID_Arrays.Cursor := Tokens.Next (Dot);
            begin
               --  If Symbol = EOF_Token, this is the start symbol accept
               --  production; don't need a kernel with dot after EOF.

               if (Dot_ID = Symbol and Symbol /= Descriptor.EOI_ID) and then
                 not Has_Element (Find (Item, Goto_Set))
               then
                  Goto_Set.Set.Insert
                    ((Prod       => Item.Prod,
                      Dot        => To_Index (Next_Dot),
                      Lookaheads => Item.Lookaheads));

                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line ("LALR_Goto_Transitions 1 " & Image (Symbol, Descriptor));
                     Put (Grammar, Descriptor, Goto_Set);
                  end if;
               end if;

               if Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                 First_Nonterm_Set (Dot_ID, Symbol)
               then
                  --  Find the production(s) that create Dot_ID with first token Symbol
                  --  and put them in.
                  for Prod of Grammar loop
                     for RHS_2_I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                        declare
                           P_ID       : constant Production_ID          := (Prod.LHS, RHS_2_I);
                           Tokens     : Token_ID_Arrays.Vector renames Prod.RHSs (RHS_2_I).Tokens;
                           Dot_2      : constant Token_ID_Arrays.Cursor := Tokens.First;
                           Next_Dot_2 : constant Token_ID_Arrays.Cursor := Tokens.Next (Dot_2);
                        begin
                           if (Dot_ID = Prod.LHS or First_Nonterm_Set (Dot_ID, Prod.LHS)) and
                             (Has_Element (Dot_2) and then Tokens (Dot_2) = Symbol)
                           then
                              if not Has_Element (Find (P_ID, To_Index (Next_Dot_2), Goto_Set)) then
                                 Goto_Set.Set.Insert
                                   ((Prod       => P_ID,
                                     Dot        => To_Index (Next_Dot_2),
                                     Lookaheads => Null_Lookahead));

                                 --  else already in goto set
                              end if;
                           end if;
                        end;
                     end loop;
                  end loop;
                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line ("LALR_Goto_Transitions 2 " & Image (Symbol, Descriptor));
                     Put (Grammar, Descriptor, Goto_Set);
                  end if;
               end if;
            end;
         end if; -- item.dot /= null
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LALR_Kernels
     (Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List
   is
      use all type Ada.Containers.Count_Type;
      use LR1_Items;

      First_State_Index : constant State_Index := 0;
      Kernels           : LR1_Items.Item_Set_List;
      Kernel_Tree       : LR1_Items.Item_Set_Tree; -- for fast find
      States_To_Check   : State_Index_Queues.Queue;
      Checking_State    : State_Index;

      First_Item_Set : Item_Set :=
        (Set               => Item_Lists.To_List
           ((Prod          => (Grammar.First_Index, 0),
             Dot           => Grammar (Grammar.First_Index).RHSs (0).Tokens.First_Index (No_Index_If_Empty => True),
             Lookaheads    => Null_Lookahead)),
         Tree_Node         =>
           (State          => First_State_Index,
            others         => <>),
         others            => <>);
   begin
      Kernels.Set_First_Last (First_State_Index, First_State_Index - 1);

      Add (Grammar, First_Item_Set, Kernels, Kernel_Tree, Descriptor, Kernel_Tree.Rows, Include_Lookaheads => False);

      States_To_Check.Put (First_State_Index);
      loop
         exit when States_To_Check.Is_Empty;
         Checking_State := States_To_Check.Get;

         if Trace_Generate_Table > Detail then
            Ada.Text_IO.Put ("Checking ");
            Put (Grammar, Descriptor, Kernels (Checking_State));
         end if;

         for Symbol in Descriptor.First_Terminal .. Descriptor.Last_Nonterminal loop
            --  LALR_Goto_Transitions does _not_ ignore Symbol if it is not in
            --  Item_Set.Dot_IDs, so we can't iterate on that here as we do in
            --  LR1_Generate.

            declare
               New_Item_Set : Item_Set := LALR_Goto_Transitions
                 (Kernels (Checking_State), Symbol, First_Nonterm_Set, Grammar, Descriptor);
            begin
               if New_Item_Set.Set.Length > 0 then

                  New_Item_Set.Tree_Node.State := Kernels.Last_Index + 1;

                  Compute_Key_Hash (New_Item_Set, Kernel_Tree.Rows, Grammar, Descriptor, Include_Lookaheads => False);

                  declare
                     Found      : Boolean;
                     Found_Ref  : constant Item_Set_Trees.Constant_Reference_Type := Kernel_Tree.Find_Or_Insert
                       (New_Item_Set.Tree_Node, Found);
                  begin
                     if not Found then
                        States_To_Check.Put (New_Item_Set.Tree_Node.State);

                        Kernels.Append (New_Item_Set);

                        if Trace_Generate_Table > Detail then
                           Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (Kernels.Last_Index));

                           Ada.Text_IO.Put_Line
                             ("  state" & Unknown_State_Index'Image (Checking_State) &
                                " adding goto on " & Image (Symbol, Descriptor) & " to state" &
                                Unknown_State_Index'Image (Kernels.Last_Index));
                        end if;

                        Kernels (Checking_State).Goto_List.Insert ((Symbol, Kernels.Last_Index));
                     else
                        --  If there's not already a goto entry between these two sets, create one.
                        if not Is_In ((Symbol, Found_Ref.State), Kernels (Checking_State).Goto_List) then
                           if Trace_Generate_Table > Detail then
                              Ada.Text_IO.Put_Line
                                ("  state" & Unknown_State_Index'Image (Checking_State) &
                                   " adding goto on " & Image (Symbol, Descriptor) & " to state" &
                                   Unknown_State_Index'Image (Found_Ref.State));

                           end if;

                           Kernels (Checking_State).Goto_List.Insert ((Symbol, Found_Ref.State));
                        end if;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
      end if;

      return Kernels;
   end LALR_Kernels;

   --  Add a propagation entry (if it doesn't already exist) from From in
   --  From_Set to To_Item.
   procedure Add_Propagation
     (From_Item    : in     LR1_Items.Item;
      From_State   : in     State_Index;
      To_Item      : in     LR1_Items.Item_Lists.Cursor;
      To_State     : in     State_Index;
      Propagations : in out Propagation_Lists.List)
   is
      use Propagation_Lists;
      use LR1_Items;
      use LR1_Items.Item_Lists;
      use Item_ID_Lists;

      To_Item_Ref : constant LR1_Items.Item_Lists.Constant_Reference_Type := Constant_Ref (To_Item);

      From_ID : constant Item_ID := (From_State, From_Item.Prod.LHS, From_Item.Prod.RHS, From_Item.Dot);
      To_ID   : constant Item_ID := (To_State, To_Item_Ref.Prod.LHS, To_Item_Ref.Prod.RHS, To_Item_Ref.Dot);

      From_Match : constant Propagation_Lists.Cursor := Propagations.Find ((From_ID, Item_ID_Lists.Empty_List));
   begin
      if not Has_Element (From_Match) then
         Propagations.Insert ((From_ID, To_List (To_ID)));

      else
         declare
            To_Match : constant Item_ID_Lists.Cursor := Constant_Ref (From_Match).To.Find (To_ID);
         begin
            if not Has_Element (To_Match) then
               Variable_Ref (From_Match).To.Insert (To_ID);
            end if;
         end;
      end if;
   end Add_Propagation;

   --  Calculate the lookaheads from Closure_Item for Source_Item.
   --  Source_Item must be one of the kernel items in Source_Set.
   --  Closure_Item must be an item in the lookahead closure of Source_Item for #.
   --
   --  Spontaneous lookaheads are put in Source_Item.Lookahead,
   --  propagated lookaheads in Propagations.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LR1_Items.Item;
      Source_Set   : in     LR1_Items.Item_Set;
      Closure_Item : in     LR1_Items.Item;
      Propagations : in out Propagation_Lists.List;
      Descriptor   : in     WisiToken.Descriptor;
      Grammar      : in     WisiToken.Productions.Prod_Arrays.Vector;
      Kernels      : in out LR1_Items.Item_Set_List)
   is
      use LR1_Items;
      use LR1_Items.Item_Lists;
      use Token_ID_Arrays;

      Spontaneous_Count : Integer := 0;
   begin
      if Trace_Generate_Table > Extra then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LR1_Items.Put (Grammar, Descriptor, Closure_Item);
         Ada.Text_IO.New_Line;
      end if;

      if Closure_Item.Dot = No_Index then
         return;
      end if;

      declare
         Tokens     : Token_ID_Arrays.Vector renames Productions.Constant_Ref_RHS
           (Grammar, Closure_Item.Prod).Tokens;
         Dot        : constant Token_ID_Arrays.Cursor := Tokens.To_Cursor (Closure_Item.Dot);
         ID         : constant Token_ID               := Tokens (Dot);
         Next_Dot   : constant Token_ID_Arrays.Cursor := Tokens.Next (Dot);
         Goto_State : constant Unknown_State_Index    := LR1_Items.Goto_State (Source_Set, ID);
      begin
         if Goto_State /= Unknown_State then
            declare
               To_Item : constant Item_Lists.Cursor :=
                 LR1_Items.Find (Closure_Item.Prod, To_Index (Next_Dot), Kernels (Goto_State));
            begin
               if Closure_Item.Lookaheads (Descriptor.Last_Lookahead) then
                  Add_Propagation
                    (From_Item    => Source_Item,
                     From_State   => Source_Set.Tree_Node.State,
                     To_Item      => To_Item,
                     To_State     => Goto_State,
                     Propagations => Propagations);
               end if;

               if Trace_Generate_Table > Extra then
                  Spontaneous_Count := Spontaneous_Count + 1;
                  Ada.Text_IO.Put_Line ("  spontaneous: " & Lookahead_Image (Closure_Item.Lookaheads, Descriptor));
               end if;

               LR1_Items.Include (Variable_Ref (To_Item), Closure_Item.Lookaheads, Descriptor);
            end;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (Propagations : in     Propagation_Lists.List;
      Kernels      : in out LR1_Items.Item_Set_List;
      Descriptor   : in     WisiToken.Descriptor)
   is
      --  In Propagations, update all To lookaheads from From lookaheads,
      --  recursively.
      More_To_Check : Boolean := True;
      Added_One     : Boolean;
   begin
      while More_To_Check loop

         More_To_Check := False;
         for Map of Propagations loop
            for ID of Map.To loop
               LR1_Items.Include
                 (Item_Ref (Kernels, ID), Item_Ref (Kernels, Map.From).Lookaheads, Added_One, Descriptor);

               More_To_Check := More_To_Check or Added_One;
            end loop;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LALR(1) lookaheads for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LALR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar                 : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production    : in     Token_ID_Set;
      First_Terminal_Sequence : in     Token_Sequence_Arrays.Vector;
      Kernels                 : in out LR1_Items.Item_Set_List;
      Descriptor              : in     WisiToken.Descriptor)
   is
      Closure      : LR1_Items.Item_Set;
      Propagations : Propagation_Lists.List;
   begin
      for Kernel of Kernels loop
         if Trace_Generate_Table > Detail then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LR1_Items.Put (Grammar, Descriptor, Kernel);
         end if;

         for Kernel_Item of Kernel.Set loop
            Closure := LR1_Items.Closure
              ((Set            => LR1_Items.Item_Lists.To_List
                  ((Prod       => Kernel_Item.Prod,
                    Dot        => Kernel_Item.Dot,
                    Lookaheads => Propagate_Lookahead (Descriptor))),
                others         => <>),
               Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

            for Closure_Item of Closure.Set loop
               Generate_Lookahead_Info
                 (Kernel_Item, Kernel, Closure_Item, Propagations, Descriptor, Grammar, Kernels);
            end loop;
         end loop;
      end loop;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Propagations:");
         Put (Propagations);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagations, Kernels, Descriptor);
   end Fill_In_Lookaheads;

   procedure Add_Actions
     (Kernels                 : in     LR1_Items.Item_Set_List;
      Grammar                 : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production    : in     Token_ID_Set;
      First_Terminal_Sequence : in     Token_Sequence_Arrays.Vector;
      First_Nonterm_Set       : in     Token_Array_Token_Set;
      Table                   : in out Parse_Table;
      Descriptor              : in     WisiToken.Descriptor;
      Declared_Conflicts      : in out WisiToken.Generate.LR.Conflict_Lists.Tree;
      Unknown_Conflicts       : in out WisiToken.Generate.LR.Conflict_Lists.Tree;
      File_Name               : in     String;
      Ignore_Conflicts        : in     Boolean := False)
   --  Add actions for all Kernels to Table.
   is
      Closure : LR1_Items.Item_Set;
   begin
      for Kernel of Kernels loop
         --  IMPROVEME: there are three "closure" computations that could
         --  probably be refactored to save computation; in
         --  LALR_Goto_Transitions, Fill_In_Lookaheads, and here.
         Closure := LR1_Items.Closure (Kernel, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

         Add_Actions
           (Closure, Table, Grammar, Descriptor, Declared_Conflicts, Unknown_Conflicts, First_Nonterm_Set, File_Name,
            Ignore_Conflicts);
      end loop;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Grammar_File_Name     : in     String;
      Known_Conflicts       : in     Conflict_Lists.Tree := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type := Default_McKenzie_Param;
      Max_Parallel          : in     SAL.Base_Peek_Type  := 15;
      Parse_Table_File_Name : in     String              := "";
      Include_Extra         : in     Boolean             := False;
      Ignore_Conflicts      : in     Boolean             := False;
      Partial_Recursion     : in     Boolean             := True;
      Use_Cached_Recursions : in     Boolean             := False;
      Recursions            : in out WisiToken.Generate.Recursions)
     return Parse_Table_Ptr
   is
      Time_Start           : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Table_Time           : Ada.Calendar.Time;
      Minimal_Actions_Time : Ada.Calendar.Time;

      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate_Table > Detail;
      Ignore_Unknown_Conflicts : constant Boolean := Ignore_Conflicts or WisiToken.Trace_Generate_Table > Detail;
      Unused_Tokens            : constant Boolean := WisiToken.Generate.Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      Recursions_Time : Ada.Calendar.Time;

      Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
        Compute_Minimal_Terminal_Sequences (Descriptor, Grammar, Grammar_File_Name);

      Minimal_Terminal_First : constant Token_Array_Token_ID :=
        Compute_Minimal_Terminal_First (Descriptor, Minimal_Terminal_Sequences);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Kernels : LR1_Items.Item_Set_List := LALR_Kernels (Grammar, First_Nonterm_Set, Descriptor);

      Unknown_Conflicts    : Conflict_Lists.Tree;
      Known_Conflicts_Edit : Conflict_Lists.Tree := Known_Conflicts;

   begin
      if not Use_Cached_Recursions or Recursions = Empty_Recursions then
         Recursions :=
           (if Partial_Recursion
            then WisiToken.Generate.Compute_Partial_Recursion (Grammar, Descriptor)
            else WisiToken.Generate.Compute_Full_Recursion (Grammar, Descriptor));
      end if;
      Set_Grammar_Recursions (Recursions, Grammar);
      Recursions_Time := Ada.Calendar.Clock;

      if Trace_Time then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "compute kernels, recursion time:" &
              Duration'Image (Ada.Calendar."-" (Recursions_Time, Time_Start)));
      end if;

      WisiToken.Generate.Error := False; -- necessary in unit tests; some previous test might have encountered an error.

      if Trace_Generate_Table + Trace_Generate_Minimal_Complete > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LALR_Generate");
      end if;

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First_Terminal_Sequence, Kernels, Descriptor);

      if Unused_Tokens then
         WisiToken.Generate.Error := not Ignore_Unused_Tokens;
         Ada.Text_IO.New_Line;
      end if;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LR1_Items.Put (Grammar, Descriptor, Kernels, Show_Lookaheads => True);
      end if;

      Table := new Parse_Table
        (State_First       => Kernels.First_Index,
         State_Last        => Kernels.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      if McKenzie_Param = Default_McKenzie_Param then
         --  Descriminants in Default are wrong
         Table.McKenzie_Param :=
           (First_Terminal              => Descriptor.First_Terminal,
            Last_Terminal               => Descriptor.Last_Terminal,
            First_Nonterminal           => Descriptor.First_Nonterminal,
            Last_Nonterminal            => Descriptor.Last_Nonterminal,
            Insert                      => (others => 0),
            Delete                      => (others => 0),
            Push_Back                   => (others => 0),
            Undo_Reduce                 => (others => 0),
            Minimal_Complete_Cost_Delta => Default_McKenzie_Param.Minimal_Complete_Cost_Delta,
            Fast_Forward                => Default_McKenzie_Param.Fast_Forward,
            Matching_Begin              => Default_McKenzie_Param.Matching_Begin,
            Ignore_Check_Fail           => Default_McKenzie_Param.Ignore_Check_Fail,
            Zombie_Limit                => Default_McKenzie_Param.Zombie_Limit,
            Check_Limit                 => Default_McKenzie_Param.Check_Limit,
            Check_Delta_Limit           => Default_McKenzie_Param.Check_Delta_Limit,
            Enqueue_Limit               => Default_McKenzie_Param.Enqueue_Limit);

         Table.Error_Recover_Enabled := False;

      else
         Table.McKenzie_Param        := McKenzie_Param;
         Table.Error_Recover_Enabled := True;
      end if;

      Table.Max_Parallel := Max_Parallel;

      Add_Actions
        (Kernels, Grammar, Has_Empty_Production, First_Terminal_Sequence, First_Nonterm_Set, Table.all,
         Descriptor, Known_Conflicts_Edit, Unknown_Conflicts, Grammar_File_Name, Ignore_Conflicts);

      if Trace_Time then
         Table_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute parse table time:" & Duration'Image (Ada.Calendar."-" (Table_Time, Recursions_Time)));
      end if;

      for State in Table.States'Range loop
         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.Put_Line ("Set_Minimal_Complete_Actions:" & State_Index'Image (State));
         end if;
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State), Kernels (State), Descriptor, Grammar, Nullable, Minimal_Terminal_Sequences,
            Minimal_Terminal_First);
      end loop;

      if Trace_Time then
         Minimal_Actions_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute minimal actions time:" & Duration'Image
              (Ada.Calendar."-" (Minimal_Actions_Time, Table_Time)));
      end if;

      if Parse_Table_File_Name /= "" then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, Parse_Table_File_Name, "LALR", Grammar, Recursions, Kernels, Known_Conflicts_Edit, Unknown_Conflicts,
            Descriptor, Include_Extra);
      end if;

      Check_Conflicts
        ("LALR", Unknown_Conflicts, Known_Conflicts_Edit, Grammar_File_Name, Descriptor, Ignore_Unknown_Conflicts);

      return Table;
   end Generate;

end WisiToken.Generate.LR.LALR_Generate;
