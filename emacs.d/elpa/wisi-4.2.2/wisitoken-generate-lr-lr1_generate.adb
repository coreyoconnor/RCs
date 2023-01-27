--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
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
with WisiToken.Generate.LR1_Items;
package body WisiToken.Generate.LR.LR1_Generate is

   function LR1_Goto_Transitions
     (Set                     : in LR1_Items.Item_Set;
      Symbol                  : in Token_ID;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   with Pre => Set.Dot_IDs.Contains (Symbol),
     Post => not LR1_Goto_Transitions'Result.Set.Is_Empty
   --  'goto' from [dragon] algorithm 4.9.
   is
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;
   begin
      for Item of Set.Set loop
         declare
            Item_Tokens : Token_ID_Arrays.Vector renames Productions.Constant_Ref_RHS
              (Grammar, Item.Prod).Tokens;
         begin
            if Item.Dot in Item_Tokens.First_Index .. Item_Tokens.Last_Index then
               if Item_Tokens (Item.Dot) = Symbol and
                 --  We don't need a state with dot after EOI in the
                 --  accept production. EOI should only appear in the
                 --  accept production.
                 Symbol /= Descriptor.EOI_ID
               then
                  Goto_Set.Set.Insert
                    ((Item.Prod,
                      (if Item.Dot = Item_Tokens.Last_Index then No_Index else Item.Dot + 1),
                      Item.Lookaheads));
               end if;
            end if;
         end;
      end loop;

      return Closure (Goto_Set, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor;
      Hash_Table_Size         : in Positive := LR1_Items.Item_Set_Trees.Default_Rows)
     return LR1_Items.Item_Set_List
   --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure
   --  "items", with some optimizations.
   is
      use LR1_Items;

      First_State_Index : constant State_Index := 0;
      States_Found      : Integer              := 0;

      C               : LR1_Items.Item_Set_List; -- result
      C_Tree          : LR1_Items.Item_Set_Tree; -- for fast find
      States_To_Check : State_Index_Queues.Queue;
      --  [dragon] specifies 'until no more items can be added', but we use
      --  a queue to avoid checking unecessary states. Ada LR1 has over
      --  100,000 states, so this is a significant gain (reduced time from
      --  600 seconds to 40).

      I       : State_Index;
      Dot_IDs : Token_ID_Arrays.Vector;

      New_Item_Set : Item_Set := Closure
        ((Set            => Item_Lists.To_List
            ((Prod       => (Grammar.First_Index, 0),
              Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First_Index (No_Index_If_Empty => True),
              Lookaheads => To_Lookahead (Descriptor.EOI_ID))),
          Tree_Node      =>
            (State       => First_State_Index,
             others      => <>),
          others         => <>),
        Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
   begin
      C.Set_First_Last (First_State_Index, First_State_Index - 1);
      C_Tree.Set_Rows (Hash_Table_Size);

      Add (Grammar, New_Item_Set, C, C_Tree, Descriptor, C_Tree.Rows, Include_Lookaheads => True);

      States_To_Check.Put (First_State_Index);
      loop
         exit when States_To_Check.Is_Empty;
         I := States_To_Check.Get;

         if Trace_Generate_Table > Detail then
            Ada.Text_IO.Put ("Checking ");
            Put (Grammar, Descriptor, C (I), Show_Lookaheads => True, Show_Goto_List => True);
         end if;

         Dot_IDs := C (I).Dot_IDs;
         --  We can't iterate on C (I).Dot_IDs when the loop adds items to C;
         --  it might be reallocated to grow.

         for Symbol of Dot_IDs loop
            --  [dragon] has 'for each grammar symbol X', but LR1_Goto_Transitions
            --  rejects Symbol that is not in Dot_IDs, so we iterate over that.

            --  The input to LR1_Goto_Transitions is always a previous output, and
            --  always unique. The output may not be unique; we store that in C,
            --  and a search key in C_Tree for faster searching.

            New_Item_Set := LR1_Goto_Transitions
              (C (I), Symbol, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

            New_Item_Set.Tree_Node.State := C.Last_Index + 1;

            Compute_Key_Hash (New_Item_Set, C_Tree.Rows, Grammar, Descriptor, True);

            declare
               Found     : Boolean;
               Found_Ref : constant Item_Set_Trees.Constant_Reference_Type := C_Tree.Find_Or_Insert
                 (New_Item_Set.Tree_Node, Found);
            begin
               if not Found then
                  States_To_Check.Put (New_Item_Set.Tree_Node.State);

                  New_Item_Set.Dot_IDs := Get_Dot_IDs (Grammar, New_Item_Set.Set, Descriptor);

                  C.Append (New_Item_Set);

                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line
                       ("  adding state" & Unknown_State_Index'Image (C.Last_Index) & ": from state" &
                          Unknown_State_Index'Image (I) & " on " & Image (Symbol, Descriptor));
                     Put (Grammar, Descriptor, New_Item_Set, Show_Lookaheads => True);
                  end if;

                  C (I).Goto_List.Insert ((Symbol, C.Last_Index));
               else
                  States_Found := @ + 1;
                  --  If there's not already a goto entry between these two sets, create one.
                  if not Is_In ((Symbol, Found_Ref.State), Goto_List => C (I).Goto_List) then
                     C (I).Goto_List.Insert ((Symbol, Found_Ref.State));

                     if Trace_Generate_Table > Detail then
                        Ada.Text_IO.Put_Line
                          ("  adding goto on " & Image (Symbol, Descriptor) & " to state" & Found_Ref.State'Image);
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      if Trace_Time then
         declare
            Elements, Max_Row_Depth, Average_Row_Depth : Ada.Containers.Count_Type;
            Rows, Empty_Rows : Integer;
         begin
            C_Tree.Sizes (Elements, Rows, Max_Row_Depth, Average_Row_Depth, Empty_Rows);

            Ada.Text_IO.Put_Line
              ("LR1 hash table states:" & Elements'Image &
                 " rows:" & Rows'Image &
                 " max_row_depth:" & Max_Row_Depth'Image &
                 " average_row_depth:" & Average_Row_Depth'Image &
                 " empty_rows:" & Empty_Rows'Image &
                 " states_found:" & States_Found'Image);
         end;
      end if;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
      end if;

      return C;
   end LR1_Item_Sets;

   procedure Add_Actions
     (Item_Sets          : in     LR1_Items.Item_Set_List;
      Table              : in out Parse_Table;
      Grammar            : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor         : in     WisiToken.Descriptor;
      Declared_Conflicts : in out WisiToken.Generate.LR.Conflict_Lists.Tree;
      Unknown_Conflicts  : in out WisiToken.Generate.LR.Conflict_Lists.Tree;
      First_Nonterm_Set  : in     WisiToken.Token_Array_Token_Set;
      File_Name          : in     String;
      Ignore_Conflicts   : in     Boolean)
   is
      --  Add actions for all Item_Sets to Table.
   begin
      for Item_Set of Item_Sets loop
         Add_Actions
           (Item_Set, Table, Grammar, Descriptor, Declared_Conflicts, Unknown_Conflicts, First_Nonterm_Set, File_Name,
            Ignore_Conflicts);
      end loop;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Grammar_File_Name     : in     String;
      Error_Recover         : in     Boolean;
      Known_Conflicts       : in     Conflict_Lists.Tree              := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type              := Default_McKenzie_Param;
      Max_Parallel          : in     SAL.Base_Peek_Type               := 15;
      Parse_Table_File_Name : in     String                           := "";
      Include_Extra         : in     Boolean                          := False;
      Ignore_Conflicts      : in     Boolean                          := False;
      Recursion_Strategy    : in     WisiToken.Recursion_Strategy     := Full;
      Use_Cached_Recursions : in     Boolean                          := False;
      Recursions            : in out WisiToken.Generate.Recursions)
     return Parse_Table_Ptr
   is
      Time_Start            : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Add_Actions_Time      : Ada.Calendar.Time;
      Minimal_Actions_Time  : Ada.Calendar.Time;
      Collect_Conflict_Time : Ada.Calendar.Time;

      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate_Table > Detail;
      Ignore_Unknown_Conflicts : constant Boolean := Ignore_Conflicts or WisiToken.Trace_Generate_Table > Detail;
      Unused_Tokens            : constant Boolean := WisiToken.Generate.Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      Recursions_Time : Ada.Calendar.Time := Ada.Calendar.Clock;

      Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
        Compute_Minimal_Terminal_Sequences (Descriptor, Grammar, Grammar_File_Name);

      Minimal_Terminal_First : constant Token_Array_Token_ID :=
        Compute_Minimal_Terminal_First (Descriptor, Minimal_Terminal_Sequences);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
           (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Unknown_Conflicts    : Conflict_Lists.Tree;
      Known_Conflicts_Edit : Conflict_Lists.Tree := Known_Conflicts;

      Initial_Item_Sets_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   begin
      if not Use_Cached_Recursions or Recursions = Empty_Recursions then
         case Recursion_Strategy is
         when None =>
            null;

         when Partial =>
            Recursions := WisiToken.Generate.Compute_Partial_Recursion (Grammar, Descriptor);

         when Full =>
            Recursions := WisiToken.Generate.Compute_Full_Recursion (Grammar, Descriptor);
         end case;
      end if;

      Set_Grammar_Recursions (Recursions, Grammar);
      Recursions_Time := Ada.Calendar.Clock;

      if Trace_Time then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "compute kernels, recursion time:" &
              Duration'Image (Ada.Calendar."-" (Recursions_Time, Time_Start)));
         Ada.Text_IO.Put_Line
           ("initial item_sets time:" & Duration'Image (Ada.Calendar."-" (Recursions_Time, Initial_Item_Sets_Time)));
      end if;
      if Trace_Generate_Table + Trace_Generate_Minimal_Complete > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR1_Generate:");
         if Trace_Generate_Table > Detail then
            Ada.Text_IO.Put_Line ("Item_Sets:");
            LR1_Items.Put (Grammar, Descriptor, Item_Sets);
         end if;
         Ada.Text_IO.New_Line;
      end if;

      Table := new Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      Table.Error_Recover_Enabled := Error_Recover;

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

      else
         Table.McKenzie_Param := McKenzie_Param;
      end if;

      Table.Max_Parallel := Max_Parallel;

      Add_Actions
        (Item_Sets, Table.all, Grammar, Descriptor, Known_Conflicts_Edit, Unknown_Conflicts, First_Nonterm_Set,
         Grammar_File_Name, Ignore_Conflicts);

      if Trace_Time then
         Add_Actions_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("add_actions time:" & Duration'Image (Ada.Calendar."-" (Add_Actions_Time, Initial_Item_Sets_Time)));
      end if;

      for State in Table.States'Range loop
         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.Put_Line ("Set_Minimal_Complete_Actions:" & State_Index'Image (State));
         end if;
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State),
            LR1_Items.Filter (Item_Sets (State), Grammar, Descriptor, LR1_Items.In_Kernel'Access),
            Descriptor, Grammar, Nullable, Minimal_Terminal_Sequences, Minimal_Terminal_First);
      end loop;

      if Trace_Time then
         Minimal_Actions_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute minimal actions time:" & Duration'Image
              (Ada.Calendar."-" (Minimal_Actions_Time, Add_Actions_Time)));
      end if;

      if Trace_Time then
         Collect_Conflict_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute conflicts time:" & Duration'Image
              (Ada.Calendar."-" (Collect_Conflict_Time, Minimal_Actions_Time)));
      end if;

      if Parse_Table_File_Name /= "" then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, Parse_Table_File_Name, "LR1", Grammar, Recursions, Item_Sets, Known_Conflicts_Edit,
            Unknown_Conflicts, Descriptor, Include_Extra);
      end if;

      if Trace_Generate_Table > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Has_Empty_Production: " & Image (Has_Empty_Production, Descriptor));

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Minimal_Terminal_First:");
         for ID in Minimal_Terminal_First'Range loop
            Ada.Text_IO.Put_Line
              (Image (ID, Descriptor) & " =>" &
                 (if Minimal_Terminal_First (ID) = Invalid_Token_ID
                  then ""
                  else ' ' & Image (Minimal_Terminal_First (ID), Descriptor)));
         end loop;
      end if;

      Check_Conflicts
        ("LR1", Unknown_Conflicts, Known_Conflicts_Edit, Grammar_File_Name, Descriptor, Ignore_Unknown_Conflicts);

      WisiToken.Generate.Error := WisiToken.Generate.Error or (Unused_Tokens and not Ignore_Unused_Tokens);

      return Table;
   end Generate;

end WisiToken.Generate.LR.LR1_Generate;
