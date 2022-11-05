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
with Ada.Exceptions;
with Ada.Text_IO;
with System.Address_To_Access_Conversions;
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

   function LR1_Item_Sets_Single
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
   end LR1_Item_Sets_Single;

   function LR1_Item_Sets_Parallel
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor;
      Task_Count              : in System.Multiprocessors.CPU_Range;
      Hash_Table_Size         : in Positive := LR1_Items.Item_Set_Trees.Default_Rows)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure
      --  "items", with some optimizations.

      type Base_Worker_ID is range 0 .. Positive'Last;
      subtype Worker_ID is Base_Worker_ID range 1 .. Base_Worker_ID'Last;

      package Item_Set_Tree_Node_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
        (Positive, Item_Set_Tree_Node, (others => <>));

      package Worker_Array_State_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
        (Worker_ID, State_Index_Arrays.Vector, State_Index_Arrays.Empty_Vector);

      type Worker_Data is record
         --  All data produced by one worker from one state, except New_C,
         --  which doesn't need to be split by supervisor state.
         From_State          : State_Index := State_Index'Last;
         New_States          : State_Index_Arrays.Vector;
         Existing_Goto_Items : Goto_Item_Arrays.Vector;
         New_Goto_Items      : Goto_Item_Arrays.Vector;
      end record;

      package State_Array_Worker_Data is new SAL.Gen_Unbounded_Definite_Vectors
        (State_Index, Worker_Data, (others => <>));

      protected Supervisor is

         procedure Initialize
           (Worker_Count   : in     LR1_Item_Sets_Parallel.Worker_ID;
            First_Item_Set : in out Item_Set);

         entry Get
           (Worker_ID     : in     LR1_Item_Sets_Parallel.Worker_ID;
            Sets_To_Check :    out Item_Set_List;
            Keys_To_Store :    out Item_Set_Tree_Node_Arrays.Vector)
         with Pre => Sets_To_Check.Is_Empty and Keys_To_Store.Is_Empty;
         --  Set Sets_To_Check to new states to check, _not_ indexed by state;
         --  they may be discontinuous. Available when there are states to
         --  check, or when all states have been checked and all workers are
         --  inactive; then Sets_To_Check is empty.
         --
         --  If Sets_To_Check is not empty, Keys_To_Store contains keys from
         --  other workers to store in worker's C_Tree; increment active worker
         --  count.

         procedure Update
           (Worker_ID   : in     LR1_Item_Sets_Parallel.Worker_ID;
            New_C       : in out Item_Set_Arrays.Vector;
            Worker_Data : in out State_Array_Worker_Data.Vector);
         --  New_C: New states found by worker, indexed by worker new state
         --  number (1 origin); add to supervisor C. States are updated to supervisor
         --  state numbers; worker should add those to worker's C_Tree.
         --
         --  Worker_Data : Indexed by supervisor state number I. Contains:
         --
         --     New_States: Worker new state numbers for states derived from C (I);
         --  sets are in New_C.
         --
         --     Existing_Goto_Items: Gotos from C (I) to some state in supervisor
         --  C (which worker found in C_Tree); add to supervisor C (I).
         --
         --     New_Goto_Items: From C (I) to some state in New_C (given by
         --  worker new state number). Add to supervisor C (I).
         --
         --  Decrement active worker count.

         procedure Fatal_Error
           (Exception_ID : in Ada.Exceptions.Exception_Id;
            Message      : in String);
         --  Worker encountered an exception; record it for Done, decrement
         --  active worker count.

         entry Done
           (ID      : out Ada.Exceptions.Exception_Id;
            Message : out Ada.Strings.Unbounded.Unbounded_String);
         --  Available when all states have been checked, and all workers
         --  inactive.

         function Get_C return Item_Set_List;

      private
         C               : Item_Set_List; -- result
         C_Tree          : Item_Set_Tree; -- for fast find
         States_To_Check : State_Index_Queues.Queue;
         --  [dragon] specifies 'until no more items can be added', but we use
         --  a queue to avoid checking unecessary states. Ada LR1 has over
         --  100,000 states, so this is a significant gain (reduced time from
         --  600 seconds to 40).

         Worker_Count   : LR1_Item_Sets_Parallel.Worker_ID;
         Active_Workers : Natural := 0;
         Fatal          : Boolean := False;

         New_States_For_Worker : Worker_Array_State_Index_Arrays.Vector;
         --  Indexed by worker ID

         Error_ID       : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
         Error_Message  : Ada.Strings.Unbounded.Unbounded_String;

         Min_States_Get      : SAL.Peek_Type := 10;

         Net_Time            : Duration    := 0.0; -- Time spent in Get, Update.
         Found_States        : Integer     := 0;   -- States found in Update; counts duplicate states found by workers
         Summary_Last_Output : State_Index := 0;
      end Supervisor;

      function Image (Node_Ref : Item_Set_Trees.Variable_Reference_Type) return String
      is
         package Convert is new System.Address_To_Access_Conversions (Item_Set_Tree_Node);
      begin
         return Convert.To_Address (Convert.Object_Pointer (Node_Ref))'Image & ":" &
           Node_Ref.Hash'Image & ":" & Node_Ref.State'Image;
      end Image;

      protected body Supervisor is

         procedure Initialize
           (Worker_Count   : in     LR1_Item_Sets_Parallel.Worker_ID;
            First_Item_Set : in out Item_Set)
         is
            First_State_Index : constant State_Index       := First_Item_Set.Tree_Node.State;
         begin
            Supervisor.Worker_Count := Worker_Count;

            New_States_For_Worker.Set_First_Last (1, Worker_Count);

            C.Set_First_Last (First_State_Index, First_State_Index - 1);
            C_Tree.Set_Rows (Hash_Table_Size);

            First_Item_Set.Dot_IDs := Get_Dot_IDs (Grammar, First_Item_Set.Set, Descriptor);
            Compute_Key_Hash (First_Item_Set, C_Tree.Rows, Grammar, Descriptor, True);

            C.Append (First_Item_Set);
            C_Tree.Insert (First_Item_Set.Tree_Node, Duplicate => SAL.Error);

            States_To_Check.Put (First_State_Index);
         end Initialize;

         entry Get
           (Worker_ID     : in     LR1_Item_Sets_Parallel.Worker_ID;
            Sets_To_Check :    out Item_Set_List;
            Keys_To_Store :    out Item_Set_Tree_Node_Arrays.Vector)
         when Fatal or States_To_Check.Length > 0 or Active_Workers = 0
         is
            use all type Ada.Calendar.Time;

            Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            if States_To_Check.Length > 0 then
               for I in 1 ..
                 (if States_To_Check.Length / SAL.Peek_Type (Worker_Count) < Min_States_Get
                  then States_To_Check.Length
                  else States_To_Check.Length / SAL.Peek_Type (Worker_Count))
               loop
                  Sets_To_Check.Append (C (States_To_Check.Get));
               end loop;

               if not New_States_For_Worker (Worker_ID).Is_Empty then
                  Keys_To_Store.Set_Capacity
                    (New_States_For_Worker (Worker_ID).First_Index, New_States_For_Worker (Worker_ID).Last_Index);
                  for State of New_States_For_Worker (Worker_ID) loop
                     pragma Assert (C (State).Tree_Node.State = State);
                     Keys_To_Store.Append (C (State).Tree_Node);
                  end loop;
                  New_States_For_Worker (Worker_ID).Clear;
               end if;

               if Trace_Generate_Table > Detail then
                  Ada.Text_IO.Put
                    ("(worker" & Worker_ID'Image & ") Checking" & Sets_To_Check.Length'Image & " states");
                  for Set of Sets_To_Check loop
                     Ada.Text_IO.Put (Set.Tree_Node.State'Image);
                  end loop;
                  Ada.Text_IO.New_Line;
                  if Trace_Generate_Table > Extra then
                     for Set of Sets_To_Check loop
                        Put (Grammar, Descriptor, Set, Show_Lookaheads => False, Kernel_Only => True);
                     end loop;
                  end if;

                  Ada.Text_IO.Put
                    ("(worker" & Worker_ID'Image & ") storing" & Keys_To_Store.Length'Image & " states");
                  for Node of Keys_To_Store loop
                     Ada.Text_IO.Put (Node.State'Image);
                  end loop;
                  Ada.Text_IO.New_Line;
               end if;

               Active_Workers := @ + 1;

               Net_Time := @ + (Ada.Calendar.Clock - Time_Start);
            end if;

            if Trace_Time and C.Last_Index > Summary_Last_Output +
              (if C_Tree.Rows < 15_053 then 500 else 10_000) --  500 for ada_lite.wy, 10_000 for ada.wy
            then
               Ada.Text_IO.Put_Line
                 ("(super) time:" & Net_Time'Image &
                    " states:" & C.Last_Index'Image &
                    " States_To_Check:" & States_To_Check.Length'Image &
                    " Found_States:" & Found_States'Image);
               Summary_Last_Output := C.Last_Index;
            end if;
         end Get;

         procedure Update
           (Worker_ID   : in     LR1_Item_Sets_Parallel.Worker_ID;
            New_C       : in out Item_Set_Arrays.Vector;
            Worker_Data : in out State_Array_Worker_Data.Vector)
         is
            use all type Ada.Calendar.Time;
            Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

            State_Map : array (New_C.First_Index .. New_C.Last_Index) of State_Index;
            --  Indexed by worker new state number, contains super state number
         begin
            if Trace_Generate_Table > Detail then
               Ada.Text_IO.Put ("(super) Update from worker" & Worker_ID'Image & "; new states:");
               for Data of Worker_Data loop
                  for Node of Data.New_Goto_Items loop
                     Ada.Text_IO.Put
                       (Data.From_State'Image & "." & Image (Node.Symbol, Descriptor) & "." &
                          Trimmed_Image (Node.State));
                  end loop;
                  Ada.Text_IO.Put (" | ");
               end loop;
               Ada.Text_IO.New_Line;
            end if;

            for Worker_Data of Update.Worker_Data loop
               for Worker_New_State of Worker_Data.New_States loop
                  declare
                     use Item_Set_Trees;

                     Super_New_State  : constant State_Index := C.Last_Index + 1;

                     Found      : Boolean;
                     Found_Ref  : constant Item_Set_Trees.Variable_Reference_Type := C_Tree.Find_Or_Insert_Var
                       (New_C (Worker_New_State).Tree_Node, Found);
                  begin
                     if Found then
                        State_Map (Worker_New_State) := Found_Ref.State;
                        Found_States := @ + 1;
                        New_C (Worker_New_State).Tree_Node.State := Found_Ref.State;

                     else
                        Found_Ref.State := Super_New_State;
                        New_C (Worker_New_State).Tree_Node.State := Super_New_State;

                        States_To_Check.Put (Super_New_State);

                        State_Map (Worker_New_State) := Super_New_State;

                        C.Append (New_C (Worker_New_State));
                        pragma Assert (C.Last_Index = Super_New_State);

                        for ID in New_States_For_Worker.First_Index .. New_States_For_Worker.Last_Index  loop
                           if ID /= Worker_ID then
                              New_States_For_Worker (ID).Append (Super_New_State);
                           end if;
                        end loop;

                        if Trace_Generate_Table > Extra then
                           Ada.Text_IO.Put_Line
                             ("from state" & Worker_Data.From_State'Image & "." & Trimmed_Image (Worker_New_State));
                           Put (Grammar, Descriptor, New_C (Worker_New_State),
                                Show_Lookaheads => False,
                                Kernel_Only  => True);
                        end if;
                     end if;
                  end;
               end loop;

               --  Now we have State_Map, we can process the gotos.
               declare
                  use Goto_Item_Lists;
                  From_State : constant State_Index := Worker_Data.From_State;
               begin
                  for Item of Worker_Data.Existing_Goto_Items loop
                     if Trace_Generate_Table > Extra and then
                       not Has_Element (C (From_State).Goto_List.Find (Item.Symbol))
                     then
                        Ada.Text_IO.Put_Line
                          ("    state" & From_State'Image & " adding goto on " &
                             Image (Item.Symbol, Descriptor) & " to existing state" & Item.State'Image);
                     end if;

                     C (From_State).Goto_List.Insert (Item, Duplicate => SAL.Ignore);
                  end loop;

                  for Item of Worker_Data.New_Goto_Items loop
                     Item.State := State_Map (Item.State);

                     if Trace_Generate_Table > Extra and then
                       not Goto_Item_Lists.Has_Element
                         (C (From_State).Goto_List.Find (Item.Symbol))
                     then
                        Ada.Text_IO.Put_Line
                          ("    state" & From_State'Image & " adding goto on " &
                             Image (Item.Symbol, Descriptor) & " to new state" & Item.State'Image);
                     end if;

                     C (From_State).Goto_List.Insert (Item, Duplicate => SAL.Ignore);
                  end loop;
               end;
            end loop;

            Active_Workers := @ - 1;

            Net_Time := @ + (Ada.Calendar.Clock - Time_Start);
         exception
         when E : others =>

            Active_Workers := @ - 1;
            Fatal          := True;
            States_To_Check.Clear; -- force an early end.
            declare
               use Ada.Text_IO;
               use Ada.Exceptions;
            begin
               Error_ID       := Exception_Identity (E);
               Error_Message  := +Exception_Message (E);
               Put_Line
                 (Standard_Error, "(super) Update exception: " & Exception_Name (E) & ": " & Exception_Message (E));
            end;
         end Update;

         procedure Fatal_Error
           (Exception_ID : in Ada.Exceptions.Exception_Id;
            Message      : in String)
         is begin
            Supervisor.Error_ID      := Exception_ID;
            Supervisor.Error_Message := +Message;

            States_To_Check.Clear; -- force an early end.
            Fatal          := True;
            Active_Workers := @ - 1;
         end Fatal_Error;

         entry Done
           (ID      : out Ada.Exceptions.Exception_Id;
            Message : out Ada.Strings.Unbounded.Unbounded_String)
         when Fatal or (Active_Workers = 0 and States_To_Check.Is_Empty)
         is begin
            if Trace_Time then
               Ada.Text_IO.Put_Line
                 ("(super) net time:" & Net_Time'Image &
                    " states:" & C.Last_Index'Image &
                    " States_To_Check:" & States_To_Check.Length'Image &
                    " Found_States:" & Found_States'Image);
            end if;

            ID      := Supervisor.Error_ID;
            Message := Supervisor.Error_Message;
         end Done;

         function Get_C return Item_Set_List
         is begin
            return C;
         end Get_C;

      end Supervisor;

      task type Worker_Task
      is
         entry Start (ID : in LR1_Item_Sets_Parallel.Worker_ID);
         --  Start states from Supervisor. Stop when Supervisor returns
         --  Invalid_State_Index;
      end Worker_Task;

      task body Worker_Task
      is
         use all type Ada.Calendar.Time;

         ID : LR1_Item_Sets_Parallel.Worker_ID;

         Time_Start     : Ada.Calendar.Time;
         Net_Time       : Duration := 0.0; --  Time spent outside Supervisor
         States_Checked : Integer  := 0;
         States_Found   : Integer  := 0;

         C_Tree : Item_Set_Tree;          -- Local copy for fast find
         C      : Item_Set_Arrays.Vector; -- Local copy of subset of C to search; from Supervisor

         Local_New_State : State_Index := 1;

         --  See Supervisor Get, Update for definitions of these.
         New_C       : Item_Set_Arrays.Vector;
         New_C_Tree  : Item_Set_Tree;
         Worker_Data : State_Array_Worker_Data.Vector;

         procedure Check_State (C_Index : in State_Index)
         is
            C_I : Item_Set renames C (C_Index);
            Worker_Data : LR1_Item_Sets_Parallel.Worker_Data renames Worker_Task.Worker_Data (C_Index);
         begin
            States_Checked := @ + 1;
            Worker_Data.From_State := C_I.Tree_Node.State;
            Worker_Data.New_States.Clear;
            Worker_Data.Existing_Goto_Items.Clear;
            Worker_Data.New_Goto_Items.Clear;

            for Dot_ID_I in C_I.Dot_IDs.First_Index .. C_I.Dot_IDs.Last_Index loop
               --  [dragon] has 'for each grammar symbol X', but LR1_Goto_Transitions
               --  rejects Symbol that is not in Dot_IDs, so we iterate over that.

               declare
                  Symbol       : Token_ID renames C_I.Dot_IDs (Dot_ID_I);
                  New_Item_Set : Item_Set := LR1_Goto_Transitions
                    (C_I, Symbol, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
               begin
                  Compute_Key_Hash (New_Item_Set, C_Tree.Rows, Grammar, Descriptor, True);
                  declare
                     use Item_Set_Trees;

                     --  First search in Worker.C_Tree
                     Found_Cur : Cursor := C_Tree.Find (New_Item_Set.Tree_Node);

                     Found_State : constant Unknown_State_Index :=
                       (if Has_Element (Found_Cur)
                        then C_Tree.Constant_Ref (Found_Cur).State
                        else Unknown_State);
                  begin
                     if Found_State = Unknown_State then
                        Found_Cur := New_C_Tree.Find (New_Item_Set.Tree_Node);

                        if Has_Element (Found_Cur) then
                           --  Local_New_State was previously generated from some other state we
                           --  are checking.
                           Worker_Data.New_Goto_Items.Append ((Symbol, C_Tree.Constant_Ref (Found_Cur).State));

                        else
                           Worker_Data.New_Goto_Items.Append ((Symbol, Local_New_State));

                           New_Item_Set.Tree_Node.State := Local_New_State;
                           New_Item_Set.Dot_IDs := Get_Dot_IDs (Grammar, New_Item_Set.Set, Descriptor);
                           New_C.Append (New_Item_Set);
                           pragma Assert (New_C.Last_Index = Local_New_State);
                           Worker_Data.New_States.Append (Local_New_State);

                           New_C_Tree.Insert (New_Item_Set.Tree_Node, Duplicate => SAL.Error);

                           Local_New_State := Local_New_State + 1;
                        end if;
                     else
                        States_Found := @ + 1;
                        pragma Assert (C_I.Goto_List.Count = 0);
                        Worker_Data.Existing_Goto_Items.Append ((Symbol, Found_State));
                        if Trace_Generate_Table > Extra then
                           Ada.Text_IO.Put_Line
                             ("(worker" & ID'Image & ")  state" & Worker_Data.From_State'Image & " adding goto on " &
                                Image (Symbol, Descriptor) & " to existing state" & Image
                                  (C_Tree.Variable_Ref (Found_Cur)));
                        end if;
                     end if;
                  end;
               end;
            end loop;
         end Check_State;
      begin
         select
            accept Start (ID : in LR1_Item_Sets_Parallel.Worker_ID)

            do
               Worker_Task.ID := ID;
            end Start;
         or
            terminate;
         end select;

         C_Tree.Set_Rows (Hash_Table_Size);
         New_C_Tree.Set_Rows (Hash_Table_Size);

         loop
            declare
               Keys_To_Store : Item_Set_Tree_Node_Arrays.Vector;
            begin
               Supervisor.Get (ID, C, Keys_To_Store);
               exit when C.Length = 0;

               Time_Start := Ada.Calendar.Clock;

               for Set of C loop
                  --  C are all new states to check, but they may
                  --  have been in a previous Keys_To_Store.
                  C_Tree.Insert (Set.Tree_Node, Duplicate => SAL.Ignore);
               end loop;
               for Node of Keys_To_Store loop
                  --  States are added to Keys_To_Store when they are new in
                  --  Supervisor.C_Tree, before they are given to any worker to check;
                  --  they may also be in C
                  C_Tree.Insert (Node, Duplicate => SAL.Ignore);
               end loop;
            end;

            Local_New_State := 1;
            New_C.Set_First_Last (First => Local_New_State, Last => Local_New_State - 1);
            New_C_Tree.Clear; -- IMPROVEME: new_c_tree red_black should use vector store, not allocate each node

            Worker_Data.Set_First_Last (C.First_Index, C.Last_Index);

            for I in C.First_Index .. C.Last_Index loop
               Check_State (I);
            end loop;
            C.Clear;

            Net_Time := @ + (Ada.Calendar.Clock - Time_Start);
            Supervisor.Update (ID, New_C, Worker_Data);
            Time_Start := Ada.Calendar.Clock;

            --  New_C.Tree_Node.State updated; insert into C_Tree.
            for Item of New_C loop
               C_Tree.Insert (Item.Tree_Node, Duplicate => SAL.Error);
            end loop;
            Net_Time := @ + (Ada.Calendar.Clock - Time_Start);
         end loop;

         if Trace_Time then
            declare
               Elements, Max_Row_Depth, Average_Row_Depth : Ada.Containers.Count_Type;
               Rows, Empty_Rows : Integer;
            begin
               C_Tree.Sizes (Elements, Rows, Max_Row_Depth, Average_Row_Depth, Empty_Rows);

               Ada.Text_IO.Put_Line
                 ("(worker" & ID'Image & ") net time" & Net_Time'Image &
                    " states checked:" & States_Checked'Image & " states found:" & States_Found'Image &
                    " hash table states:" & Elements'Image &
                    " rows:" & Rows'Image &
                    " max_row_depth:" & Max_Row_Depth'Image &
                    " average_row_depth:" & Average_Row_Depth'Image &
                    " empty_rows:" & Empty_Rows'Image);
            end;
         end if;

         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("(worker" & ID'Image & ") terminate");
         end if;
      exception
      when E : others =>
         Supervisor.Fatal_Error (Ada.Exceptions.Exception_Identity (E), Ada.Exceptions.Exception_Message (E));
         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("(worker" & ID'Image & ") terminate on exception");
         end if;
      end Worker_Task;

      Worker_Tasks : array
        (1 .. System.Multiprocessors.CPU_Range'Min
         (Task_Count,
         System.Multiprocessors.CPU_Range'Max (1, System.Multiprocessors.Number_Of_CPUs)))
        of Worker_Task;

      First_State_Index : constant State_Index := 0;

      First_Item_Set : Item_Set := Closure
        ((Set            => Item_Lists.To_List
            ((Prod       => (Grammar.First_Index, 0),
              Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First_Index,
              Lookaheads => To_Lookahead (Descriptor.EOI_ID))),
          Tree_Node      =>
            (State       => First_State_Index,
             others      => <>),
          others         => <>),
         Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
   begin
      Supervisor.Initialize (LR1_Item_Sets_Parallel.Worker_ID (Worker_Tasks'Last), First_Item_Set);

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.Put_Line (Worker_Tasks'Length'Image & " lr1_items worker tasks");
      end if;

      for I in Worker_Tasks'Range loop
         Worker_Tasks (I).Start (LR1_Item_Sets_Parallel.Worker_ID (I));
      end loop;

      declare
         use Ada.Exceptions;
         ID      : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Supervisor.Done (ID, Message); -- Wait for all states to be checked

         if ID /= Null_Id then
            for I in Worker_Tasks'Range loop
               if not Worker_Tasks (I)'Terminated then
                  abort Worker_Tasks (I);
               end if;
            end loop;
            Raise_Exception (ID, -Message);
         else
            if Trace_Generate_Table > Outline then
               Ada.Text_IO.Put_Line ("super reports done");
            end if;
         end if;
      end;
      return Supervisor.Get_C;
   end LR1_Item_Sets_Parallel;

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
      Known_Conflicts       : in     Conflict_Lists.Tree              := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type              := Default_McKenzie_Param;
      Max_Parallel          : in     SAL.Base_Peek_Type               := 15;
      Parse_Table_File_Name : in     String                           := "";
      Include_Extra         : in     Boolean                          := False;
      Ignore_Conflicts      : in     Boolean                          := False;
      Partial_Recursion     : in     Boolean                          := True;
      Task_Count            : in     System.Multiprocessors.CPU_Range := 1;
      Hash_Table_Size       : in     Positive                         := LR1_Items.Item_Set_Trees.Default_Rows;
      Use_Cached_Recursions : in     Boolean                          := False;
      Recursions            : in out WisiToken.Generate.Recursions)
     return Parse_Table_Ptr
   is
      use all type System.Multiprocessors.CPU_Range;

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

      Item_Sets : constant LR1_Items.Item_Set_List :=
        (if Task_Count = 1
         then LR1_Item_Sets_Single
           (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Hash_Table_Size)
        else LR1_Item_Sets_Parallel
           (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Task_Count,
            Hash_Table_Size));

      Unknown_Conflicts    : Conflict_Lists.Tree;
      Known_Conflicts_Edit : Conflict_Lists.Tree := Known_Conflicts;

      Initial_Item_Sets_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

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
