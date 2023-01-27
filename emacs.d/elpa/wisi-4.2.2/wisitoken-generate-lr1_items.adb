--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2008, 2009, 2012 - 2015, 2017 - 2021 Free Software Foundation, Inc.
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
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body WisiToken.Generate.LR1_Items is
   use type Ada.Strings.Unbounded.Unbounded_String;

   ----------
   --  body subprograms

   function Merge
     (Prod         : in     Production_ID;
      Dot          : in     Token_ID_Arrays.Extended_Index;
      ID           : in     Token_ID;
      Existing_Set : in out Item_Set)
     return Boolean
   is
      --  Merge item into Existing_Set. Return True if Existing_Set
      --  is modified.

      use Item_Lists;

      Found    : constant Item_Lists.Cursor := Find (Prod, Dot, Existing_Set);
      Modified : Boolean                    := False;
   begin
      if not Has_Element (Found) then
         Existing_Set.Set.Insert ((Prod, Dot, To_Lookahead (ID)));
         Modified := True;
      else
         declare
            Lookaheads : Lookahead renames Variable_Ref (Found).Lookaheads;
         begin
            Modified := not Lookaheads (ID);
            Lookaheads (ID) := True;
         end;
      end if;

      return Modified;
   end Merge;

   function Merge
     (Prod         : in     Production_ID;
      Dot          : in     Token_ID_Arrays.Extended_Index;
      Lookaheads   : in     Lookahead;
      Existing_Set : in out Item_Set)
     return Boolean
   is
      --  Merge item into Existing_Set. Return True if Existing_Set
      --  is modified.

      use Item_Lists;

      Found    : constant Item_Lists.Cursor := Find (Prod, Dot, Existing_Set);
      Modified : Boolean                    := False;
   begin
      if not Has_Element (Found) then
         Existing_Set.Set.Insert ((Prod, Dot, Lookaheads));

         Modified := True;
      else
         Include (Variable_Ref (Found), Lookaheads, Modified);
      end if;

      return Modified;
   end Merge;

   ----------
   --  Public subprograms, declaration order

   function To_Lookahead (Item : in Token_ID) return Lookahead
   is begin
      return Result : Lookahead := (others => False) do
         Result (Item) := True;
      end return;
   end To_Lookahead;

   function Lookahead_Image (Item : in Lookahead; Descriptor : in WisiToken.Descriptor) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Descriptor.First_Terminal .. Descriptor.Last_Lookahead  loop
         if Item (I) then
            if Length (Result) > 0 then
               Result := Result & "/";
            end if;
            Result := Result & Image (I, Descriptor);
         end if;
      end loop;
      return To_String (Result);
   end Lookahead_Image;

   function Item_Compare (Left, Right : in Item) return SAL.Compare_Result
     is (if Left.Prod.LHS > Right.Prod.LHS then SAL.Greater
         elsif Left.Prod.LHS < Right.Prod.LHS then SAL.Less
         elsif Left.Prod.RHS > Right.Prod.RHS then SAL.Greater
         elsif Left.Prod.RHS < Right.Prod.RHS then SAL.Less
         elsif Left.Dot > Right.Dot then SAL.Greater
         elsif Left.Dot < Right.Dot then SAL.Less
         else SAL.Equal);

   procedure Include
     (Item  : in out LR1_Items.Item;
      Value : in     Lookahead;
      Added :    out Boolean)
   is
      Item_Lookaheads_1 : constant Lookahead := Item.Lookaheads;
   begin
      Item.Lookaheads := Item.Lookaheads or Value;
      Added           := Item.Lookaheads /= Item_Lookaheads_1;
   end Include;

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Added      :    out Boolean;
      Descriptor : in     WisiToken.Descriptor)
   is
      Item_Lookaheads_1 : constant Lookahead := Item.Lookaheads;
   begin
      Item.Lookaheads := Item.Lookaheads or Value;

      Item.Lookaheads (Descriptor.Last_Lookahead) := False;

      Added := Item.Lookaheads /= Item_Lookaheads_1;
   end Include;

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Descriptor : in     WisiToken.Descriptor)
   is begin
      Item.Lookaheads := Item.Lookaheads or Value;

      Item.Lookaheads (Descriptor.Last_Lookahead) := False;
   end Include;

   function Get_Dot_IDs
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Set        : in Item_Lists.List;
      Descriptor : in WisiToken.Descriptor)
     return Token_ID_Arrays.Vector
   is
      use Item_Lists;
      IDs : Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal) := (others => False);
   begin
      --  Note that this algorithm eliminates duplicate Dot_IDs
      for Item of Set loop
         declare
            use Token_ID_Arrays;
            RHS : WisiToken.Productions.Right_Hand_Side renames
              WisiToken.Productions.Constant_Ref_RHS (Grammar, Item.Prod);
         begin
            if Item.Dot in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index then
               if RHS.Tokens (Item.Dot) /= Descriptor.EOI_ID then
                  IDs (RHS.Tokens (Item.Dot)) := True;
               end if;
            end if;
         end;
      end loop;
      return To_Array (IDs);
   end Get_Dot_IDs;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor;
         Item       : in LR1_Items.Item)
        return Boolean)
     return Item_Set
   is begin
      return Result : Item_Set :=
        (Set       => <>,
         Goto_List => Set.Goto_List,
         Dot_IDs   => Set.Dot_IDs,
         Tree_Node => Set.Tree_Node)
      do
         for Item of Set.Set loop
            if Include (Grammar, Descriptor, Item) then
               Result.Set.Insert (Item);
            end if;
         end loop;
      end return;
   end Filter;

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor;
      Item       : in LR1_Items.Item)
     return Boolean
   is
      use all type Ada.Containers.Count_Type;
      use Token_ID_Arrays;
      Prod : WisiToken.Productions.Instance renames Grammar (Item.Prod.LHS);
      RHS  : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (Item.Prod.RHS);
   begin
      return
        RHS.Tokens.Length > 0 and
        (Item.Dot = No_Index or else
           ((Prod.LHS = Descriptor.Accept_ID and
               Item.Dot = RHS.Tokens.First_Index)
              -- Start symbol production with dot before first token.
              or
              Item.Dot /= RHS.Tokens.First_Index));
   end In_Kernel;

   function Find
     (Item : in LR1_Items.Item;
      Set  : in Item_Set)
     return Item_Lists.Cursor
   is begin
      return Find (Item.Prod, Item.Dot, Set);
   end Find;

   function Find
     (Prod : in Production_ID;
      Dot  : in Token_ID_Arrays.Extended_Index;
      Set  : in Item_Set)
     return Item_Lists.Cursor
   is begin
      return Set.Set.Find ((Prod, Dot, Null_Lookahead));
   end Find;

   procedure Compute_Key_Hash
     (Item_Set           : in out LR1_Items.Item_Set;
      Rows               : in     Positive;
      Grammar            : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor         : in     WisiToken.Descriptor;
      Include_Lookaheads : in     Boolean)
   is
      use Interfaces;

      type Lookahead_Int_16 is record
         Word_0 : Interfaces.Unsigned_16;
         Word_1 : Interfaces.Unsigned_16;
         Word_2 : Interfaces.Unsigned_16;
         Word_3 : Interfaces.Unsigned_16;
         Word_4 : Interfaces.Unsigned_16;
         Word_5 : Interfaces.Unsigned_16;
         Word_6 : Interfaces.Unsigned_16;
         Word_7 : Interfaces.Unsigned_16;
      end record;
      for Lookahead_Int_16'Size use 128;

      function To_Int_16 is new Ada.Unchecked_Conversion (Source => Lookahead, Target => Lookahead_Int_16);
   begin
      Item_Set.Tree_Node.Key.Append (Unsigned_16 (Item_Set.Set.Length));
      --  Int_Arrays."<" compares length, but only after everything else; we
      --  want it to compare first, since it is most likely to be different.

      for Item of Item_Set.Set loop
         if In_Kernel (Grammar, Descriptor, Item) then
            Item_Set.Tree_Node.Key.Append (Unsigned_16 (Item.Prod.LHS));
            Item_Set.Tree_Node.Key.Append (Unsigned_16 (Item.Prod.RHS));
            Item_Set.Tree_Node.Key.Append (Unsigned_16 (Item.Dot));
            if Include_Lookaheads then
               declare
                  Temp : constant Lookahead_Int_16 := To_Int_16 (Item.Lookaheads);
               begin
                  --  This faster than scanning the 128 booleans to get the token_ids,
                  --  and shorter than some of those.
                  Item_Set.Tree_Node.Key.Append (Temp.Word_0);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_1);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_2);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_3);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_4);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_5);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_6);
                  Item_Set.Tree_Node.Key.Append (Temp.Word_7);
               end;
            end if;
         end if;
      end loop;

      Item_Set.Tree_Node.Hash := Hash_Sum_32 (Item_Set.Tree_Node.Key, Rows);
   end Compute_Key_Hash;

   function Hash_Sum_32 (Key : in Item_Set_Tree_Key; Rows : in Positive) return Positive
   is
      use Interfaces;
      Accum : Unsigned_32 := 0;
   begin
      for I of Key loop
         Accum := @ + Unsigned_32 (I);
      end loop;

      Accum := 1 + (Accum mod Unsigned_32 (Rows));
      return Positive (Accum);
   end Hash_Sum_32;

   function To_Item_Set_Tree_Hash (Node : in Item_Set_Tree_Node; Rows : in Positive) return Positive
   is
      pragma Unreferenced (Rows);
   begin
      return Node.Hash;
   end To_Item_Set_Tree_Hash;

   procedure Add
     (Grammar            : in     WisiToken.Productions.Prod_Arrays.Vector;
      New_Item_Set       : in out Item_Set;
      Item_Set_List      : in out LR1_Items.Item_Set_List;
      Item_Set_Tree      : in out LR1_Items.Item_Set_Tree;
      Descriptor         : in     WisiToken.Descriptor;
      Hash_Table_Rows    : in     Positive;
      Include_Lookaheads : in     Boolean)
   is
      use Item_Set_Trees;
   begin
      Compute_Key_Hash (New_Item_Set, Hash_Table_Rows, Grammar, Descriptor, Include_Lookaheads);
      Item_Set_List.Append (New_Item_Set);
      Item_Set_List (Item_Set_List.Last_Index).Dot_IDs := Get_Dot_IDs (Grammar, New_Item_Set.Set, Descriptor);
      Item_Set_Tree.Insert (New_Item_Set.Tree_Node, Duplicate => SAL.Error);
   end Add;

   function Is_In
     (Item      : in Goto_Item;
      Goto_List : in Goto_Item_List)
     return Boolean
   is begin
      for List_Item of Goto_List loop
         if List_Item = Item then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

   function Goto_State
     (From   : in Item_Set;
      Symbol : in Token_ID)
     return Unknown_State_Index
   is begin
      for Item of From.Goto_List loop
         if Item.Symbol = Symbol then
            return Item.State;
         end if;
      end loop;

      return Unknown_State;
   end Goto_State;

   function Closure
     (Set                     : in Item_Set;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return Item_Set
   is
      use Token_ID_Arrays;
      use WisiToken.Productions;
      use all type Item_Lists.Cursor;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead.

      I : Item_Set := Set; --  The result.

      Item_I     : Item_Lists.Cursor := I.Set.First; -- iterator 'for each item in I'
      Added_Item : Boolean := False;  -- 'until no more items can be added'
   begin
      loop
         declare
            Item : LR1_Items.Item renames I.Set (Item_I);
            Item_Tokens : Token_ID_Arrays.Vector renames Constant_Ref_RHS (Grammar, Item.Prod).Tokens;
            Item_Dot_ID : constant Token_ID :=
              (if Item.Dot in Item_Tokens.First_Index .. Item_Tokens.Last_Index
               then Item_Tokens (Item.Dot)
               else Invalid_Token_ID);
         begin
            --  An item has the structure [A -> alpha Dot B Beta, a].
            --
            --  If B is a nonterminal, find its productions and place
            --  them in the set with lookaheads from FIRST(Beta a).

            if Item_Dot_ID /= Invalid_Token_ID and
              Item_Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal
            then
               declare
                  Prod : WisiToken.Productions.Instance renames Grammar (Item_Dot_ID);
               begin
                  For_Each_RHS :
                  for J in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare
                        RHS  : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (J);
                        P_ID : constant Production_ID := (Prod.LHS, J);
                        Beta : Integer := (if Item.Dot = Item_Tokens.Last_Index then No_Index else Item.Dot + 1);
                        --  Tokens after Item nonterminal, null if No_Index
                     begin
                        --  Compute FIRST (<tail of right hand side> a); loop
                        --  until find a terminal, a nonterminal that
                        --  cannot be empty, or end of production, adding
                        --  items on the way.

                        First_Tail :
                        loop
                           if Beta = No_Index then
                              --  Use FIRST (a); a = Item.Lookaheads.
                              --  Lookaheads are all terminals, so
                              --  FIRST (a) = a.
                              Added_Item := Added_Item or
                                Merge (P_ID, RHS.Tokens.First_Index (No_Index_If_Empty => True), Item.Lookaheads, I);
                              exit First_Tail;

                           elsif Item_Tokens (Beta) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                              --  FIRST (Beta) = Beta
                              Added_Item := Added_Item or Merge
                                (P_ID, RHS.Tokens.First_Index (No_Index_If_Empty => True), Item_Tokens (Beta), I);
                              exit First_Tail;

                           else
                              --  Beta is a nonterminal; use FIRST (Beta)
                              for Terminal of First_Terminal_Sequence (Item_Tokens (Beta)) loop
                                 Added_Item := Added_Item or
                                   Merge (P_ID, RHS.Tokens.First_Index (No_Index_If_Empty => True), Terminal, I);
                              end loop;

                              if Has_Empty_Production (Item_Tokens (Beta)) then
                                 --  Process the next token in the tail, or "a"
                                 Beta := (if Beta = Item_Tokens.Last_Index then No_Index else Beta + 1);

                              else
                                 exit First_Tail;
                              end if;
                           end if;
                        end loop First_Tail;
                     end;
                  end loop For_Each_RHS;
               end;
            end if; -- Dot is at non-terminal
         end;

         if not Has_Element (Item_Lists.Next (Item_I)) then
            exit when not Added_Item;

            Item_I := I.Set.First;
            Added_Item := False;
         else
            Item_Lists.Next (Item_I);
         end if;
      end loop;

      return I;
   end Closure;

   function Productions (Set : in Item_Set) return Production_ID_Arrays.Vector
   is begin
      return Result : Production_ID_Arrays.Vector do
         for Item of Set.Set loop
            Result.Append (Item.Prod);
         end loop;
      end return;
   end Productions;

   function Image
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean)
     return String
   is
      use Token_ID_Arrays;

      Prod   : WisiToken.Productions.Instance renames Grammar (Item.Prod.LHS);
      RHS    : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (Item.Prod.RHS);
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        +Padded_Image (Item.Prod, Width => Prod_ID_Image_Width) & ":" & Image (Prod.LHS, Descriptor) & " <=";

      I : Cursor := RHS.Tokens.First;
   begin
      while Has_Element (I) loop
         if To_Index (I) = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Image (RHS.Tokens (I), Descriptor);
         RHS.Tokens.Next (I);
      end loop;

      if Item.Dot = No_Index then
         Result := Result & " ^";
      end if;

      if Show_Lookaheads then
         Result := Result & ", " & Lookahead_Image (Item.Lookaheads, Descriptor);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean := True)
   is begin
      Ada.Text_IO.Put (Image (Grammar, Descriptor, Item, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put
     (Descriptor : in WisiToken.Descriptor;
      List       : in Goto_Item_List)
   is
      use Ada.Text_IO;
   begin
      for Item of List loop
         Put_Line
           ("      on " & Image (Item.Symbol, Descriptor) &
              " => State" & Unknown_State_Index'Image (Item.State));
      end loop;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Lists.List;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False)
   is begin
      for It of Item loop
         if not Kernel_Only or else
           In_Kernel (Grammar, Descriptor, It)
         then
            Ada.Text_IO.Put_Line
              ("  " & Image (Grammar, Descriptor, It, Show_Lookaheads => Show_Lookaheads));
         end if;
      end loop;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False)
   is
      use Ada.Text_IO;
   begin
      if Item.Tree_Node.State /= Unknown_State then
         Put_Line ("State" & Unknown_State_Index'Image (Item.Tree_Node.State) & ":");
      end if;

      Put (Grammar, Descriptor, Item.Set, Show_Lookaheads, Kernel_Only);

      if Show_Goto_List then
         Put (Descriptor, Item.Goto_List);
      end if;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
   begin
      for Set of Item loop
         Put (Grammar, Descriptor, Set, Show_Lookaheads);
         Put_Line ("   Goto:");
         Put (Descriptor, Set.Goto_List);
      end loop;
   end Put;

end WisiToken.Generate.LR1_Items;
