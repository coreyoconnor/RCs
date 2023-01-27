--  Abstract :
--
--  See spec
--
--  Notice
--
--  Copyright (C) 2020 - 2021 Free Software Foundation, Inc. All Rights Reserved.
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

package body SAL.Gen_Unbounded_Definite_Hash_Tables is

   --  Local subprograms

   Row_Sizes : constant array (Positive range <>) of Positive :=
     --  List of primes > 2 * previous. From [1]. Max is given by Ada LR1
     --  parse table number of states; ~ 500_000, with an average row
     --  content of 3; we allow for twice that => (* 2 (/ 500000 3)) =
     --  333_332.
     (113, 227, 467, 937, 1877, 3761, 7523, 15_053, 31_013, 62_039, 124_087, 248_177, 496_381);

   function Find_Prime (Rows_Target : in Positive) return Positive
   is begin
      for P of Row_Sizes loop
         if P >= Rows_Target then
            return P;
         end if;
      end loop;
      raise Programmer_Error with Rows_Target'Image & " not found in Primes table (max" &
        Row_Sizes (Row_Sizes'Last)'Image & "); need better hash function, or more primes";
   end Find_Prime;

   procedure Grow (Table : in out Hash_Table; New_Rows : in Positive := Positive'First)
   is
      Rows_Target : constant Positive := (if New_Rows = Positive'First then 2 * Table.Table.Last_Index else New_Rows);

      Prime_New_Rows : constant Positive := Find_Prime (Rows_Target);

      New_Table : Hash_Table;
   begin
      New_Table.Table.Set_First_Last (Table.Table.First_Index, Prime_New_Rows);

      for Row of Table.Table loop
         for Element of Row loop
            New_Table.Insert (Element, Duplicate => Error);
         end loop;
      end loop;

      Table := New_Table;
   end Grow;

   ----------
   --  Public subprograms, in spec order

   procedure Set_Rows
     (Table : in out Hash_Table;
      Rows  : in     Positive)
   is begin
      if Table.Table.Is_Empty then
         Table.Table.Set_First_Last (1, Find_Prime (Rows));

      elsif Table.Table.Last_Index = Rows then
         --  No change
         null;

      else
         Grow (Table, Rows);
      end if;
   end Set_Rows;

   procedure Clear (Table : in out Hash_Table)
   is begin
      if Table.Table.Is_Empty then
         return;
      else
         for Row of Table.Table loop
            Row.Clear;
         end loop;
      end if;
   end Clear;

   procedure Insert
     (Table     : in out Hash_Table;
      Element   : in     Element_Type;
      Duplicate : in     Ignore_Error_Type)
   is begin
      if Table.Table.Is_Empty then
         Set_Rows (Table, Default_Init_Rows);
      end if;

      declare
         Found    : Boolean;
         Tree_Cur : constant Element_Trees.Cursor := Table.Table (Hash (Element, Table.Table.Last_Index))
           .Find_Or_Insert (Element, Found);
         pragma Unreferenced (Tree_Cur);
      begin
         if Found then
            case Duplicate is
            when Ignore =>
               null;
            when Error =>
               raise Duplicate_Key;
            end case;
         end if;
      end;
   end Insert;

   function Find_Or_Insert
     (Table   : in out Hash_Table;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Constant_Reference_Type
   is begin
      if Table.Table.Is_Empty then
         Set_Rows (Table, Default_Init_Rows);
      end if;

      declare
         Tree : Element_Trees.Tree renames Table.Table (Hash (Element, Table.Table.Last_Index));
         Tree_Cur : constant Element_Trees.Cursor := Tree.Find_Or_Insert (Element, Found);
      begin
         return Tree.Unchecked_Const_Ref (Tree_Cur);
      end;
   end Find_Or_Insert;

   function Find_Or_Insert_Var
     (Table   : in out Hash_Table;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Variable_Reference_Type
   is begin
      if Table.Table.Is_Empty then
         Set_Rows (Table, Default_Init_Rows);
      end if;

      declare
         Tree : Element_Trees.Tree renames Table.Table (Hash (Element, Table.Table.Last_Index));
         Tree_Cur : constant Element_Trees.Cursor := Tree.Find_Or_Insert (Element, Found);
      begin
         return Variable_Reference_Type (Tree.Unchecked_Var_Ref (Tree_Cur));
      end;
   end Find_Or_Insert_Var;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean
   is begin
      return Cursor /= No_Element;
   end Has_Element;

   function Constant_Ref
     (Table    : aliased in Hash_Table;
      Position :         in Cursor)
     return Constant_Reference_Type
   is
      Tree : Element_Trees.Tree renames Table.Table (Position.Row);
   begin
      return Tree.Unchecked_Const_Ref (Position.Cur);
   end Constant_Ref;

   function Variable_Ref
     (Table    : aliased in Hash_Table;
      Position :         in Cursor)
     return Variable_Reference_Type
   is
      Tree : Element_Trees.Tree renames Table.Table (Position.Row);
   begin
      return Tree.Unchecked_Var_Ref (Position.Cur);
   end Variable_Ref;

   function Find
     (Table   : aliased in Hash_Table;
      Element :         in Element_Type)
     return Cursor
   is begin
      if Table.Table.Is_Empty then
         return No_Element;
      end if;

      declare
         Row : constant Positive := Hash (Element, Table.Table.Last_Index);
         Tree : Element_Trees.Tree renames Table.Table (Row);
         Tree_Cur : constant Element_Trees.Cursor := Tree.Find (Key (Element));
      begin
         if Element_Trees.Has_Element (Tree_Cur) then
            return (Row, Tree_Cur);
         else
            return No_Element;
         end if;
      end;
   end Find;

   function Iterate (Table : aliased in Pkg.Hash_Table'Class) return Iterator
   is begin
      return (Table => Table'Access);
   end Iterate;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor
   is begin
      for Row in Iterator.Table.Table.First_Index .. Iterator.Table.Table.Last_Index loop
         declare
            Iter     : constant Element_Trees.Iterator := Iterator.Table.Table (Row).Iterate;
            Tree_Cur : constant Element_Trees.Cursor   := Iter.First;
         begin
            if Element_Trees.Has_Element (Tree_Cur) then
               return (Iterator.Table.Table.First_Index, Tree_Cur);
            end if;
         end;
      end loop;
      return No_Element;
   end First;

   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   is
      Iter      : Element_Trees.Iterator        := Iterator.Table.Table (Position.Row).Iterate;
      Tree_Next : constant Element_Trees.Cursor := Iter.Next (Position.Cur);
   begin
      if Element_Trees.Has_Element (Tree_Next) then
         return (Position.Row, Tree_Next);
      elsif Position.Row < Iterator.Table.Table.Last_Index then
         Iter := Iterator.Table.Table (Position.Row + 1).Iterate;
         return (Position.Row + 1, Iter.First);
      else
         return No_Element;
      end if;
   end Next;

   procedure Sizes
     (Table             : in     Hash_Table;
      Elements          :    out Ada.Containers.Count_Type;
      Rows              :    out Integer;
      Max_Row_Depth     :    out Ada.Containers.Count_Type;
      Average_Row_Depth :    out Ada.Containers.Count_Type;
      Empty_Rows        :    out Integer)
   is
      use Ada.Containers;
   begin
      Elements          := 0;
      Rows              := Table.Table.Last_Index;
      Max_Row_Depth     := 0;
      Average_Row_Depth := 0;
      Empty_Rows        := 0;

      for Row of Table.Table loop
         declare
            Count : Count_Type;
            Depth : Count_Type;
         begin
            Row.Count_Depth (Count, Depth);
            Elements := @ + Count;
            if Count = 0 then
               Empty_Rows := @ + 1;
            else
               if Max_Row_Depth < Depth then
                  Max_Row_Depth := Depth;
               end if;
               Average_Row_Depth := @ + Depth;
            end if;
         end;
      end loop;

      if Rows > 0 and Rows > Empty_Rows then
         Average_Row_Depth := @ / Count_Type (Rows - Empty_Rows);
      end if;
   end Sizes;

end SAL.Gen_Unbounded_Definite_Hash_Tables;
