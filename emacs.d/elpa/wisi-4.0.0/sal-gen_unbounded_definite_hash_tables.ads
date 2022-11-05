--  Abstract:
--
--  Generic Hash Table, using red-black trees for collisions.
--
--  Design
--
--  We assume computing Key from Element is free (for example, Element
--  is (Key, Index to actual store)), and computing Hash from Key is
--  cheap. Hashes are recomputed for all elements when the table is
--  grown.
--
--  References
--
--  [1] Prime numbers http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
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

with Ada.Containers;
with Ada.Iterator_Interfaces;
private with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
private with SAL.Gen_Unbounded_Definite_Vectors;
generic
   type Element_Type is private;
   type Key_Type (<>) is private;
   with function Key (Element : in Element_Type) return Key_Type is <>;
   with function Key_Compare (Left, Right : in Key_Type) return Compare_Result;

   with function Hash (Element : Element_Type; Rows : Positive) return Positive;
   --  WORKAROUND: GNAT community 2019 doesn't allow 'with post' here
   --  with Release compilation switches. Fixed in GNAT community 2021.
   --  with Post => Hash'Result in 1 .. Rows;
   --
   --  Takes Element, not Key, to allow storing Hash in Element so it is
   --  only computed once.
   --
   --  1 + (Some_hash (Key) mod Rows) works.

   Default_Init_Rows : Positive := 113;

package SAL.Gen_Unbounded_Definite_Hash_Tables is

   package Pkg renames Gen_Unbounded_Definite_Hash_Tables;

   Default_Rows : constant Positive := Pkg.Default_Init_Rows;

   type Hash_Table is tagged private
   with
     Constant_Indexing => Constant_Ref,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   procedure Set_Rows
     (Table : in out Hash_Table;
      Rows  : in     Positive);
   --  Set the hash table size. If Table is not empty, all hashes are
   --  recomputed; this renders any Constant_Refs invalid.

   function Rows (Table : in Hash_Table) return Positive;

   procedure Clear (Table : in out Hash_Table);
   --  Set Table to empty.

   procedure Insert
     (Table     : in out Hash_Table;
      Element   : in     Element_Type;
      Duplicate : in     Ignore_Error_Type);
   --  If Key (Element) is already in Table: if Duplicate is Ignore, does
   --  nothing; otherwise, raises Duplicate_Key.

   type Constant_Reference_Type is access constant Element_Type;
   --  The name lies; this is not a "reference type" as defined by Ada.
   --  But gnat pro 22.0w 20201222 does not support using a real
   --  reference type here. See AdaCore ticket U117-010 (on the
   --  Eurocontrol contract).

   type Variable_Reference_Type is access all Element_Type;
   --  Similarly, this is not a "reference type"; therefore we cannot
   --  implement aspect Variable_Indexing.

   function Find_Or_Insert
     (Table   : in out Hash_Table;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Constant_Reference_Type;

   function Find_Or_Insert_Var
     (Table   : in out Hash_Table;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Variable_Reference_Type;
   --  User must not change Key or Hash via this reference.

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean;

   function Constant_Ref
     (Table    : aliased in Hash_Table;
      Position :         in Cursor)
     return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Variable_Ref
     (Table    : aliased in Hash_Table;
      Position :         in Cursor)
     return Variable_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Find
     (Table   : aliased in Hash_Table;
      Element :         in Element_Type)
     return Cursor;
   --  Result is No_Element if Key is not in Table.
   --
   --  Takes Element instead of Key to allow storing Hash in Element.

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator (<>) is new Iterators.Forward_Iterator with private;

   function Iterate (Table : aliased in Pkg.Hash_Table'Class) return Iterator;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   with Pre => Has_Element (Position);

   procedure Sizes
     (Table             : in     Hash_Table;
      Elements          :    out Ada.Containers.Count_Type;
      Rows              :    out Integer;
      Max_Row_Depth     :    out Ada.Containers.Count_Type;
      Average_Row_Depth :    out Ada.Containers.Count_Type;
      Empty_Rows        :    out Integer);

private

   package Element_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type, Key_Type, Key, Key_Compare);
   --  Holds elements for a row

   package Hash_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Element_Trees.Tree, Element_Trees.Empty_Tree);

   type Hash_Table is tagged record
      --  Directly deriving Hash_Table from Hash_Arrays.Vector would mean we
      --  have to implement Iterate.
      Table : Hash_Arrays.Vector;
   end record;

   function Rows (Table : in Hash_Table) return Positive
   is (if Table.Table.Last_Index = Hash_Arrays.No_Index then Default_Init_Rows else Table.Table.Last_Index);

   type Cursor is record
      Row : Integer              := Hash_Arrays.No_Index; --  index into Table.Table.
      Cur : Element_Trees.Cursor := Element_Trees.No_Element;
   end record;

   No_Element : constant Cursor := (others => <>);

   type Iterator (Table : not null access constant Hash_Table) is new Iterators.Forward_Iterator
     with null record;

end SAL.Gen_Unbounded_Definite_Hash_Tables;
