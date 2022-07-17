--  Abstract :
--
--  Unbounded sparse sets.
--
--  Copyright (C) 2020 - 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Iterator_Interfaces;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
generic
   type Index_Type is private;
   --  Index_Type must have a valid default initialization; it is used as
   --  Gen_Unbounded_Definite_Red_Black_Trees.Element_Type.

   with function Index_Compare (Left, Right : in Index_Type) return Compare_Result;
package SAL.Gen_Unbounded_Sparse_Ordered_Sets is

   package Pkg renames Gen_Unbounded_Sparse_Ordered_Sets;

   type Set is tagged private
   with
     Constant_Indexing => Constant_Ref,
     Default_Iterator  => Iterate,
     Iterator_Element  => Index_Type;
   --  We'd like to have Constant_Indexing return a Boolean, so we could
   --  use 'if Set (Item) then'. But then the default iterator would
   --  always return True, instead of Index_Type; we can't specify a
   --  different Constant_Indexing function for the default iterator.

   procedure Clear (Set : in out Pkg.Set);
   --  Set Set to empty.

   function Count (Set : in Pkg.Set) return Ada.Containers.Count_Type;

   procedure Insert (Set : in out Pkg.Set; Item : in Index_Type);
   --  No error if already present.

   function Contains (Set : in Pkg.Set; Item : in Index_Type) return Boolean;

   procedure Delete (Set : in out Pkg.Set; Item : in Index_Type);
   --  Invalid while an iterator is active; not enforced.

   type Cursor is private;

   function Has_Element (Position : in Cursor) return Boolean;

   function Element (Position : in Cursor) return Index_Type;

   type Constant_Reference_Type (Element : not null access constant Index_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref
     (Container : aliased in Set;
      Position  :         in Cursor)
     return Constant_Reference_Type
   with Inline;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator (<>) is new Iterators.Forward_Iterator with private;

   function Iterate (Container : aliased in Set'Class) return Iterator;
   --  Returns Index_Type of elements that were inserted.

   overriding function First (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor;

private
   function Key (Item : in Index_Type) return Index_Type
   is (Item);

   package Boolean_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Index_Type,
      Key_Type     => Index_Type,
      Key          => Key,
      Key_Compare  => Index_Compare);

   type Set is tagged record
      Tree : aliased Boolean_Trees.Tree;
   end record;

   type Constant_Reference_Type (Element : not null access constant Index_Type)
   is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Cursor is record
      Cur : Boolean_Trees.Cursor;
   end record;

   Empty_Set : constant Set := (Tree => Boolean_Trees.Empty_Tree);

   type Iterator (Container : not null access constant Boolean_Trees.Tree) is new Iterators.Forward_Iterator
   with record
      Iter : Boolean_Trees.Iterator (Container);
   end record;

end SAL.Gen_Unbounded_Sparse_Ordered_Sets;
