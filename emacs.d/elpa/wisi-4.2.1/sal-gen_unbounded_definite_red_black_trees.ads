--  Abstract :
--
--  Generic unbounded red-black tree with definite elements, definite
--  or indefinite key.
--
--  Design:
--
--  We don't enforce any control of references active on a tree; that
--  proved buggy, slow, and not helpful in finding higher-level bugs.
--  The only real problem is references to a deleted node, which is
--  easy to keep track of.
--
--  References :
--
--  [1] Introduction to Algorithms, Thomas H. Cormen, Charles E.
--  Leiserson, Ronald L. Rivest, Clifford Stein.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
   --  Element_Type must have valid default initialization; one
   --  non-initialized object of this type is declared, in Tree.Nil.

   type Key_Type (<>) is private;
   with function Key (Element : in Element_Type) return Key_Type is <>;
   with function Key_Compare (Left, Right : in Key_Type) return Compare_Result is <>;
package SAL.Gen_Unbounded_Definite_Red_Black_Trees is

   package Pkg renames Gen_Unbounded_Definite_Red_Black_Trees;

   type Tree is new Ada.Finalization.Controlled with private
   with
     Constant_Indexing => Constant_Ref,
     Variable_Indexing => Variable_Ref,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   overriding procedure Finalize (Object : in out Tree);
   overriding procedure Initialize (Object : in out Tree);
   overriding procedure Adjust (Object : in out Tree);

   Empty_Tree : constant Tree;

   procedure Clear (Tree : in out Pkg.Tree);
   --  Set Tree to empty.

   type Direction_Type is (Ascending, Descending, Unknown);
   subtype Known_Direction_Type is Direction_Type range Ascending .. Descending;
   --  Direction of Iterators.
   --  If Ascending, Next may be called.
   --  If Descending, Previous may be called.
   --  If Unknown, neither.

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean;
   function Direction (Cursor : in Pkg.Cursor) return Direction_Type;
   function Key (Cursor : in Pkg.Cursor) return Key_Type;
   function Element (Cursor : in Pkg.Cursor) return Element_Type;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref
     (Container : in Tree;
      Position  : in Cursor)
     return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Constant_Ref
     (Container : in Tree;
      Key       : in Key_Type)
     return Constant_Reference_Type
   with Inline;
   --  Raises Not_Found if Key not found in Container.

   type Variable_Reference_Type (Element : not null access Element_Type) is private with
     Implicit_Dereference => Element;
   --  User must not change value of Key thru this reference; if Key is
   --  changed, use Delete, Insert.

   function Variable_Ref
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Variable_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Variable_Ref
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Variable_Reference_Type
   with Inline;
   --  Raises Not_Found if Key not found in Container.

   function Unchecked_Const_Ref (Container : in Tree; Position  : in Cursor) return access constant Element_Type;
   function Unchecked_Var_Ref (Container : in Tree; Position  : in Cursor) return access Element_Type;
   --  For higher level containers.

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator (Container : not null access constant Tree) is new Iterators.Reversible_Iterator with private;

   function Iterate (Tree : aliased in Pkg.Tree'Class) return Iterator;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   with Pre => Has_Element (Position) and Direction (Position) /= Unknown;
   overriding function Last (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Previous (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   with Pre => Has_Element (Position) and Direction (Position) /= Unknown;

   function Previous (Iterator : in Pkg.Iterator; Key : in Key_Type) return Cursor;
   --  Initialise Iterator to descending, starting at element with
   --  largest key < Key. Has_Element (result) is False if there is no
   --  such element.

   function Find
     (Iterator  : in Pkg.Iterator;
      Key       : in Key_Type;
      Direction : in Direction_Type := Ascending)
     return Cursor;
   --  Has_Element is False if Key is not in Container.

   function Find
     (Container : in Tree;
      Key       : in Key_Type;
      Direction : in Direction_Type := Ascending)
     return Cursor;
   --  Creates an Iterator internally.

   function Find_In_Range
     (Iterator    : in Pkg.Iterator;
      Direction   : in Known_Direction_Type;
      First, Last : in Key_Type)
     return Cursor;
   --  Find first element with key in range First .. Last. If Direction
   --  is Ascending, start at First, otherwise start at Last.
   --
   --  Has_Element (result) is False if there is no such element.
   --
   --  The Iterator does not remember First, Last; the user must check
   --  those for any element that Next or Previous returns.

   function Count (Tree : in Pkg.Tree) return Ada.Containers.Count_Type;
   function Length (Tree : in Pkg.Tree) return Ada.Containers.Count_Type
     renames Count;
   procedure Count_Depth
     (Tree  : in     Pkg.Tree;
      Count :    out Ada.Containers.Count_Type;
      Depth :    out Ada.Containers.Count_Type);
   --  Count and Count_Depth traverse the entire tree.

   function Present (Container : in Tree; Key : in Key_Type) return Boolean;

   procedure Insert
     (Tree      : in out Pkg.Tree;
      Element   : in     Element_Type;
      Duplicate : in     Duplicate_Action_Type := Error);
   function Insert
     (Tree      : in out Pkg.Tree;
      Element   : in     Element_Type;
      Duplicate : in     Duplicate_Action_Type := Error)
     return Cursor;
   --  Result points to newly inserted element, with Direction Unknown.
   --
   --  If Key (Element) is found, and Duplicate is:
   --
   --  - Allow, Element is inserted; it can only by retrieved using
   --  Iterate.
   --
   --  - Ignore, Element is not inserted.
   --
   --  - Error, raises Duplicate_Key.

   function Find_Or_Insert
     (Tree    : in out Pkg.Tree;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Cursor;
   --  Search for Element; if found, Found is True. If not found, insert
   --  it. Return a Cursor to the found or inserted element. , and return
   --  a cursor for it.

   procedure Delete (Tree : in out Pkg.Tree; Key : in Key_Type);
   --  Delete element with Key.
   --
   --  Raises SAL.Not_Found if Key is not found.
   --
   --  Invalidates any active iterators; not enforced.

private

   type Node;
   type Node_Access is access Node;

   type Color is (Red, Black);

   type Node is record
      Element : aliased Element_Type;
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Pkg.Color;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   type Tree is new Ada.Finalization.Controlled with record
      Root : Node_Access;
      Nil  : Node_Access;
      --  Nil is the node pointed to by all links that would otherwise be
      --  'null'. This simplifies several algorithms (for example,
      --  Node.Left.Color is always valid). Its parent, left, right links
      --  are used as temp storage for some algorithms (especially Delete).
      --  Nil.Color is Black. Nil.Element is never accessed.
   end record;

   type Cursor is record
      Node : Node_Access := null;

      Direction  : Direction_Type := Unknown;
      --  Set in First or Last, enforced in next/prev (cannot change direction).

      Left_Done  : Boolean := True;
      Right_Done : Boolean := True;
   end record;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean
   is (Cursor.Node /= null);

   function Direction (Cursor : in Pkg.Cursor) return Direction_Type
   is (Cursor.Direction);

   function Key (Cursor : in Pkg.Cursor) return Key_Type
   is (Key (Cursor.Node.Element));

   function Element (Cursor : in Pkg.Cursor) return Element_Type
   is (Cursor.Node.Element);

   type Constant_Reference_Type (Element : not null access constant Element_Type)
   is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type)
   is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Tree : constant Tree := (Ada.Finalization.Controlled with null, null);

   No_Element : constant Cursor :=
     (Node       => null,
      Direction  => Unknown,
      Left_Done  => True,
      Right_Done => True);

   type Iterator (Container : not null access constant Tree) is new Iterators.Reversible_Iterator
     with null record;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees;
