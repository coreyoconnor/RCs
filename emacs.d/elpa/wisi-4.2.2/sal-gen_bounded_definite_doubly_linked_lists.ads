--  Abstract :
--
--  A generic bounded doubly linked list with definite elements; no dynamic memory.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
package SAL.Gen_Bounded_Definite_Doubly_Linked_Lists is

   type List (Size : Peek_Type) is tagged private
   --  We'd like to force initialization; one choice is to use
   --  Ada.Finalization, but that's pretty heavy. Another is to use <>
   --  instead of Size, but then we can't declare a List in a record type
   --  (ie WisiToken Configuration). So we do this, and let the user
   --  cope.
   with
      Constant_Indexing => Constant_Ref,
      Variable_Indexing => Variable_Ref,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   type List_Access_Constant is access constant List;
   for List_Access_Constant'Storage_Size use 0;

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   procedure Initialize (Container : in out List);

   function Empty_List (Size : in Peek_Type) return List;

   procedure Clear (Container : in out List);
   --  Set Container to empty.

   function Length (Container : in List) return Ada.Containers.Count_Type;

   procedure Append (Container : in out List; Element : in Element_Type);
   --  Raises SAL.Container_Full if Container is full, or if it is not
   --  initialized.

   procedure Prepend (Container : in out List; Element : in Element_Type);
   --  Raises SAL.Container_Full if Container is full, or if it is not
   --  initialized.

   function To_List (Element : in Element_Type; Size : in Peek_Type) return List;

   type Cursor is private;

   function Has_Element (Position : in Cursor) return Boolean;

   No_Element : constant Cursor;
   function First (Container : in List) return Cursor;
   function Last (Container : in List) return Cursor;

   procedure Next (Container : in List; Position : in out Cursor)
   with Pre => Has_Element (Position);

   function Next (Container : in List; Position : in Cursor) return Cursor
   with Pre => Has_Element (Position);

   procedure Previous (Container : in List; Position : in out Cursor)
   with Pre => Has_Element (Position);

   function Previous (Container : in List; Position : in Cursor) return Cursor
   with Pre => Has_Element (Position);

   function Element (Container : in List; Position : in Cursor) return Element_Type
   with Pre => Has_Element (Position);

   procedure Delete (Container : in out List; Position : in out Cursor)
   with Pre => Has_Element (Position);

   procedure Delete_First (Container : in out List);

   function Append (Container : in out List; Element : in Element_Type) return Cursor;

   procedure Insert
     (Container : in out List;
      Before    : in     Cursor;
      Element   : in     Element_Type);
   function Insert
     (Container : in out List;
      Before    : in     Cursor;
      Element   : in     Element_Type)
     return Cursor;
   --  If Before is No_Element, insert after Last.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref (Container : aliased in List; Position : in Cursor) return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   type Variable_Reference_Type (Element : not null access Element_Type) is private with
     Implicit_Dereference => Element;

   function Variable_Ref (Container : aliased in out List; Position : in Cursor) return Variable_Reference_Type
   with Inline, Pre => Has_Element (Position);

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in List) return Iterator_Interfaces.Reversible_Iterator'Class;

private

   type Node_Type is record
      Element : aliased Element_Type;
      Prev    : Base_Peek_Type := Invalid_Peek_Index; -- index in List.Nodes
      Next    : Base_Peek_Type := Invalid_Peek_Index;
   end record;

   type Node_Array_Type is array (Peek_Type range <>) of Node_Type;

   type Index_Array is array (Peek_Type range <>) of Base_Peek_Type;
   --  We'd like to use a different index type for Free_List, but Ada
   --  does not allow Integer (Size); "discriminant in constraint must
   --  appear alone"

   type List (Size : Peek_Type) is tagged record
      Head : Base_Peek_Type := Invalid_Peek_Index;
      Tail : Base_Peek_Type := Invalid_Peek_Index;

      Nodes     : Node_Array_Type (1 .. Size);
      Free_List : Index_Array (1 .. Size);
      Free_Last : Base_Peek_Type := 0; --  Append raises Container_Full if user does not call Initialize.

      Count : Ada.Containers.Count_Type := 0;
   end record;

   type Cursor is record
      Ptr : Base_Peek_Type := Invalid_Peek_Index; -- index of Node in List.Nodes
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   No_Element : constant Cursor := (Ptr => Invalid_Peek_Index);

   type Iterator (Container : not null access constant List) is new Iterator_Interfaces.Reversible_Iterator with
   null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end SAL.Gen_Bounded_Definite_Doubly_Linked_Lists;
