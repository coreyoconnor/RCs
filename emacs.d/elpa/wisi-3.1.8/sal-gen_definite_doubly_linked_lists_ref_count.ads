--  Abstract :
--
--  A generic doubly linked list with definite elements, with
--  reference counting on cursors to detect dangling references.
--
--  WORKAROUND: there is a bug in GNAT Community 2020 (Eurocontrol
--  ticket V107-045) that causes reference counting to be inaccurate
--  in some cases, so we support turning off the reference counting.
--
--  Rationale for not implementing reference types and iterators:
--  Consider a typical reference type use:
--
--  declare
--     A : Element_Type renames List.First;
--     To_Delete : Cursor := List.First;
--  begin
--     Delete (To_Delete);
--  end;
--
--  The reference object exists only while evaluating the renames, so
--  it cannot assert any kind of lock on the element or list that
--  survives thru the call to Delete and is then released. We would
--  have to use something like:
--
--  declare
--     A_Ref : constant Reference_Type := List.First;
--     A : Element_Type renames Element (A_Ref);
--     To_Delete : Cursor := List.First;
--  begin
--     Delete (To_Delete);
--  end;
--
--  Where "Reference_Type" is opaque, and thus cannot be used for iterators.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
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
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
package SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count is

   type List is new Ada.Finalization.Controlled with private;
   --  We cannot implement reference counting that detects reference
   --  types (see discussion above), so no reference types, no iterators.

   type List_Access_Constant is access constant List;
   for List_Access_Constant'Storage_Size use 0;

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   Empty_List : constant List;

   overriding procedure Adjust (Container : in out List);
   --  Deep copy.

   overriding procedure Finalize (Container : in out List);
   --  Free all items in List.

   procedure Enable_Ref_Count_Check (Container : in out List; Enable : in Boolean);
   --  Enable or disable checks; default is enabled.
   --
   --  This is useful when there are bugs that you want to ignore.

   procedure Check_Ref_Counts (Container : in out List);
   --  Raises SAL.Invalid_Operation if any ref count is non-zero.

   procedure Clear (Container : in out List) renames Finalize;

   function Length (Container : in List) return Ada.Containers.Count_Type;

   procedure Append (Container : in out List; Element : in Element_Type);

   procedure Prepend (Container : in out List; Element : in Element_Type);

   function To_List (Element : in Element_Type) return List;

   type Cursor is tagged private;
   --  Cursor is not simply 'private', to allow implementing reference
   --  counting.

   function Has_Element (Position : in Cursor) return Boolean;

   No_Element : constant Cursor;

   function Ref_Count (Position : in Cursor) return Integer;
   --  For debugging, tests, preconditions.

   function First (Container : in List'Class) return Cursor;
   function Last (Container : in List'Class) return Cursor;

   procedure Next (Position : in out Cursor)
   with Pre => Has_Element (Position);

   function Next (Position : in Cursor) return Cursor
   with Pre => Has_Element (Position);

   procedure Previous (Position : in out Cursor)
   with Pre => Has_Element (Position);

   function Previous (Position : in Cursor) return Cursor
   with Pre => Has_Element (Position);

   function Element (Position : in Cursor) return Element_Type
   with Pre => Has_Element (Position);

   procedure Delete (Container : in out List; Position : in out Cursor'Class)
   with Pre => Has_Element (Position) and then Ref_Count (Position) = 1;
   --  Raises SAL.Invalid_Operation if any other cursors, iterators, or
   --  reference objects reference the same element as Position.

   procedure Replace_Element
     (Position    : in Cursor'Class;
      New_Element : in Element_Type)
   with Pre => Has_Element (Position);

   function Append (Container : in out List'Class; Element : in Element_Type) return Cursor;

   procedure Insert
     (Container : in out List;
      Before    : in     Cursor'Class;
      Element   : in     Element_Type);
   function Insert
     (Container : in out List'Class;
      Before    : in     Cursor'Class;
      Element   : in     Element_Type)
     return Cursor;
   --  If Before is No_Element, insert after Last.

   ----------
   --  Iterator replacements, since we don't have Ada.Iterator_Interfaces
   --  iterators.

   function Contains
     (Container : in List;
      Item      : in Cursor'Class)
     return Boolean;

private
   type Node_Type;

   type Node_Access is access Node_Type;

   type Node_Type is record
      Element   : aliased Element_Type;
      Prev      : Node_Access;
      Next      : Node_Access;
      Ref_Count : Integer := 0;
      --  Ref_Count does not include the internal pointers Prev, Next, Head,
      --  Tail, since there are always two of those for each node.
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   type List is new Ada.Finalization.Controlled with record
      Head  : Node_Access               := null;
      Tail  : Node_Access               := null;
      Count : Ada.Containers.Count_Type := 0;

      Enable_Checks : Boolean := True;
   end record;

   Empty_List : constant List := (Ada.Finalization.Controlled with others => <>);

   type Cursor is new Ada.Finalization.Controlled with record
      Ptr : Node_Access;
   end record;

   overriding procedure Finalize (Object : in out Cursor);
   --  Decrement external reference count of Object.Ptr.all.

   overriding procedure Adjust (Object : in out Cursor);
   --  Increment external reference count of Object.Ptr.all.

   No_Element : constant Cursor := (Ada.Finalization.Controlled with Ptr => null);

   function Ref_Count (Position : in Cursor) return Integer
   is (Position.Ptr.Ref_Count);

end SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count;
