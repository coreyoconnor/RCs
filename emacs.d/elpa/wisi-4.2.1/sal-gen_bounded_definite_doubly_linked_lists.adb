--  Abstract :
--
--  see spec
--
--  Copyright (C) 2020, 2021 Free Software Foundation, Inc.
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

package body SAL.Gen_Bounded_Definite_Doubly_Linked_Lists is

   function Allocate (Container : in out List) return Peek_Type
   is begin
      if Container.Free_Last = 0 then
         raise SAL.Container_Full;
      end if;
      return Result : constant Peek_Type := Container.Free_List (Container.Free_Last) do
         Container.Free_List (Container.Free_Last) := Invalid_Peek_Index;
         Container.Free_Last := @ - 1;
      end return;
   end Allocate;

   procedure Free (Container : in out List; Node : in out Base_Peek_Type)
   is begin
      Container.Free_Last := @ + 1;
      Container.Free_List (Container.Free_Last) := Node;
      Node := Invalid_Peek_Index;
   end Free;

   procedure Delete_Node (Container : in out List; Node : in out Base_Peek_Type)
   is begin
      if Container.Nodes (Node).Next = Invalid_Peek_Index then
         Container.Tail := Container.Nodes (Node).Prev;
      else
         Container.Nodes (Container.Nodes (Node).Next).Prev := Container.Nodes (Node).Prev;
      end if;
      if Container.Nodes (Node).Prev = Invalid_Peek_Index then
         Container.Head := Container.Nodes (Node).Next;
      else
         Container.Nodes (Container.Nodes (Node).Prev).Next := Container.Nodes (Node).Next;
      end if;
      Free (Container, Node);
   end Delete_Node;

   ---------
   --  Public operations, declaration order.

   procedure Initialize (Container : in out List)
   is begin
      for I in 1 .. Container.Size loop
         Container.Free_List (I) := I;
      end loop;
      Container.Free_Last := Container.Size;
   end Initialize;

   function Empty_List (Size : in Peek_Type) return List
   is begin
      return Result : List (Size) do
         for I in 1 .. Size loop
            Result.Free_List (I) := I;
         end loop;
         Result.Free_Last := Size;
      end return;
   end Empty_List;

   procedure Clear (Container : in out List)
   is begin
      Container.Head := Invalid_Peek_Index;
      Container.Tail := Invalid_Peek_Index;
      for I in 1 .. Container.Size loop
         Container.Free_List (I) := I;
      end loop;
      Container.Free_Last := Container.Size;

      for I in 1 .. Container.Size loop
         Container.Nodes (I).Next := Invalid_Peek_Index;
         Container.Nodes (I).Prev := Invalid_Peek_Index;
      end loop;

      Container.Count := 0;
   end Clear;

   function Length (Container : in List) return Ada.Containers.Count_Type
   is begin
      return Container.Count;
   end Length;

   procedure Append (Container : in out List; Element : in Element_Type)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Peek_Type := Allocate (Container);
   begin
      Container.Nodes (New_Node) :=
        (Element => Element,
         Prev    => Container.Tail,
         Next    => Invalid_Peek_Index);

      if Container.Tail = Invalid_Peek_Index then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Nodes (Container.Tail).Next := New_Node;
         Container.Tail                        := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Append;

   procedure Prepend (Container : in out List; Element : in Element_Type)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Peek_Type := Allocate (Container);
   begin
      Container.Nodes (New_Node) :=
        (Element => Element,
         Prev    => Invalid_Peek_Index,
         Next    => Container.Head);

      if Container.Tail = Invalid_Peek_Index then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Nodes (Container.Head).Prev := New_Node;
         Container.Head                        := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Prepend;

   function To_List (Element : in Element_Type; Size : in Peek_Type) return List
   is begin
      return Result : List (Size) do
         Result.Append (Element);
      end return;
   end To_List;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Ptr /= Invalid_Peek_Index;
   end Has_Element;

   function First (Container : in List) return Cursor
   is begin
      if Container.Head = Invalid_Peek_Index then
         return (Ptr => Invalid_Peek_Index);
      else
         return (Ptr => Container.Head);
      end if;
   end First;

   function Last (Container : in List) return Cursor
   is begin
      if Container.Tail = Invalid_Peek_Index then
         return (Ptr => Invalid_Peek_Index);
      else
         return (Ptr => Container.Tail);
      end if;
   end Last;

   procedure Next (Container : in List; Position : in out Cursor)
   is begin
      if Position.Ptr /= Invalid_Peek_Index then
         if Container.Nodes (Position.Ptr).Next = Invalid_Peek_Index then
            Position.Ptr := Invalid_Peek_Index;
         else
            Position.Ptr := Container.Nodes (Position.Ptr).Next;
         end if;
      end if;
   end Next;

   function Next (Container : in List; Position : in Cursor) return Cursor
   is begin
      if Position.Ptr = Invalid_Peek_Index then
         return Position;
      else
         if Container.Nodes (Position.Ptr).Next = Invalid_Peek_Index then
            return (Ptr => Invalid_Peek_Index);
         else
            return (Ptr => Container.Nodes (Position.Ptr).Next);
         end if;
      end if;
   end Next;

   procedure Previous (Container : in List; Position : in out Cursor)
   is begin
      if Position.Ptr = Invalid_Peek_Index then
         return;
      else
         if Container.Nodes (Position.Ptr).Prev = Invalid_Peek_Index then
            Position.Ptr := Invalid_Peek_Index;
         else
            Position.Ptr := Container.Nodes (Position.Ptr).Prev;
         end if;
      end if;
   end Previous;

   function Previous (Container : in List; Position : in Cursor) return Cursor
   is begin
      if Position.Ptr = Invalid_Peek_Index then
         return Position;
      else
         if Container.Nodes (Position.Ptr).Prev = Invalid_Peek_Index then
            return (Ptr => Invalid_Peek_Index);
         else
            return (Ptr => Container.Nodes (Position.Ptr).Prev);
         end if;
      end if;
   end Previous;

   function Element (Container : in List; Position : in Cursor) return Element_Type
   is begin
      return Container.Nodes (Position.Ptr).Element;
   end Element;

   procedure Delete (Container : in out List; Position : in out Cursor)
   is
      use all type Ada.Containers.Count_Type;
   begin
      Delete_Node (Container, Position.Ptr);
      Position        := (Ptr => Invalid_Peek_Index);
      Container.Count := Container.Count - 1;
   end Delete;

   procedure Delete_First (Container : in out List)
   is
      use all type Ada.Containers.Count_Type;
      Node : Base_Peek_Type := Container.Head;
   begin
      Delete_Node (Container, Node);
      Container.Count := Container.Count - 1;
   end Delete_First;

   function Append (Container : in out List; Element : in Element_Type) return Cursor
   is begin
      Append (Container, Element);
      return (Ptr => Container.Tail);
   end Append;

   function Insert
     (Container : in out List;
      Before    : in     Cursor;
      Element   : in     Element_Type)
     return Cursor
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Before = (Ptr => Invalid_Peek_Index) then
         return Container.Append (Element);
      else
         return Result : Cursor do
            if Before.Ptr = Container.Head then
               declare
                  --  old list: before ...
                  --  newlist:  new  before ...
                  New_Node : constant Peek_Type := Allocate (Container);
               begin
                  Container.Nodes (New_Node) :=
                    (Element => Element,
                     Prev    => Invalid_Peek_Index,
                     Next    => Before.Ptr);

                  Container.Nodes (Before.Ptr).Prev := New_Node;
                  Container.Head                    := New_Node;
                  Result.Ptr                        := New_Node;
               end;
            else
               declare
                  --  old list: ... prev  before ...
                  --  newlist:  ... prev  new  before ...
                  New_Node : constant Peek_Type := Allocate (Container);
               begin
                  Container.Nodes (New_Node) :=
                    (Element => Element,
                     Prev    => Container.Nodes (Before.Ptr).Prev,
                     Next    => Before.Ptr);

                  Container.Nodes (Container.Nodes (Before.Ptr).Prev).Next := New_Node;

                  Container.Nodes (Before.Ptr).Prev := New_Node;

                  Result.Ptr := New_Node;
               end;
            end if;
            Container.Count := Container.Count + 1;
         end return;
      end if;
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : in     Cursor;
      Element   : in     Element_Type)
   is
      Junk : Cursor := Insert (Container, Before, Element);
      pragma Unreferenced (Junk);
   begin
      null;
   end Insert;

   function Constant_Ref (Container : aliased in List; Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Container.Nodes (Position.Ptr).Element'Access, Dummy => 1);
   end Constant_Ref;

   function Variable_Ref (Container : aliased in out List; Position : in Cursor) return Variable_Reference_Type
   is begin
      return (Element => Container.Nodes (Position.Ptr).Element'Access, Dummy => 1);
   end Variable_Ref;

   function Iterate (Container : aliased in List) return Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor
   is begin
      return First (Object.Container.all);
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      return Last (Object.Container.all);
   end Last;

   overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   is begin
      return Next (Object.Container.all, Position);
   end Next;

   overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor
   is begin
      return Previous (Object.Container.all, Position);
   end Previous;

end SAL.Gen_Bounded_Definite_Doubly_Linked_Lists;
