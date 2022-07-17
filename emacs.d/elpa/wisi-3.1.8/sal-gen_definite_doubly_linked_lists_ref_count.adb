--  Abstract :
--
--  see spec
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

package body SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count is

   ---------
   --  Public operations, declaration order.

   overriding
   procedure Adjust (Container : in out List)
   is
      Next_Source : Node_Access := Container.Head;
      New_Node    : Node_Access;
   begin
      if Next_Source = null then
         return;
      end if;

      Container.Tail := null;

      loop
         New_Node := new Node_Type'
           (Element   => Next_Source.Element,
            Next      => null,
            Prev      => Container.Tail,
            Ref_Count => 0);

         if Container.Tail = null then
            Container.Head := New_Node;
            Container.Tail := New_Node;
         else
            Container.Tail.Next := New_Node;
            Container.Tail      := New_Node;
         end if;
         Next_Source := Next_Source.Next;
         exit when Next_Source = null;
      end loop;
   end Adjust;

   overriding
   procedure Finalize (Container : in out List)
   is
      Next : Node_Access := Container.Head;
   begin
      loop
         exit when Next = null;
         Next := Container.Head.Next;
         --  We raise an exception here, even though Finalize never should,
         --  because Finalize is also renamed to Clear, and called as a
         --  normal procedure.
         if Container.Enable_Checks and Container.Head.Ref_Count /= 0 then
            raise Invalid_Operation with "ref_count " & Container.Head.Ref_Count'Image;
         end if;
         Free (Container.Head);
         Container.Head := Next;
      end loop;
      Container.Tail  := null;
      Container.Count := 0;
   end Finalize;

   procedure Enable_Ref_Count_Check (Container : in out List; Enable : in Boolean)
   is begin
      Container.Enable_Checks := Enable;
   end Enable_Ref_Count_Check;

   procedure Check_Ref_Counts (Container : in out List)
   is
      Next : Node_Access := Container.Head;
   begin
      loop
         exit when Next = null;
         if Container.Enable_Checks and Next.Ref_Count /= 0 then
            raise Invalid_Operation with "ref_count " & Next.Ref_Count'Image;
         end if;
         Next := Next.Next;
      end loop;
   end Check_Ref_Counts;

   function Length (Container : in List) return Ada.Containers.Count_Type
   is begin
      return Container.Count;
   end Length;

   procedure Append (Container : in out List; Element : in Element_Type)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Node_Access := new Node_Type'
        (Element   => Element,
         Prev      => Container.Tail,
         Next      => null,
         Ref_Count => 0);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Tail.Next := New_Node;
         Container.Tail      := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Append;

   procedure Prepend (Container : in out List; Element : in Element_Type)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Node_Access := new Node_Type'
        (Element   => Element,
         Prev      => null,
         Next      => Container.Head,
         Ref_Count => 0);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Head.Prev := New_Node;
         Container.Head      := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Prepend;

   function To_List (Element : in Element_Type) return List
   is begin
      return Result : List do
         Result.Append (Element);
      end return;
   end To_List;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Ptr /= null;
   end Has_Element;

   function First (Container : in List'Class) return Cursor
   is begin
      if Container.Head = null then
         return (Ada.Finalization.Controlled with Ptr => null);
      else
         Container.Head.Ref_Count := @ + 1;
         return (Ada.Finalization.Controlled with Ptr => Container.Head);
      end if;
   end First;

   function Last (Container : in List'Class) return Cursor
   is begin
      if Container.Tail = null then
         return (Ada.Finalization.Controlled with Ptr => null);
      else
         Container.Tail.Ref_Count := @ + 1;
         return (Ada.Finalization.Controlled with Ptr => Container.Tail);
      end if;
   end Last;

   procedure Next (Position : in out Cursor)
   is begin
      Position.Ptr.Ref_Count := @ - 1;
      if Position.Ptr.Next /= null then
         Position.Ptr.Next.Ref_Count := @ + 1;
      end if;

      Position.Ptr := Position.Ptr.Next;
   end Next;

   function Next (Position : in Cursor) return Cursor
   is begin
      if Position.Ptr.Next /= null then
         Position.Ptr.Next.Ref_Count := @ + 1;
      end if;

      return (Ada.Finalization.Controlled with Ptr => Position.Ptr.Next);
   end Next;

   procedure Previous (Position : in out Cursor)
   is begin
      Position.Ptr.Ref_Count := @ - 1;
      if Position.Ptr.Prev /= null then
         Position.Ptr.Prev.Ref_Count := @ + 1;
      end if;

      Position.Ptr := Position.Ptr.Prev;
   end Previous;

   function Previous (Position : in Cursor) return Cursor
   is begin
      return Result : constant Cursor := (Ada.Finalization.Controlled with Ptr => Position.Ptr.Prev) do
         if Result.Ptr /= null then
            Result.Ptr.Ref_Count := @ + 1;
         end if;
      end return;
   end Previous;

   function Element (Position : in Cursor) return Element_Type
   is begin
      return Position.Ptr.Element;
   end Element;

   procedure Delete (Container : in out List; Position : in out Cursor'Class)
   is
      use all type Ada.Containers.Count_Type;
      Node : Node_Access renames Position.Ptr;
   begin
      if Container.Enable_Checks and Node.Ref_Count /= 1 then
         raise Invalid_Operation with "ref_count " & Node.Ref_Count'Image;
      end if;

      if Node.Next = null then
         Container.Tail := Node.Prev;
      else
         Node.Next.Prev := Node.Prev;
      end if;
      if Node.Prev = null then
         Container.Head := Node.Next;
      else
         Node.Prev.Next := Node.Next;
      end if;
      Free (Node);

      Container.Count := Container.Count - 1;
   end Delete;

   procedure Replace_Element
     (Position    : in Cursor'Class;
      New_Element : in Element_Type)
   is begin
      Position.Ptr.Element := New_Element;
   end Replace_Element;

   function Append (Container : in out List'Class; Element : in Element_Type) return Cursor
   is begin
      Append (Container, Element);
      Container.Tail.Ref_Count := @ + 1;
      return (Ada.Finalization.Controlled with Ptr => Container.Tail);
   end Append;

   function Insert
     (Container : in out List'Class;
      Before    : in     Cursor'Class;
      Element   : in     Element_Type)
     return Cursor
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Before.Ptr = null then
         return Container.Append (Element);
      else
         return Result : Cursor do
            if Before.Ptr = Container.Head then
               declare
                  --  old list: before ...
                  --  newlist:  new  before ...
                  New_Node : constant Node_Access := new Node_Type'
                    (Element   => Element,
                     Prev      => null,
                     Next      => Before.Ptr,
                     Ref_Count => 1);
               begin
                  Before.Ptr.Prev := New_Node;
                  Container.Head  := New_Node;
                  Result.Ptr      := New_Node;
               end;
            else
               declare
                  --  old list: ... prev  before ...
                  --  newlist:  ... prev  new  before ...
                  New_Node : constant Node_Access := new Node_Type'
                    (Element   => Element,
                     Prev      => Before.Ptr.Prev,
                     Next      => Before.Ptr,
                     Ref_Count => 1);
               begin
                  Before.Ptr.Prev.Next := New_Node;
                  Before.Ptr.Prev      := New_Node;
                  Result.Ptr           := New_Node;
               end;
            end if;
            Container.Count := Container.Count + 1;
         end return;
      end if;
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : in     Cursor'Class;
      Element   : in     Element_Type)
   is
      Junk : Cursor := Insert (Container, Before, Element);
      pragma Unreferenced (Junk);
   begin
      null;
   end Insert;

   function Contains
     (Container : in List;
      Item      : in Cursor'Class)
     return Boolean
   is
      Node : Node_Access := Container.Head;
   begin
      loop
         exit when Node = null;
         if Node = Item.Ptr then
            return True;
         end if;
         Node := Node.Next;
      end loop;
      return False;
   end Contains;

   ----------
   --  Private operations, declaration order

   overriding procedure Finalize (Object : in out Cursor)
   is begin
      if Object.Ptr /= null then
         Object.Ptr.Ref_Count := @ - 1;
      end if;
   end Finalize;

   overriding procedure Adjust (Object : in out Cursor)
   is begin
      if Object.Ptr /= null then
         Object.Ptr.Ref_Count := @ + 1;
      end if;
   end Adjust;

end SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count;
