--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2021, 2022 Free Software Foundation, Inc.
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

with Ada.Strings.Unbounded;
function SAL.Gen_Indefinite_Doubly_Linked_Lists.Gen_Image_Aux
  (Item  : in List;
   Aux   : in Aux_Data;
   First : in Cursor := No_Element)
  return String
is
   use Ada.Strings.Unbounded;
   Result : Unbounded_String := To_Unbounded_String ("(");
   Node   : Node_Access      :=
     (if First = No_Element
      then Item.Head
      else First.Ptr);
begin
   if Node /= null then
      loop
         Append (Result, Element_Image (Node.Element.all, Aux));

         Node := Node.Next;

         exit when Node = null;

         Append (Result, ", ");
      end loop;
   end if;
   Append (Result, ")");
   return To_String (Result);
end SAL.Gen_Indefinite_Doubly_Linked_Lists.Gen_Image_Aux;
