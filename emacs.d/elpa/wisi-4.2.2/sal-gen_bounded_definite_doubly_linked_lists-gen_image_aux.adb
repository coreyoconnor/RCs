--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 Free Software Foundation, Inc.
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
function SAL.Gen_Bounded_Definite_Doubly_Linked_Lists.Gen_Image_Aux (Item : in List; Aux : in Aux_Data) return String
is
   use Ada.Strings.Unbounded;
   Result : Unbounded_String        := To_Unbounded_String ("(");
   Node   : Base_Peek_Type := Item.Head;
begin
   if Node /= Invalid_Peek_Index then
      loop
         Result := Result & Element_Image (Item.Nodes (Node).Element, Aux);

         Node := Item.Nodes (Node).Next;

         exit when Node = Invalid_Peek_Index;

         Result := Result & ", ";
      end loop;
   end if;
   Result := Result & ")";
   return To_String (Result);
end SAL.Gen_Bounded_Definite_Doubly_Linked_Lists.Gen_Image_Aux;
