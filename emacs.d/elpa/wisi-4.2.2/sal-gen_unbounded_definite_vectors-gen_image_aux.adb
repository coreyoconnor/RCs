--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Stephen Leake All Rights Reserved.
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
function SAL.Gen_Unbounded_Definite_Vectors.Gen_Image_Aux
  (Item        : in Vector;
   Aux         : in Aux_Data;
   First       : in Extended_Index := Index_Type'First;
   Last        : in Extended_Index := Index_Type'Last;
   Association : in Boolean := False)
  return String
is
   use Ada.Strings.Unbounded;
   Result       : Unbounded_String        := To_Unbounded_String ("(");
   Actual_First : constant Base_Peek_Type := To_Peek_Type
     (if First = Index_Type'First
      then Item.First_Index
      else First);
   Actual_Last : constant Base_Peek_Type := To_Peek_Type
     (if Last = Index_Type'Last
      then Item.Last_Index
      else Last);
begin
   if First /= No_Index then
      for I in Actual_First .. Actual_Last loop
         if Association then
            Result := Result & Index_Trimmed_Image (To_Index_Type (I)) & " => ";
         end if;
         Result := Result & Element_Image (Item.Elements (I), Aux);
         if I /= Actual_Last then
            Result := Result & ", ";
         end if;
      end loop;
   end if;
   Result := Result & ")";
   return To_String (Result);
end SAL.Gen_Unbounded_Definite_Vectors.Gen_Image_Aux;
