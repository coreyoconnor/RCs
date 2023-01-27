--  Abstract:
--
--  See spec.
--
--  Copyright (C) 1997 - 2004, 2006, 2009, 2019 - 2022 Free Software Foundation, Inc.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body SAL is

   function Version return String is
   begin
      return "SAL 3.7";
   end Version;

   function String_Compare (Left, Right : in String) return Compare_Result
   is
      J : Integer := Right'First;
   begin
      if Left'Length = 0 then
         if Right'Length = 0 then
            return Equal;
         else
            return Less;
         end if;
      else
         if Right'Length = 0 then
            return Greater;
         end if;

         for I in Left'Range loop
            if Left (I) > Right (J) then
               return Greater;
            elsif Left (I) < Right (J) then
               return Less;
            end if;

            J := J + 1;
            if I < Left'Last and J > Right'Last then
               return Greater;
            end if;
         end loop;

         if J < Right'Last then
            return Less;
         else
            return Equal;
         end if;
      end if;
   end String_Compare;

   function Gen_Compare_Integer (Left, Right : in Item_Type) return Compare_Result
   is begin
      if Left < Right then
         return Less;
      elsif Left > Right then
         return Greater;
      else
         return Equal;
      end if;
   end Gen_Compare_Integer;

end SAL;
