--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020, 2022 All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with GNATCOLL.Memory;
with Run_Wisi_Common_Parse;
with WisiToken.Text_IO_Trace;
procedure Gen_Run_Wisi_LR_Parse
is
   Trace               : aliased WisiToken.Text_IO_Trace.Trace;
   Parse_Data_Template : aliased Parse_Data_Type;
begin
   --  FIXME: report memory during lexer, parser create
   --  WisiToken.Trace_Memory            := 2;
   --  WisiToken.Trace_Incremental_Parse := 1;
   GNATCOLL.Memory.Configure
     (Activate_Monitor      => True,
      Stack_Trace_Depth     => 10,
      Reset_Content_On_Free => False);

   declare
      Lexer : constant WisiToken.Lexer.Handle := Create_Lexer (Trace'Unchecked_Access);
      --  No point in reporting lexer memory; it's very small
      Parse_Table : constant WisiToken.Parse.LR.Parse_Table_Ptr := Create_Parse_Table;
   begin
      Trace.Put_Line ("parse table created");
      WisiToken.Report_Memory (Trace, Prefix => True);

      Run_Wisi_Common_Parse.Parse_File
        ((Descriptor, Lexer, Parse_Table, Create_Productions, Partial_Parse_Active,
          Partial_Parse_Byte_Goal, Language_Fixes, Language_Matching_Begin_Tokens, Language_String_ID_Set,
          Parse_Data_Template'Unchecked_Access),
         Trace'Unchecked_Access);
   end;
end Gen_Run_Wisi_LR_Parse;
