--  Abstract :
--
--  Common utilities for Gen_Run_Wisi_*_Parse
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with Wisi;
with WisiToken;
with Wisi.Parse_Context;
package Run_Wisi_Common_Parse is

   procedure Parse_File (Language : in Wisi.Parse_Context.Language; Trace : in WisiToken.Trace_Access);
   --  Reads command line, processes command(s).

end Run_Wisi_Common_Parse;
