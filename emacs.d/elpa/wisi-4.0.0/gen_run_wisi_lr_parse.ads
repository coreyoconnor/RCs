--  Abstract :
--
--  Run an Emacs LR parser as a standalone executable, for debugging.
--
--  See gen_emacs_wisi_lr_parse.ads for the Emacs background process.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
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
with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
generic
   type Parse_Data_Type is new Wisi.Parse_Data_Type with private;

   Descriptor                     : in WisiToken.Descriptor_Access_Constant;
   Partial_Parse_Active           : in WisiToken.Boolean_Access;
   Partial_Parse_Byte_Goal        : in WisiToken.Buffer_Pos_Access;
   Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;
   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
   Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;

   with function Create_Lexer (Trace : in WisiToken.Trace_Access) return WisiToken.Lexer.Handle;
   with function Create_Parse_Table return WisiToken.Parse.LR.Parse_Table_Ptr;
   with function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector;

procedure Gen_Run_Wisi_LR_Parse;
