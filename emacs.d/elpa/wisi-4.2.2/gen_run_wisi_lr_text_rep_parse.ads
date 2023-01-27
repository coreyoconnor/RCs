--  Abstract :
--
--  Run an Emacs LR text_rep parser as a standalone executable, for debugging.
--
--  See gen_emacs_wisi_*_parse.ads for the Emacs background process.
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
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
generic
   type Parse_Data_Type is new Wisi.Parse_Data_Type with private;

   Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;
   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
   Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
   Text_Rep_File_Name             : in String;

   with function Create_Parser
     (Trace                          : in WisiToken.Trace_Access;
      User_Data                      : in WisiToken.Syntax_Trees.User_Data_Access;
      Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Text_Rep_File_Name             : in String)
     return WisiToken.Parse.LR.Parser.Parser;

procedure Gen_Run_Wisi_LR_Text_Rep_Parse;
