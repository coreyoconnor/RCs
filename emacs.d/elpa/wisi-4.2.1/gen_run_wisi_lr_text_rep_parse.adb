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

with Ada.Command_Line;
with Ada.Directories;
with Run_Wisi_Common_Parse;
with WisiToken.Text_IO_Trace;
procedure Gen_Run_Wisi_LR_Text_Rep_Parse
is
   Trace : aliased WisiToken.Text_IO_Trace.Trace;

   Text_Rep_File_Name_Full : constant String := Ada.Directories.Containing_Directory
     (Ada.Command_Line.Command_Name) & "/" & Text_Rep_File_Name;

   function Factory return WisiToken.Parse.Base_Parser_Access
   is begin
      return new WisiToken.Parse.LR.Parser.Parser'
        (Create_Parser
           (Trace'Unchecked_Access,
            User_Data                      => new Parse_Data_Type,
            Language_Fixes                 => Language_Fixes,
            Language_Matching_Begin_Tokens => Language_Matching_Begin_Tokens,
            Language_String_ID_Set         => Language_String_ID_Set,
            Text_Rep_File_Name             => Text_Rep_File_Name_Full));
   end Factory;

   procedure Free_Parser (Object : in out WisiToken.Parse.Base_Parser_Access)
   is
      LR_Parser : WisiToken.Parse.LR.Parser.Parser_Access := WisiToken.Parse.LR.Parser.Parser_Access (Object);
   begin
      WisiToken.Parse.LR.Parser.Free (LR_Parser);
      Object := null;
   end Free_Parser;

begin
   Run_Wisi_Common_Parse.Parse_File
     (Factory'Unrestricted_Access, Free_Parser'Unrestricted_Access, Trace'Unchecked_Access);
end Gen_Run_Wisi_LR_Text_Rep_Parse;
