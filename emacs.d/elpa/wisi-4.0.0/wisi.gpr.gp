--  Abstract :
--
--  Make installed and source ELPA package wisi Ada code available for
--  other projects.
--
--  Copyright (C) 2017, 2019, 2021 Free Software Foundation, Inc.
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

with "gnatcoll";
with "standard_common";
#if ELPA="no"
with "aunit_ext";
with "sal_devel";
with "wisitoken_devel";
#end if;
project Wisi is

   for Source_Dirs use (".");

   case Standard_Common.Profile is
   when "On" =>
      for Object_Dir use "obj_pro";
      for Exec_Dir use "exec_pro";

   when "Off" =>
      for Object_Dir use "obj";
      for Exec_Dir use ".";
   end case;

   for Languages use
   ("Ada"
#if ELPA="yes" 
   ,"C" -- C needed for wisitoken_grammar_re2c.c
#end if;
   );

   package Compiler is

      case Standard_Common.Build is
      when "Debug" =>
         for Default_Switches ("Ada") use
           Standard_Common.Compiler.Common_Switches &
           Standard_Common.Compiler.Style_Checks &
           Standard_Common.Compiler.Debug_Switches;
#if ELPA="yes" 
         for Default_Switches ("C") use Standard_Common.Compiler.Debug_Switches_C;
#end if;

      when "Normal" =>
         for Default_Switches ("Ada") use
           Standard_Common.Compiler.Common_Switches &
           Standard_Common.Compiler.Style_Checks &
           Standard_Common.Compiler.Release_Switches;
#if ELPA="yes" 
         for Default_Switches ("C") use Standard_Common.Compiler.Release_Switches_C;
#end if;
      end case;

   end Compiler;

end Wisi;
