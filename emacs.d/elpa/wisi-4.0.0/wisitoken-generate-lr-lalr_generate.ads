--  Abstract :
--
--  Generalized LALR parse table generator.
--
--  Copyright (C) 2002 - 2003, 2009 - 2010, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
package WisiToken.Generate.LR.LALR_Generate is

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Grammar_File_Name     : in     String;
      Known_Conflicts       : in     Conflict_Lists.Tree := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type := Default_McKenzie_Param;
      Max_Parallel          : in     SAL.Base_Peek_Type  := 15;
      Parse_Table_File_Name : in     String              := "";
      Include_Extra         : in     Boolean             := False;
      Ignore_Conflicts      : in     Boolean             := False;
      Partial_Recursion     : in     Boolean             := True;
      Use_Cached_Recursions : in     Boolean             := False;
      Recursions            : in out WisiToken.Generate.Recursions)
     return Parse_Table_Ptr
   with Pre =>
     Descriptor.Last_Lookahead = Descriptor.First_Nonterminal and
     Descriptor.First_Nonterminal = Descriptor.Accept_ID;
   --  Generate a generalized LALR parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  Unless Ignore_Unused_Tokens is True, raise Grammar_Error if
   --  there are unused tokens.
   --
   --  Unless Ignore_Unknown_Conflicts is True, raise Grammar_Error if there
   --  are unknown conflicts.
   --
   --  Grammar_File_Name is used for error messages.

   ----------
   --  Visible for unit tests

   function LALR_Kernels
     (Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List;

end WisiToken.Generate.LR.LALR_Generate;
