--  Abstract :
--
--  LR1 (Left-to-right scanning 1 look-ahead) parser table generator.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with System.Multiprocessors;
with WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
package WisiToken.Generate.LR.LR1_Generate is

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Grammar_File_Name     : in     String;
      Known_Conflicts       : in     Conflict_Lists.Tree              := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type              := Default_McKenzie_Param;
      Max_Parallel          : in     SAL.Base_Peek_Type               := 15;
      Parse_Table_File_Name : in     String                           := "";
      Include_Extra         : in     Boolean                          := False;
      Ignore_Conflicts      : in     Boolean                          := False;
      Partial_Recursion     : in     Boolean                          := True;
      Task_Count            : in     System.Multiprocessors.CPU_Range := 1;
      Hash_Table_Size       : in     Positive                         := LR1_Items.Item_Set_Trees.Default_Rows;
      Use_Cached_Recursions : in     Boolean                          := False;
      Recursions            : in out WisiToken.Generate.Recursions)
     return Parse_Table_Ptr
   with Pre => Descriptor.First_Nonterminal = Descriptor.Accept_ID;
   --  Generate a generalized LR1 parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  Sets Recursive components in Grammar.
   --
   --  If Trace, output debug info to Standard_Error about generation
   --  process. We don't use WisiToken.Trace here; we often want to
   --  see a trace of the parser execution without the parser
   --  generation.
   --
   --  Unless Ignore_Unused_Tokens is True, raise Grammar_Error if
   --  there are unused tokens.
   --
   --  Unless Ignore_Unknown_Conflicts is True, raise Grammar_Error if there
   --  are unknown conflicts.
   --
   --  Use Task_Count tasks in computing LR1 items. Default is 1 so unit
   --  tests return repeatable results.

   ----------
   --  visible for unit test

   function LR1_Item_Sets_Single
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor;
      Hash_Table_Size         : in Positive := LR1_Items.Item_Set_Trees.Default_Rows)
     return LR1_Items.Item_Set_List;
   --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items", no tasking

   function LR1_Item_Sets_Parallel
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor;
      Task_Count              : in System.Multiprocessors.CPU_Range;
      Hash_Table_Size         : in Positive := LR1_Items.Item_Set_Trees.Default_Rows)
     return LR1_Items.Item_Set_List;
   --  With tasking; used if State_Count known.

end WisiToken.Generate.LR.LR1_Generate;
