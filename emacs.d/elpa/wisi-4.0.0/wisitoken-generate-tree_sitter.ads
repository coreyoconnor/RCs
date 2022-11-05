--  Abstract :
--
--  WisiToken utilities for using the tree-sitter parser.
--
--  Copyright (C) 2020, 2021, 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
with WisiToken_Grammar_Runtime;
package WisiToken.Generate.Tree_Sitter is

   procedure Eliminate_Empty_Productions
     (Data : in out WisiToken_Grammar_Runtime.User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree);
   --  Edit Tree to eliminate productions that can be empty, which are
   --  forbidden by the tree-sitter generator.
   --
   --  Also processes %if, so subsequent passes don't have to.

   procedure Print_Tree_Sitter
     (Data             : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree             : in out WisiToken.Syntax_Trees.Tree;
      Lexer            : in     WisiToken.Lexer.Handle;
      Output_File_Name : in     String;
      Language_Name    : in     String);
   --  Tree is 'in out' because we use WisiToken.Syntax_Tree.LR_Utils lists.

   procedure Create_Test_Main (Output_File_Name_Root : in String);

end WisiToken.Generate.Tree_Sitter;
