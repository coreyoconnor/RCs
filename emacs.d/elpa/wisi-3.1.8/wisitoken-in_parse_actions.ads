--  Abstract :
--
--  Grammar in parse action routines.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Syntax_Trees;
package WisiToken.In_Parse_Actions is

   function Image
     (Item       : in Syntax_Trees.In_Parse_Actions.Status;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   function Match_Names
     (Tree         : in Syntax_Trees.Tree;
      Tokens       : in Syntax_Trees.Recover_Token_Array;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Syntax_Trees.In_Parse_Actions.Status;
   --  Check that buffer text at Tokens (Start_Index).Name matches buffer
   --  text at Tokens (End_Index).Name. Comparison is controlled by
   --  Descriptor.Case_Insensitive.

   function Propagate_Name
     (Tree       : in     Syntax_Trees.Tree;
      Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Syntax_Trees.In_Parse_Actions.Status;
   function Merge_Names
     (Tree       : in     Syntax_Trees.Tree;
      Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Syntax_Trees.In_Parse_Actions.Status
   renames Propagate_Name;
   --  Set Nonterm.Name to Tokens (Name_Index).Name, or .Byte_Region, if
   --  .Name is Null_Buffer_Region. Return Ok.

   function Merge_Names
     (Tree        : in     Syntax_Trees.Tree;
      Nonterm     : in out Syntax_Trees.Recover_Token;
      Tokens      : in     Syntax_Trees.Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Syntax_Trees.In_Parse_Actions.Status;
   --  Set Nonterm.Name to the merger of Tokens (First_Index ..
   --  Last_Index).Name, return Ok.
   --
   --  If Tokens (Last_Index).Name is Null_Buffer_Region, use Tokens
   --  (Last_Index).Byte_Region instead.

   function Terminate_Partial_Parse
     (Tree                    : in Syntax_Trees.Tree;
      Partial_Parse_Active    : in Boolean;
      Partial_Parse_Byte_Goal : in Buffer_Pos;
      Recover_Active          : in Boolean;
      Nonterm                 : in Syntax_Trees.Recover_Token)
     return Syntax_Trees.In_Parse_Actions.Status;
   pragma Inline (Terminate_Partial_Parse);
   --  If partial parse is complete, raise Wisitoken.Partial_Parse;
   --  otherwise return Ok.

end WisiToken.In_Parse_Actions;
