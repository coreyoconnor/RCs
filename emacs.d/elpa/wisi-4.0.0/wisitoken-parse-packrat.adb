--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
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

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
package body WisiToken.Parse.Packrat is

   overriding
   procedure Execute_Actions
     (Parser              : in out Packrat.Parser;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region)
   is
      use all type WisiToken.Syntax_Trees.User_Data_Access;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use all type Syntax_Trees.Node_Label;
         use all type Syntax_Trees.Post_Parse_Action;
         use all type WisiToken.Syntax_Trees.Node_Access;
      begin
         if Tree.Label (Node) /= Nonterm or else
           not Overlaps (Tree.Byte_Region (Node, Trailing_Non_Grammar => False), Action_Region_Bytes)
         then
            return;
         end if;

         declare
            Tree_Children     : constant Syntax_Trees.Node_Access_Array := Tree.Children (Node);
            Post_Parse_Action : constant Syntax_Trees.Post_Parse_Action := Parser.Get_Post_Parse_Action
              (Tree.Production_ID (Node));
         begin
            for Child of Tree_Children loop
               if Child /= null and then Overlaps
                 (Tree.Byte_Region (Child, Trailing_Non_Grammar => False), Action_Region_Bytes)
               then
                  Process_Node (Tree, Child);
               end if;
            end loop;

            Parser.User_Data.Reduce (Tree, Node);

            if Post_Parse_Action /= null then
               Post_Parse_Action (Parser.User_Data.all, Tree, Node);
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data = null then
         return;
      end if;

      if Trace_Action > Outline then
         if Trace_Action > Extra then
            Parser.Tree.Print_Tree (Parser.Tree.Root);
            Parser.Tree.Lexer.Trace.New_Line;
         end if;
         Parser.Tree.Lexer.Trace.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root));
      end if;

      Parser.User_Data.Initialize_Actions (Parser.Tree);
      Process_Node (Parser.Tree, Parser.Tree.Root);
   exception
   when E : others =>
      if Debug_Mode then
         Parser.Tree.Lexer.Trace.Put_Line
           (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Parser.Tree.Lexer.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         Parser.Tree.Lexer.Trace.New_Line;
      end if;
      raise;
   end Execute_Actions;

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Syntax_Trees.Stream_ID;
      Element : in Syntax_Trees.Stream_Index)
     return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Tree.Get_Node_Index (Stream, Element)'Image;
      end if;
   end Image_Pos;

end WisiToken.Parse.Packrat;
