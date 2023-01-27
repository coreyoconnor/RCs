--  Abstract :
--
--  See spec.
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

with Ada.Characters.Handling;
with WisiToken.Lexer;
package body WisiToken.In_Parse_Actions is

   function Image
     (Item       : in Syntax_Trees.In_Parse_Actions.Status;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use WisiToken.Syntax_Trees;
   begin
      case Item.Label is
      when Syntax_Trees.In_Parse_Actions.Ok =>
         return Syntax_Trees.In_Parse_Actions.Status_Label'Image (Item.Label);
      when Syntax_Trees.In_Parse_Actions.Error =>
         declare
            Begin_Node : constant Valid_Node_Access := Tree.Child (Error_Node, Item.Begin_Name);
            End_Node   : constant Valid_Node_Access := Tree.Child (Error_Node, Item.End_Name);
         begin
            return '(' & Syntax_Trees.In_Parse_Actions.Status_Label'Image (Item.Label) & ", " &
              Tree.Image (Begin_Node) & "'" & Tree.Lexer.Buffer_Text
                (Tree.Byte_Region (Begin_Node, Trailing_Non_Grammar => False)) & "'," &
              Tree.Image (End_Node) & "'" & Tree.Lexer.Buffer_Text
                (Tree.Byte_Region (End_Node, Trailing_Non_Grammar => False)) & "')";
         end;
      end case;
   end Image;

   function Match_Names
     (Tree         : in Syntax_Trees.Tree;
      Tokens       : in Syntax_Trees.Recover_Token_Array;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Syntax_Trees.In_Parse_Actions.Status
   is
      use Syntax_Trees;
   begin
      if Tree.Contains_Virtual_Terminal (Tokens (Start_Index)) or
        Tree.Contains_Virtual_Terminal (Tokens (End_Index))
      then
         return (Label => Syntax_Trees.In_Parse_Actions.Ok);
      end if;

      declare
         Start_Name_Region : constant Buffer_Region := Tree.Name (Tokens (Start_Index));
         End_Name_Region   : constant Buffer_Region := Tree.Name (Tokens (End_Index));

         function Equal return Boolean
         is
            use Ada.Characters.Handling;
            Start_Name : constant String :=
              (if Tree.Lexer.Descriptor.Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Start_Name_Region))
               else Tree.Lexer.Buffer_Text (Start_Name_Region));
            End_Name  : constant String :=
              (if Tree.Lexer.Descriptor.Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (End_Name_Region))
               else Tree.Lexer.Buffer_Text (End_Name_Region));
         begin
            return Start_Name = End_Name;
         end Equal;
      begin

         if End_Optional then
            if End_Name_Region = Null_Buffer_Region then
               return (Label => Syntax_Trees.In_Parse_Actions.Ok);

            elsif Start_Name_Region = Null_Buffer_Region then
               return (Syntax_Trees.In_Parse_Actions.Extra_Name_Error, Start_Index, End_Index);
            else
               if Equal then
                  return (Label => Syntax_Trees.In_Parse_Actions.Ok);
               else
                  return (Syntax_Trees.In_Parse_Actions.Match_Names_Error, Start_Index, End_Index);
               end if;
            end if;

         else
            if Start_Name_Region = Null_Buffer_Region then
               if End_Name_Region = Null_Buffer_Region then
                  return (Label => Syntax_Trees.In_Parse_Actions.Ok);
               else
                  return (Syntax_Trees.In_Parse_Actions.Extra_Name_Error, Start_Index, End_Index);
               end if;

            elsif End_Name_Region = Null_Buffer_Region then
               return (Syntax_Trees.In_Parse_Actions.Missing_Name_Error, Start_Index, End_Index);

            else
               if Equal then
                  return (Label => Syntax_Trees.In_Parse_Actions.Ok);
               else
                  return (Syntax_Trees.In_Parse_Actions.Match_Names_Error, Start_Index, End_Index);
               end if;
            end if;
         end if;
      end;
   end Match_Names;

   function Propagate_Name
     (Tree       : in     Syntax_Trees.Tree;
      Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Syntax_Trees.In_Parse_Actions.Status
   is begin
      Tree.Set_Name (Nonterm, Tree.Name (Tokens (Name_Index)));
      return (Label => Syntax_Trees.In_Parse_Actions.Ok);
   end Propagate_Name;

   function Merge_Names
     (Tree        : in     Syntax_Trees.Tree;
      Nonterm     : in out Syntax_Trees.Recover_Token;
      Tokens      : in     Syntax_Trees.Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Syntax_Trees.In_Parse_Actions.Status
   is begin
      Tree.Set_Name (Nonterm, Tree.Name (Tokens (First_Index)) and Tree.Name (Tokens (Last_Index)));
      return (Label => Syntax_Trees.In_Parse_Actions.Ok);
   end Merge_Names;

   function Terminate_Partial_Parse
     (Tree                    : in Syntax_Trees.Tree;
      Partial_Parse_Active    : in Boolean;
      Partial_Parse_Byte_Goal : in Buffer_Pos;
      Recover_Active          : in Boolean;
      Nonterm                 : in Syntax_Trees.Recover_Token)
     return Syntax_Trees.In_Parse_Actions.Status
   is begin
      if Partial_Parse_Active and then
        (not Recover_Active) and then
        Tree.Byte_Region (Nonterm).Last >= Partial_Parse_Byte_Goal
      then
         raise WisiToken.Partial_Parse;
      else
         return (Label => Syntax_Trees.In_Parse_Actions.Ok);
      end if;
   end Terminate_Partial_Parse;

end WisiToken.In_Parse_Actions;
