--  Abstract :
--
--  Utilities for editing wisitoken grammars.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with WisiToken.Syntax_Trees.LR_Utils;
with WisiToken.Syntax_Trees;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
package WisiToken_Grammar_Editing is
   use all type WisiToken.Production_ID;
   use all type WisiToken.Token_ID;
   use all type WisiToken.Base_Identifier_Index;
   use all type Wisitoken_Grammar_Actions.Token_Enum_ID;
   use all type WisiToken.Syntax_Trees.Node_Label;
   use all type WisiToken.Syntax_Trees.Node_Access;

   type Identifier_Token
     (Label : WisiToken.Syntax_Trees.Terminal_Label := WisiToken.Syntax_Trees.Terminal_Label'First)
   is record
      --  Either a syntax tree terminal node, or data to create a syntax
      --  tree Virtual_Identifier node. Used to represent an identifier in a
      --  grammar production.
      case Label is
      when Source_Terminal | Virtual_Terminal =>
         Node : WisiToken.Syntax_Trees.Node_Access;

      when Virtual_Identifier =>
         ID         : WisiToken.Token_ID;
         Identifier : WisiToken.Identifier_Index;
      end case;
   end record;

   function Image (Item : in Identifier_Token; Tree : in WisiToken.Syntax_Trees.Tree) return String
   is ((case Item.Label is
        when Source_Terminal | Virtual_Terminal => WisiToken.Syntax_Trees.Trimmed_Image
          (Tree.Get_Node_Index (Item.Node)) & ":",
        when Virtual_Identifier => Trimmed_Image (Item.Identifier) & ";") &
         Image (Item.ID, Wisitoken_Grammar_Actions.Descriptor));

   Invalid_Identifier_Token : constant Identifier_Token :=
     (Label => WisiToken.Syntax_Trees.Virtual_Terminal,
      Node  => WisiToken.Syntax_Trees.Invalid_Node_Access);

   function ID (Tree : in WisiToken.Syntax_Trees.Tree; Item : in Identifier_Token) return WisiToken.Token_ID
   is (case Item.Label is
       when Source_Terminal | Virtual_Terminal => Tree.ID (Item.Node),
       when Virtual_Identifier => Item.ID);

   function To_Identifier_Token
     (Item : in WisiToken.Identifier_Index)
     return Identifier_Token
   is ((Virtual_Identifier, +IDENTIFIER_ID, Item));

   function To_Identifier_Token
     (Item : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Tree : in WisiToken.Syntax_Trees.Tree)
     return Identifier_Token
   with Pre => To_Token_Enum (Tree.ID (Item)) in rhs_element_ID | rhs_item_ID | IDENTIFIER_ID;

   function Add_RHS_Group_Item
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      RHS_Index : in     Natural;
      Content   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.Syntax_Trees.Valid_Node_Access
   with Pre => Tree.ID (Content) = +rhs_alternative_list_ID,
     Post => Tree.ID (Add_RHS_Group_Item'Result) = +rhs_group_item_ID;

   function Add_RHS_Optional_Item
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      RHS_Index : in     Natural;
      Content   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.Syntax_Trees.Valid_Node_Access
   with Pre => To_Token_Enum (Tree.ID (Content)) in rhs_alternative_list_ID | IDENTIFIER_ID | STRING_LITERAL_2_ID and
               RHS_Index <= 3,
     Post => Tree.ID (Add_RHS_Optional_Item'Result) = +rhs_optional_item_ID;

   function Add_Identifier_Token
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     Identifier_Token)
     return WisiToken.Syntax_Trees.Valid_Node_Access;

   function Add_RHS_Item
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.Syntax_Trees.Valid_Node_Access
   with Pre => To_Token_Enum (Tree.ID (Item)) in IDENTIFIER_ID | STRING_LITERAL_2_ID,
     Post => Tree.ID (Add_RHS_Item'Result) = +rhs_item_ID;

   function Add_RHS_Element
     (Tree  : in out WisiToken.Syntax_Trees.Tree;
      Item  : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Label : in     Identifier_Token := Invalid_Identifier_Token)
     return WisiToken.Syntax_Trees.Valid_Node_Access
   with Pre => Tree.ID (Item) = +rhs_item_ID,
     Post => Tree.Production_ID (Add_RHS_Element'Result) =
             (+rhs_element_ID, (if Label = Invalid_Identifier_Token then 0 else 1));

   function Empty_RHS_Item_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Empty_RHS_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Add_RHS
     (Tree              : in out WisiToken.Syntax_Trees.Tree;
      Item              : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Auto_Token_Labels : in     Boolean;
      Edited_Token_List : in     Boolean;
      Post_Parse_Action : in     WisiToken.Syntax_Trees.Node_Access := WisiToken.Syntax_Trees.Invalid_Node_Access;
      In_Parse_Action   : in     WisiToken.Syntax_Trees.Node_Access := WisiToken.Syntax_Trees.Invalid_Node_Access)
     return WisiToken.Syntax_Trees.Valid_Node_Access
   with Pre => Tree.ID (Item) = +rhs_item_list_ID and
               (Post_Parse_Action = WisiToken.Syntax_Trees.Invalid_Node_Access or else
                Tree.ID (Post_Parse_Action) = +ACTION_ID) and
               (In_Parse_Action = WisiToken.Syntax_Trees.Invalid_Node_Access or else
                Tree.ID (In_Parse_Action) = +ACTION_ID),
     Post => Tree.ID (Add_RHS'Result) = +rhs_ID;

   function Find_Declaration
     (Data  : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree  : in out WisiToken.Syntax_Trees.Tree;
      Name  : in     String)
     return WisiToken.Syntax_Trees.Node_Access
   with Post => Find_Declaration'Result = WisiToken.Syntax_Trees.Invalid_Node_Access or else
                To_Token_Enum (Tree.ID (Find_Declaration'Result)) in declaration_ID | nonterminal_ID;
   --  Return the node that declares Name, Invalid_Node_Access if none.

   procedure Validate_Node
     (Tree                : in     WisiToken.Syntax_Trees.Tree;
      Node                : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      User_Data           : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Node_Error_Reported : in out Boolean);
   --  Verify that all nodes match wisitoken_grammar.wy. Data must be of
   --  type WisiToken_Grammar_Runtime.User_Data_Type. Uses
   --  Data.EBNF_Allowed.
   --
   --  For use with Syntax_Trees.Validate_Tree.

   procedure Translate_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out WisiToken_Grammar_Runtime.User_Data_Type)
   with Pre => Tree.Editable;
   --  Edit EBNF nonterms, adding new nonterms as needed, resulting in
   --  a BNF tree.
   --
   --  Tree.Root is updated as necessary; all streams must be cleared, or
   --  they might be corrupted.
   --
   --  Generator.LR.*_Generate requires a BNF grammar.

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in WisiToken_Grammar_Runtime.User_Data_Type);
   --  Print the wisitoken grammar source represented by Tree, Terminals
   --  to a new file File_Name.

end WisiToken_Grammar_Editing;
