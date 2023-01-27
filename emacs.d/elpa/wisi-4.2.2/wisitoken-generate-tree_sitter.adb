--  Abstract :
--
--  Translate a wisitoken grammar file to a tree-sitter grammar file.
--
--  References:
--
--  [1] tree-sitter grammar: https://tree-sitter.github.io/tree-sitter/creating-parsers#the-grammar-dsl
--
--  Copyright (C) 2020 - 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.BNF.Output_Ada_Common;
with WisiToken.Syntax_Trees.LR_Utils;
with WisiToken_Grammar_Editing;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
package body WisiToken.Generate.Tree_Sitter is
   use WisiToken.Syntax_Trees;

   procedure Eliminate_Empty_Productions
     (Data : in out WisiToken_Grammar_Runtime.User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree)
   is
      Ignore_Lines    : Boolean := False;

      type Empty_Nonterm is record
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Empty_Node : WisiToken.Syntax_Trees.Node_Access := WisiToken.Syntax_Trees.Invalid_Node_Access;
      end record;

      package Empty_Nonterm_Lists is new SAL.Gen_Unbounded_Definite_Vectors
        (Positive_Index_Type, Empty_Nonterm, Default_Element => (others => <>));

      Empty_Nonterms  : Empty_Nonterm_Lists.Vector;
      Nodes_To_Delete : Valid_Node_Access_Lists.List;
      Nodes_To_Check  : Valid_Node_Access_Lists.List;
      --  If we edit a node to now contain an optional item, it might become
      --  possibly empty.

      function Get_Text (Node : in Valid_Node_Access) return String
      is begin
         return WisiToken_Grammar_Runtime.Get_Text (Data, Tree, Node);
      end Get_Text;

      function Can_Be_Empty (Node : in Valid_Node_Access) return Node_Access
      --  Return a descendant node of Node that can be empty, Invalid_Node_Access if none.
      with Pre => To_Token_Enum (Tree.ID (Node)) in
                  rhs_list_ID | rhs_item_list_ID | rhs_alternative_list_ID | rhs_element_ID
      is
         use Syntax_Trees.LR_Utils;
      begin
         case To_Token_Enum (Tree.ID (Node)) is
         when rhs_list_ID =>
            declare
               RHS_List : constant Constant_List := Creators.Create_List (Tree, Node, +rhs_list_ID, +rhs_ID);
            begin
               for RHS of RHS_List loop
                  if Tree.RHS_Index (RHS) = 0 then
                     return RHS;
                  end if;
                  declare
                     Empty_Node : constant Node_Access := Can_Be_Empty (Tree.Child (RHS, 1));
                  begin
                     if Empty_Node /= Invalid_Node_Access then
                        return Empty_Node;
                     end if;
                  end;
               end loop;
               return Invalid_Node_Access;
            end;

         when rhs_item_list_ID =>
            declare
               Item_List : constant Constant_List := Creators.Create_List
                 (Tree, Node, +rhs_item_list_ID, +rhs_element_ID);
            begin
               for Element of Item_List loop
                  declare
                     Empty_Node : constant Node_Access := Can_Be_Empty (Element);
                  begin
                     if Empty_Node = Invalid_Node_Access then
                        --  This item can't be empty, so the list can't be empty.
                        return Invalid_Node_Access;
                     end if;
                  end;
               end loop;
               --  All items can be empty
               return Node;
            end;

         when rhs_element_ID =>
            declare
               Item : constant Valid_Node_Access := Tree.Find_Descendant (Node, +rhs_item_ID);
            begin
               case Tree.RHS_Index (Item) is
               when 0 | 1 =>
                  return Invalid_Node_Access;

               when 2 =>
                  --  If the only elements in an rhs_item_list are attributes, the list
                  --  is empty for LR generation purposes.
                  return Item;

               when 3 =>
                  return Item;

               when 4 =>
                  case Tree.RHS_Index (Tree.Child (Item, 1)) is
                  when 0 | 3 | 5 =>
                     return Item;
                  when 1 | 2 =>
                     return Can_Be_Empty (Tree.Child (Tree.Child (Item, 1), 2));
                  when 4 =>
                     return Invalid_Node_Access;
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

               when 5 =>
                  return Can_Be_Empty (Tree.Child (Tree.Child (Item, 1), 2));

               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when rhs_alternative_list_ID =>
            declare
               RHS_Alt_List : constant Constant_List := Creators.Create_List
                 (Tree, Node, +rhs_alternative_list_ID, +rhs_item_list_ID);
            begin
               for Item_List of RHS_Alt_List loop
                  declare
                     Empty_Node : constant Node_Access := Can_Be_Empty (Item_List);
                  begin
                     if Empty_Node /= Invalid_Node_Access then
                        return Empty_Node;
                     end if;
                  end;
               end loop;
               return Invalid_Node_Access;
            end;

         when others =>
            raise SAL.Programmer_Error;
         end case;
      end Can_Be_Empty;

      procedure Find_Empty_Nodes (Node : in Valid_Node_Access)
      is begin
         if Ignore_Lines then
            case To_Token_Enum (Tree.ID (Node)) is
            when declaration_ID =>
               --  Contained in a compilation_unit that is already marked for deletion
               case To_Token_Enum (Tree.ID (Tree.Child (Node, 2))) is
               when ELSIF_ID =>
                  --  | PERCENT ELSIF IDENTIFIER EQUAL IDENTIFIER
                  --  | PERCENT ELSIF IDENTIFIER IN IDENTIFIER_BAR_LIST
                  declare
                     use WisiToken.BNF;
                  begin
                     if "lexer" = Get_Text (Tree.Child (Node, 3)) then
                        Ignore_Lines := not WisiToken_Grammar_Runtime.Get_Lexer_Set
                          (Data, Tree, Tree.Child (Node, 5)) (Tree_Sitter_Lexer);

                     elsif "parser" = Get_Text (Tree.Child (Node, 3)) then
                        Ignore_Lines := not WisiToken_Grammar_Runtime.Get_Generate_Algorithm_Set
                          (Data, Tree, Tree.Child (Node, 5)) (WisiToken.BNF.Tree_Sitter);

                     else
                        raise SAL.Programmer_Error;
                     end if;

                     if Trace_Generate_EBNF > Outline then
                        Ada.Text_IO.Put_Line
                          ("ignore lines " & Ignore_Lines'Image & " line" &
                             Tree.Line_Region (Tree.Child (Node, 1), Trailing_Non_Grammar => True).First'Image);
                     end if;
                  end;

               when END_ID =>
                  --  | PERCENT END IF
                  Ignore_Lines := False;
                  if Trace_Generate_EBNF > Outline then
                     Ada.Text_IO.Put_Line
                       ("ignore lines false line" &
                          Tree.Line_Region (Tree.Child (Node, 1), Trailing_Non_Grammar => True).First'Image);
                  end if;

               when others =>
                  null;
               end case;

            when compilation_unit_ID =>
               Nodes_To_Delete.Append (Node);
               Find_Empty_Nodes (Tree.Child (Node, 1));

            when compilation_unit_list_ID =>
               declare
                  Children : constant Node_Access_Array := Tree.Children (Node);
               begin
                  case To_Token_Enum (Tree.ID (Children (1))) is
                  when compilation_unit_list_ID =>
                     Find_Empty_Nodes (Children (1));
                     Find_Empty_Nodes (Children (2));

                  when compilation_unit_ID =>
                     Find_Empty_Nodes (Children (1));

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end;

            when others =>
               --  FIXME tree-sitter: handle rhs_list %if, %end if
               null;
            end case;
            return;
         end if;

         case To_Token_Enum (Tree.ID (Node)) is
         --  SOI, EOI first, then Enum_Token_ID alphabetical order

         when Wisi_SOI_ID | Wisi_EOI_ID =>
            null;

         when compilation_unit_ID =>
            Find_Empty_Nodes (Tree.Child (Node, 1));

         when compilation_unit_list_ID =>
            declare
               Children : constant Node_Access_Array := Tree.Children (Node);
            begin
               case To_Token_Enum (Tree.ID (Children (1))) is
               when compilation_unit_list_ID =>
                  Find_Empty_Nodes (Children (1));
                  Find_Empty_Nodes (Children (2));
               when compilation_unit_ID =>
                  Find_Empty_Nodes (Children (1));
               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when declaration_ID =>
            case To_Token_Enum (Tree.ID (Tree.Child (Node, 2))) is
            when IF_ID | ELSIF_ID =>
               --  | PERCENT (IF | ELSIF) IDENTIFIER (EQUAL IDENTIFIER | IN IDENTIFIER_BAR_list)
               Nodes_To_Delete.Append (Node);
               declare
                  use WisiToken.BNF;
               begin
                  if "lexer" = Get_Text (Tree.Child (Node, 3)) then
                     Ignore_Lines := not WisiToken_Grammar_Runtime.Get_Lexer_Set
                       (Data, Tree, Tree.Child (Node, 5)) (Tree_Sitter_Lexer);

                  elsif "parser" = Get_Text (Tree.Child (Node, 3)) then
                     Ignore_Lines := not WisiToken_Grammar_Runtime.Get_Generate_Algorithm_Set
                       (Data, Tree, Tree.Child (Node, 5)) (WisiToken.BNF.Tree_Sitter);

                  else
                     raise SAL.Programmer_Error;
                  end if;

                  if Ignore_Lines and Trace_Generate_EBNF > Outline then
                     Ada.Text_IO.Put_Line
                       ("ignore lines true line" &
                          Tree.Line_Region (Tree.Child (Node, 1), Trailing_Non_Grammar => True).First'Image);
                  end if;

               end;

            when END_ID =>
               --  %end if
               Nodes_To_Delete.Append (Node);

            when others =>
               null;
            end case;

         when nonterminal_ID =>
            --  FIXME tree-sitter: handle %if in rhs_list; need test case

            --  tree-sitter allows the start nonterm of the grammar to be empty.
            --  For WisiToken, that's always wisitoken_accept_ID, which is not in
            --  the grammar file. So we ignore that case.

            declare
               Empty_Node : constant Node_Access := Can_Be_Empty (Tree.Child (Node, 3));
            begin
               if Empty_Node /= Invalid_Node_Access then
                  Empty_Nonterms.Append
                    ((+WisiToken_Grammar_Runtime.Get_Text (Data, Tree, Tree.Child (Node, 1)),
                      Empty_Node));
               end if;
            end;

         when wisitoken_accept_ID =>
            Find_Empty_Nodes (Tree.Child (Node, 2));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Find_Empty_Nodes;

      procedure Delete_Node (Node : in Valid_Node_Access)
      with Pre => To_Token_Enum (Tree.ID (Node)) in compilation_unit_ID | declaration_ID | rhs_list_ID | rhs_ID
      is
         use Syntax_Trees.LR_Utils;
      begin
         if Trace_Generate_EBNF > Detail then
            Ada.Text_IO.Put_Line
              ("delete " & Tree.Image (Node, Node_Numbers => True));
         end if;

         case To_Token_Enum (Tree.ID (Node)) is
         when compilation_unit_ID | declaration_ID =>
            declare
               Element : constant Valid_Node_Access :=
                 (if Tree.ID (Node) = +compilation_unit_ID then Node else Tree.Parent (Node));

               Container : List   := Creators.Create_From_Element
                 (Tree, Element, +compilation_unit_list_ID, +compilation_unit_ID, Separator_ID => Invalid_Token_ID);
               Cur       : Cursor := Container.To_Cursor (Element);
            begin
               Container.Delete (Cur);
            end;

         when rhs_list_ID =>
            --  %if in an rhs_list is not a canonical list element, so we can't
            --  use LR_Utils.Delete.
            raise SAL.Not_Implemented;

         when rhs_ID =>
            declare
               Container : List   := Creators.Create_From_Element
                 (Tree, Node, +rhs_list_ID, +rhs_ID, Separator_ID => Invalid_Token_ID);
               Cur       : Cursor := Container.To_Cursor (Node);
            begin
               Container.Delete (Cur);
            end;

         when others =>
            raise SAL.Programmer_Error;
         end case;
      end Delete_Node;

      procedure Make_Non_Empty (Empty_Node : in Valid_Node_Access)
      with Pre => To_Token_Enum (Tree.ID (Empty_Node)) in
                  rhs_ID | rhs_item_list_ID | rhs_item_ID
      is
         use WisiToken.Syntax_Trees.LR_Utils;
         use all type SAL.Base_Peek_Type;

         procedure Make_Non_Empty_RHS_Item (Item : in Valid_Node_Access)
         with Pre => Tree.ID (Item) = +rhs_item_ID
         is
            Item_Var : Valid_Node_Access := Item;
         begin
            case Tree.RHS_Index (Item) is
            when 0 | 1 | 2 =>
               raise SAL.Programmer_Error;

            when 3 => -- rhs_optional_item
               declare
                  Optional_Item :  Valid_Node_Access := Tree.Child (Item, 1);
               begin
                  case Tree.RHS_Index (Optional_Item) is
                  when 0         =>
                     Tree.Set_Children
                       (Node     => Optional_Item,
                        New_ID   => (+rhs_group_item_ID, 0),
                        Children =>
                          (1     => Tree.Add_Terminal (+LEFT_PAREN_ID),
                           2     => Tree.Child (Optional_Item, 2),
                           3     => Tree.Add_Terminal (+RIGHT_PAREN_ID)));

                     Tree.Set_Children
                       (Node     => Item_Var,
                        New_ID   => (+rhs_item_ID, 5),
                        Children =>
                          (1     => Optional_Item));

                  when 1         =>
                     Tree.Set_Children
                       (Node     => Optional_Item,
                        New_ID   => (+rhs_group_item_ID, 0),
                        Children => Tree.Children (Optional_Item) (1 .. 3));

                     Tree.Set_Children
                       (Node     => Item_Var,
                        New_ID   => (+rhs_item_ID, 5),
                        Children =>
                          (1     => Optional_Item));

                  when 2         =>
                     Tree.Set_Children
                       (Node     => Item_Var,
                        New_ID   => (+rhs_item_ID, 0),
                        Children => (1 => Tree.Child (Optional_Item, 1)));

                  when 3         =>
                     Tree.Set_Children
                       (Node     => Item_Var,
                        New_ID   => (+rhs_item_ID, 1),
                        Children => (1 => Tree.Child (Optional_Item, 1)));

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end;

            when 4 =>
               declare
                  Multiple_Item : Valid_Node_Access := Tree.Child (Item, 1);
               begin
                  case Tree.RHS_Index (Multiple_Item) is
                  when 0 | 3 | 5 =>
                     Tree.Set_Children
                       (Multiple_Item,
                        (+rhs_multiple_item_ID,
                         (case Tree.RHS_Index (Multiple_Item) is
                          when 0 => 1,
                          when 3 => 2,
                          when 5 => 4,
                          when others => raise SAL.Programmer_Error)),
                        (case Tree.RHS_Index (Multiple_Item) is
                         when 0 | 3 =>
                           (1 => Tree.Child (Multiple_Item, 1),
                            2 => Tree.Child (Multiple_Item, 2),
                            3 => Tree.Child (Multiple_Item, 3),
                            4 => (case Tree.RHS_Index (Multiple_Item) is
                                  when 0 => Tree.Add_Terminal (+MINUS_ID),
                                  when 3 => Tree.Add_Terminal (+PLUS_ID),
                                  when others => raise SAL.Programmer_Error)),
                         when 5 => (1 => Tree.Child (Multiple_Item, 1)),
                         when others => raise SAL.Programmer_Error));

                  when others =>
                     raise SAL.Programmer_Error with "make_non_empty_rhs_item " & Tree.Image
                       (Multiple_Item, RHS_Index => True, Node_Numbers => True);
                  end case;
               end;

            when others =>
               raise SAL.Programmer_Error;
            end case;
         end Make_Non_Empty_RHS_Item;

      begin
         case To_Token_Enum (Tree.ID (Empty_Node)) is
         when rhs_item_ID =>
            Make_Non_Empty_RHS_Item (Empty_Node);

         when rhs_item_list_ID =>
            --  Entire item_list can be empty
            declare
               Item_List : constant Constant_List := Creators.Create_List
                 (Tree, Empty_Node, +rhs_item_list_ID, +rhs_element_ID);
            begin
               --  If there is more than one item in the rhs_item_list, we can
               --  arbitrarily make the first non-empty. See ada_lite_ebnf.wy
               --  handled_sequence_of_statements.
               Make_Non_Empty_RHS_Item (Tree.Find_Descendant (Element (Item_List.First), +rhs_item_ID));
            end;

         when rhs_ID =>
            Delete_Node (Empty_Node);

         when others =>
            raise SAL.Programmer_Error;
         end case;
      end Make_Non_Empty;

      procedure Make_Optional (Name : in String)
      is
         procedure Find_Nodes (Node : in Valid_Node_Access)
         is
            use all type SAL.Base_Peek_Type;
            Node_Var : Node_Access := Node;
         begin
            case To_Token_Enum (Tree.ID (Node)) is
            --  common code first, then Enum_Token_ID alphabetical order

            when wisitoken_accept_ID =>
               Find_Nodes (Tree.Child (Node, 2));

            when compilation_unit_ID =>
               Find_Nodes (Tree.Child (Node, 1));

            when compilation_unit_list_ID | rhs_alternative_list_ID | rhs_item_list_ID | rhs_list_ID =>
               declare
                  Children : constant Node_Access_Array := Tree.Children (Node);
               begin
                  case Tree.RHS_Index (Node) is
                  when 0 =>
                     Find_Nodes (Children (1));

                  when 1 =>
                     Find_Nodes (Children (1));
                     Find_Nodes
                       (Children
                          ((if To_Token_Enum (Tree.ID (Node)) in rhs_list_ID | rhs_alternative_list_ID then 3 else 2)));

                  when others =>
                     --  rhs_list can have other rhs_index, but those nodes should have been
                     --  deleted by now.
                     raise SAL.Programmer_Error with "Make_Optional.Find_Nodes list: rhs_index" &
                       Tree.RHS_Index (Node)'Image & " node " & Tree.Image
                         (Node, Node_Numbers => True);
                  end case;
               end;

            when declaration_ID =>
               return;

            when nonterminal_ID =>
               if Name = Get_Text (Tree.Child (Node, 1)) then
                  return;
               end if;

               Find_Nodes (Tree.Child (Node, 3));

            when rhs_element_ID =>
               Find_Nodes (Tree.Child (Node, (if Tree.RHS_Index (Node) = 0 then 1 else 3)));

            when rhs_group_item_ID =>
               Find_Nodes (Tree.Child (Node, 2));

            when rhs_item_ID =>
               case Tree.RHS_Index (Node) is
               when 0 | 1 =>
                  if Name = Get_Text (Tree.Child (Node, 1)) then
                     Nodes_To_Check.Append (Node);
                     declare
                        Child : constant Valid_Node_Access := WisiToken_Grammar_Editing.Add_RHS_Optional_Item
                          (Tree,
                           RHS_Index => (if Tree.RHS_Index (Node) = 0 then 2 else 3),
                           Content   => Tree.Child (Node, 1));
                     begin
                        Tree.Set_Children (Node_Var, (+rhs_item_ID, 3), (1 => Child));
                     end;
                  end if;

               when 2 =>
                  null;

               when 3 | 4 | 5 =>
                  Find_Nodes (Tree.Child (Node, 1));

               when others =>
                  raise SAL.Programmer_Error;
               end case;

            when rhs_multiple_item_ID =>
               case Tree.RHS_Index (Node) is
               when 0 | 1 | 2 | 3 =>
                  Find_Nodes (Tree.Child (Node, 2));

               when 4 =>
                  Nodes_To_Check.Append (Node);

                  Tree.Set_Children
                    (Node_Var, (+rhs_multiple_item_ID, 5), (Tree.Child (Node, 1), Tree.Add_Terminal (+STAR_ID)));

               when 5 =>
                  --  already optional
                  null;
               when others =>
                  raise SAL.Programmer_Error;
               end case;

            when rhs_optional_item_ID =>
               case Tree.RHS_Index (Node) is
               when 0 | 1 =>
                  Find_Nodes (Tree.Child (Node, 2));

               when 2 | 3 =>
                  --  already optional
                  null;
               when others =>
                  raise SAL.Programmer_Error;
               end case;

            when rhs_ID =>
               if Tree.Child_Count (Node) = 0 then
                  return;
               else
                  Find_Nodes (Tree.Child (Node, 1));
               end if;

            when others =>
               raise SAL.Programmer_Error with "Make_Optional.Find_Nodes name " & Name & " in node " & Tree.Image
                 (Node, Node_Numbers => True);
            end case;
         end Find_Nodes;

      begin
         Find_Nodes (Tree.Root);
      end Make_Optional;

   begin
      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("tree_sitter eliminate empty productions start");
         if Trace_Generate_EBNF > Detail then
            Tree.Print_Tree (Tree.Root);
         end if;
      end if;

      Find_Empty_Nodes (Tree.Root);
      --  Also finds %if etc, adds them to Nodes_To_Delete.

      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("nodes to delete:" & Nodes_To_Delete.Length'Image);
      end if;

      for Node of Nodes_To_Delete loop
         Delete_Node (Node);
      end loop;

      Data.Error_Reported.Clear;

      Tree.Validate_Tree
        (Data, Data.Error_Reported,
         Root             => Tree.Root,
         Validate_Node    => WisiToken_Grammar_Editing.Validate_Node'Access,
         Node_Index_Order => True);

      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("empty nonterms:");
         for Nonterm of Empty_Nonterms loop
            Ada.Text_IO.Put (-Nonterm.Name & ", ");
         end loop;
         Ada.Text_IO.New_Line (2);
      end if;

      for Nonterm of Empty_Nonterms loop
         Make_Non_Empty (Nonterm.Empty_Node);
      end loop;

      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("after Make_Non_Empty");
         Ada.Text_IO.New_Line;
      end if;

      Tree.Validate_Tree
        (Data, Data.Error_Reported,
         Root             => Tree.Root,
         Validate_Node    => WisiToken_Grammar_Editing.Validate_Node'Access,
         Node_Index_Order => False);

      for Nonterm of Empty_Nonterms loop
         Make_Optional (-Nonterm.Name);
      end loop;

      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("after Make_Optional");
         Ada.Text_IO.New_Line;
      end if;

      Tree.Validate_Tree
        (Data, Data.Error_Reported,
         Root             => Tree.Root,
         Validate_Node    => WisiToken_Grammar_Editing.Validate_Node'Access,
         Node_Index_Order => False);

      declare
         use Valid_Node_Access_Lists;
         Cur  : Cursor := Nodes_To_Check.First;
         Temp : Cursor;
      begin
         loop
            exit when not Has_Element (Cur);
            declare
               RHS_List_Node : constant Valid_Node_Access := Tree.Find_Ancestor (Element (Cur), +rhs_list_ID);
               Nonterm_Node  : constant Valid_Node_Access := Tree.Parent (RHS_List_Node);

               Empty_Node : constant Node_Access := Can_Be_Empty (RHS_List_Node);
            begin
               if Empty_Node /= Invalid_Node_Access then
                  declare
                     Nonterm_Name : constant String := Get_Text (Tree.Child (Nonterm_Node, 1));
                  begin
                     if Trace_Generate_EBNF > Outline then
                        Ada.Text_IO.Put_Line ("newly empty nonterm " & Nonterm_Name);
                     end if;

                     Make_Non_Empty (Empty_Node);
                     Make_Optional (Nonterm_Name);
                  end;
               end if;
            end;
            Temp := Cur;
            Cur  := Next (Cur);
            Nodes_To_Check.Delete (Temp);
         end loop;
      end;

      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("after Nodes_To_Check");
         Ada.Text_IO.New_Line;
      end if;

      Tree.Validate_Tree
        (Data, Data.Error_Reported,
         Root             => Tree.Root,
         Validate_Node    => WisiToken_Grammar_Editing.Validate_Node'Access,
         Node_Index_Order => False);

      if Trace_Generate_EBNF > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("tree_sitter eliminate empty productions end");
         Tree.Print_Tree (Tree.Root);
      end if;
   end Eliminate_Empty_Productions;

   procedure Print_Tree_Sitter
     (Data             : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree             : in out Syntax_Trees.Tree;
      Lexer            : in     WisiToken.Lexer.Handle;
      Output_File_Name : in     String;
      Language_Name    : in     String)
   is
      use all type Ada.Containers.Count_Type;

      File : File_Type;

      Extras    : WisiToken.BNF.String_Lists.List;
      Conflicts : WisiToken.BNF.String_Lists.List;

      Start_Node : Node_Access := Invalid_Node_Access;

      --  Local specs

      procedure Put_RHS_Item_List (Node : in Valid_Node_Access; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_item_list_ID;

      --  Local bodies

      function Get_Text (Tree_Index : in Valid_Node_Access) return String
      is
         function Strip_Delimiters (Tree_Index : in Valid_Node_Access) return String
         is
            Region : Buffer_Region renames Tree.Byte_Region (Tree_Index, Trailing_Non_Grammar => False);
         begin
            if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
               --  Strip delimiters. We don't strip leading/trailing spaces to preserve indent.
               return Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

               --  We don't strip string delimiters; tree-setter can use the same ones.
            else
               return Lexer.Buffer_Text (Region);
            end if;
         end Strip_Delimiters;

      begin
         case Tree.Label (Tree_Index) is
         when Source_Terminal =>
            return Strip_Delimiters (Tree_Index);

         when Virtual_Terminal =>
            --  Terminal keyword inserted during tree edit. We could check for
            --  Identifier, but that will be caught later.
            return Image (Tree.ID (Tree_Index), Wisitoken_Grammar_Actions.Descriptor);

         when Virtual_Identifier =>
            raise SAL.Programmer_Error;

         when Nonterm =>
            declare
               use all type Ada.Strings.Unbounded.Unbounded_String;
               Result       : Ada.Strings.Unbounded.Unbounded_String;
               Tree_Indices : constant Valid_Node_Access_Array := Tree.Get_Terminals (Tree_Index);
               Need_Space   : Boolean                                      := False;
            begin
               for Tree_Index of Tree_Indices loop
                  Result := Result & (if Need_Space then " " else "") &
                    Get_Text (Tree_Index);
                  Need_Space := True;
               end loop;
               return -Result;
            end;
         end case;
      end Get_Text;

      procedure Not_Translated (Label : in String; Node : in Valid_Node_Access)
      is begin
         New_Line (File);
         Put (File, "// " & Label & ": not translated: " & Node_Access'Image (Node) & ":" &
                Tree.Image (Node, Children => True));

         Put_Line
           (Current_Error,
            Tree.Error_Message
              (Node,
               "not translated: " &
                 Tree.Image
                   (Node,
                    RHS_Index    => True,
                    Children     => True,
                    Node_Numbers => True)));
      end Not_Translated;

      procedure Put_RHS_Alternative_List (Node : in Valid_Node_Access; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_alternative_list_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            --  If only alternative, don't need "choice()".
            Put_RHS_Item_List (Tree.Child (Node, 1), First => True);

         when 1 =>
            if First then
               Put (File, "choice(");
            end if;

            Put_RHS_Alternative_List (Tree.Child (Node, 1), First => False);
            Put (File, ", ");
            Put_RHS_Item_List (Tree.Child (Node, 3), First => True);

            if First then
               Put (File, ")");
            end if;

         when others =>
            Not_Translated ("Put_RHS_Alternative_List", Node);
         end case;
      end Put_RHS_Alternative_List;

      procedure Put_RHS_Optional_Item (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_optional_item_ID
      is begin
         Put (File, "optional(");

         case Tree.RHS_Index (Node) is
         when 0 | 1 =>
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
         when 2 =>
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
         when 3 =>
            --  STRING_LITERAL_2
            Put (File, Get_Text (Tree.Child (Node, 1)));
         when others =>
            Not_Translated ("Put_RHS_Optional_Item", Node);
         end case;

         Put (File, ")");
      end Put_RHS_Optional_Item;

      procedure Put_RHS_Multiple_Item (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_multiple_item_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 | 3 =>
            Put (File, "repeat(");
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
            Put (File, ")");

         when 1 | 2 =>
            Put (File, "repeat1(");
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
            Put (File, ")");

         when 4 =>
            Put (File, "repeat1(");
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
            Put (File, ")");

         when 5 =>
            Put (File, "repeat(");
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
            Put (File, ")");

         when others =>
            Not_Translated ("Put_RHS_Multiple_Item", Node);
         end case;
      end Put_RHS_Multiple_Item;

      procedure Put_RHS_Group_Item (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_group_item_ID
      is begin
         Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
      end Put_RHS_Group_Item;

      procedure Put_RHS_Item (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_item_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            declare
               Ident : constant String     := Get_Text (Node);
               Decl  : constant Node_Access := WisiToken_Grammar_Editing.Find_Declaration (Data, Tree, Ident);
            begin
               if Decl = Invalid_Node_Access then
                  Generate.Put_Error (Tree.Error_Message (Node, "decl for '" & Ident & "' not found"));

               elsif Tree.ID (Decl) = +nonterminal_ID then
                  Put (File, "$." & Get_Text (Tree.Child (Decl, 1)));

               else
                  case Tree.RHS_Index (Decl) is
                  when 0 =>
                     case To_Token_Enum (Tree.ID (Tree.Child (Tree.Child (Decl, 2), 1))) is
                     when KEYWORD_ID =>
                        Put (File, Get_Text (Tree.Child (Decl, 4)));

                     when NON_GRAMMAR_ID =>
                        Not_Translated ("put_rhs_item", Node);

                     when Wisitoken_Grammar_Actions.TOKEN_ID =>
                        declare
                           use WisiToken.Syntax_Trees.LR_Utils;
                           List : constant Constant_List := Creators.Create_List
                             (Tree, Tree.Child (Decl, 4), +declaration_item_list_ID, +declaration_item_ID);
                           Item : constant Valid_Node_Access := Tree.Child (Element (List.First), 1);
                        begin
                           case To_Token_Enum (Tree.ID (Item)) is
                           when REGEXP_ID =>
                              Put (File, "$." & Ident);

                           when STRING_LITERAL_1_ID | STRING_LITERAL_2_ID =>
                              --  FIXME tree-sitter: STRING_LITERAL_1_ID in regexp is case insensitive; not
                              --  clear how to do that in tree-sitter.
                              Put (File, Get_Text (Item));

                           when others =>
                              Not_Translated ("put_rhs_item ident token", Node);
                           end case;
                        end;

                     when others =>
                        Not_Translated ("put_rhs_item ident", Node);
                     end case;

                  when others =>
                     Not_Translated ("put_rhs_item 0", Node);
                  end case;
               end if;
            end;

         when 1 =>
            --  STRING_LITERAL_2
            Put (File, Get_Text (Node));

         when 2 =>
            --  ignore attribute
            null;

         when 3 =>
            Put_RHS_Optional_Item (Tree.Child (Node, 1));

         when 4 =>
            Put_RHS_Multiple_Item (Tree.Child (Node, 1));

         when 5 =>
            Put_RHS_Group_Item (Tree.Child (Node, 1));

         when others =>
            Not_Translated ("Put_RHS_Item", Node);
         end case;
      end Put_RHS_Item;

      procedure Put_RHS_Element (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_element_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS_Item (Tree.Child (Node, 1));

         when 1 =>
            --  Ignore the label
            Put_RHS_Item (Tree.Child (Node, 3));

         when others =>
            Not_Translated ("Put_RHS_Element", Node);
         end case;
      end Put_RHS_Element;

      procedure Put_RHS_Item_List (Node : in Valid_Node_Access; First : in Boolean)
      is
         Children : constant Node_Access_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_RHS_Element (Children (1));
         else
            if First then
               Put (File, "seq(");
            end if;
            Put_RHS_Item_List (Children (1), First => False);
            Put (File, ", ");
            Put_RHS_Element (Children (2));

            if First then
               Put (File, ")");
            end if;
         end if;
      end Put_RHS_Item_List;

      procedure Put_RHS (Node : in Valid_Node_Access)
      with Pre => Tree.ID (Node) = +rhs_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Generate.Put_Error
              (Tree.Error_Message
                 ((declare
                      RHS_List : constant Valid_Node_Access := Tree.Parent (Node);
                   begin
                      (case Tree.RHS_Index (RHS_List) is
                       when 0 => RHS_List,
                       when others => Tree.Child (RHS_List, 2))),
                  "empty RHS forbidden by tree-sitter"));

         when 1 .. 3 =>
            Put_RHS_Item_List (Tree.Child (Node, 1), First => True);
            --  tree-sitter does not have actions in the grammar

         when others =>
            Not_Translated ("put_rhs", Node);
         end case;
      end Put_RHS;

      procedure Put_RHS_List (Node : in Valid_Node_Access; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_list_ID
      is
         Children : constant Node_Access_Array := Tree.Children (Node);
      begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS (Children (1));

         when 1 =>
            if First then
               Put (File, "choice(");
            end if;

            Put_RHS_List (Children (1), First => False);
            Put (File, ",");
            Put_RHS (Children (3));

            if First then
               Put (File, ")");
            end if;

         when 2 .. 4 =>
            --  Should have been eliminated by Eliminate_Empty_Productions
            raise SAL.Programmer_Error with "Print_Tree_Sitter rhs_list %if " &
              Tree.Image (Node, Node_Numbers => True);

         when others =>
               raise SAL.Programmer_Error;
         end case;
      end Put_RHS_List;

      procedure Process_Node (Node : in Valid_Node_Access)
      is begin
         if Node = Start_Node then
            return;
         end if;

         case To_Token_Enum (Tree.ID (Node)) is
         --  SOI, EOI first, then Enum_Token_ID alphabetical order

         when Wisi_SOI_ID | Wisi_EOI_ID =>
            null;

         when compilation_unit_ID =>
            Process_Node (Tree.Child (Node, 1));

         when compilation_unit_list_ID =>
            declare
               Children : constant Node_Access_Array := Tree.Children (Node);
            begin
               case To_Token_Enum (Tree.ID (Children (1))) is
               when compilation_unit_list_ID =>
                  Process_Node (Children (1));
                  Process_Node (Children (2));
               when compilation_unit_ID =>
                  Process_Node (Children (1));
               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when declaration_ID =>
            raise SAL.Not_Implemented with "FIXME: match current wisitoken_grammar.wy";
            --  case To_Token_Enum (Tree.ID (Tree.Child (Node, 2))) is
            --  when Wisitoken_Grammar_Actions.TOKEN_ID | NON_GRAMMAR_ID =>
            --     --  We need tokens with 'regexp' values because they are not defined
            --     --  elsewhere, 'punctuation' tokens for consistent names, and
            --     --  'line-comment' to allow comments. tree-sitter default 'extras'
            --     --  handles whitespace and newline, but if we define 'comment', we
            --     --  also need 'new-line' and 'whitespace'.
            --     declare
            --        use Ada.Strings;
            --        use Ada.Strings.Fixed;
            --        use WisiToken.Syntax_Trees.LR_Utils;
            --        Name  : constant String        := Get_Text (Tree.Child (Node, 3));
            --        Class : constant Token_Enum_ID := To_Token_Enum (Tree.ID (Tree.Child (Tree.Child (Node, 2), 1)));
            --        Kind  : constant String        :=
            --          (if Class in NON_GRAMMAR_ID | Wisitoken_Grammar_Actions.TOKEN_ID
            --           then Get_Text (Tree.Child (Tree.Child (Node, 2), 3))
            --           else "keyword");
            --        List  : constant Constant_List    := Creators.Create_List
            --          (Tree, Tree.Child (Node, 4), +declaration_item_list_ID, +declaration_item_ID);
            --        Value : constant Valid_Node_Access := Tree.Child (Element (List.First), 1);
            --        --  We are ignoring any repair image
            --     begin
            --        if Class = NON_GRAMMAR_ID then
            --           if Kind = "line-comment" then
            --              --  WORKAROUND: tree-sitter 0.16.6 treats rule "token(seq('--',
            --              --  /.*/))" correctly for an Ada comment, but not extra "/--.*/". See
            --              --  github tree-sitter issue 651 - closed without resolving this
            --              --  question, but it does provide a workaround.
            --              Put_Line (File, Name & ": $ => token(seq(" & Get_Text (Value) & ", /.*/)),");
            --              Extras.Append ("$." & Name);
            --           else
            --              Extras.Append ("/" & Trim (Get_Text (Value), Both) & "/");
            --           end if;

            --        elsif Kind = "punctuation" then
            --           Put_Line (File, Name & ": $ => " & Get_Text (Value) & ",");

            --        elsif To_Token_Enum (Tree.ID (Value)) = REGEXP_ID then
            --           Put_Line (File, Name & ": $ => /" & Trim (Get_Text (Value), Both) & "/,");

            --        end if;
            --     end;

            --  when 1 =>
            --     --  new-line with no regexp; tree-sitter defaults to DOS, Unix newline.
            --     null;

            --  when 2 =>
            --     --  FIXME tree-sitter: CODE copyright_license
            --     null;

            --  when 3 =>
            --     declare
            --        Kind : constant String := Get_Text (Tree.Child (Node, 2));
            --     begin
            --        --  FIXME tree-sitter: lexer_regexp
            --        if Kind = "conflict" then
            --           --  .wy LR format:
            --           --  %conflict action LHS [| action LHS]* 'on token' on
            --           --            I      I+1
            --           --
            --           --  .wy Tree_Sitter format:
            --           --  %conflict LHS (LHS)*
            --           --
            --           --  .js format:
            --           --  [$.LHS, $.LHS, ...]

            --           declare
            --              use Ada.Strings.Unbounded;

            --            Tree_Indices : constant Valid_Node_Access_Array := Tree.Get_Terminals (Tree.Child (Node, 3));
            --              Result       : Unbounded_String                := +"[";
            --           begin
            --              if Tree_Indices'Length < 3 or else Tree.ID (Tree_Indices (3)) /= +BAR_ID then
            --                 --  Tree_Sitter format
            --                 for LHS of Tree_Indices loop
            --                    Result := @ & "$." & Get_Text (LHS) & ", ";
            --                 end loop;

            --              else
            --                 --  LR format
            --                 declare
            --                    use all type SAL.Base_Peek_Type;
            --                    I : SAL.Peek_Type := Tree_Indices'First;
            --                 begin
            --                    loop
            --                       Result := @ & "$." & Get_Text (Tree_Indices (I + 1)) & ", ";

            --                       I := I + 2;
            --                       exit when Tree.ID (Tree_Indices (I)) /= +BAR_ID;
            --                       I := I + 1;
            --                    end loop;
            --                 end;
            --              end if;
            --              Conflicts.Append (-Result & ']');
            --           end;
            --        end if;
            --     end;

            --  when 4 =>
            --     --  %case_insensitive
            --     null;

            --  when 5 .. 9 =>
            --     --  Should have been eliminated by Eliminate_Empty_Productions
            --     raise SAL.Programmer_Error with "Print_Tree_Sitter declaration %if " &
            --       Tree.Image (Node, Node_Numbers => True);

            --  when others =>
            --     raise SAL.Programmer_Error;
            --  end case;

         when nonterminal_ID =>
            declare
               Children : constant Node_Access_Array := Tree.Children (Node);
            begin
               Put (File, Get_Text (Children (1)) & ": $ => ");

               Put_RHS_List (Children (3), First => True);

               Put_Line (File, ",");
            end;

         when wisitoken_accept_ID =>
            --  Child 1 is SOI, 2 compilation_unit_list
            Process_Node (Tree.Child (Node, 2));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Process_Node;
   begin
      if Trace_Generate_EBNF > Outline then
         Ada.Text_IO.Put_Line ("translate to tree_sitter");
      end if;

      Create (File, Out_File, Output_File_Name);
      Put_Line (File, "// generated from " & Tree.Lexer.File_Name & " -*- buffer-read-only:t -*-");

      Put_Line (File, "module.exports = grammar({");
      Put_Line (File, "  name: '" & Language_Name & "',");

      Put_Line (File, "  rules: {");

      --  Start symbol must be the first rule; that's how tree-sitter knows
      --  it's the start symbol. accept rule with wisi-eoi is implicit in
      --  tree-sitter (as in .wy).
      if -Data.Language_Params.Start_Token = "" then
         Generate.Put_Error (Generate.Error_Message (Tree.Lexer.File_Name, 1, "%start not specified"));
      else
         declare
            Temp : constant Node_Access := WisiToken_Grammar_Editing.Find_Declaration
              (Data, Tree, -Data.Language_Params.Start_Token);
         begin
            Process_Node (Temp);
            Start_Node := Temp;
         end;
      end if;

      Process_Node (Tree.Root);
      Put (File, "  }");

      if Conflicts.Length > 0 then
         Put_Line (File, ",");
         Put_Line (File, "  conflicts: $ => [");
         for Item of Conflicts loop
            Put_Line (File, "    " & Item & ",");
         end loop;
         Put (File, "  ]");
      end if;

      if Extras.Length > 0 then
         Put_Line (File, ",");
         Put_Line (File, "  extras: $ => [");
         for Item of Extras loop
            Put_Line (File, "    " & Item & ",");
         end loop;
         Put_Line (File, "  ],");
      end if;
      Put (File, "  }");

      Put_Line (File, ");");
      Close (File);
   end Print_Tree_Sitter;

   procedure Create_Test_Main (Output_File_Name_Root : in String)
   is
      use WisiToken.BNF;

      Ada_Name_Root : constant String := Output_Ada_Common.File_Name_To_Ada (Output_File_Name_Root);
      Unit_Name     : constant String := Ada_Name_Root & "_Tree_Sitter_Run";

      File_Name : constant String := To_Lower (Unit_Name) & ".adb";

      File : File_Type;

   begin
      Create (File, Out_File, File_Name);
      Set_Output (File);

      Put_File_Header (Ada_Comment);
      --  no Copyright_License; just a test file
      New_Line;

      Put_Line ("with Interfaces.C.Extensions;");
      Put_Line ("with Gen_Tree_Sitter_Parser_Run;");
      Put_Line ("procedure " & Unit_Name);
      Put_Line ("is");
      Put_Line ("   function Tree_Sitter_" & Ada_Name_Root & " return Interfaces.C.Extensions.void_ptr");
      Put_Line ("   with Import     => True,");
      Put_Line ("     External_Name => ""tree_sitter_" & Ada_Name_Root & """,");
      Put_Line ("     Convention    => C;");
      Put_Line ("   procedure Parse_Run is new Gen_Tree_Sitter_Parser_Run");
      Put_Line ("     (Tree_Sitter_Language => Tree_Sitter_" & Ada_Name_Root & ");");
      Put_Line ("begin");
      Put_Line ("   Parse_Run;");
      Put_Line ("end " & Unit_Name & ";");
      Close (File);
      Set_Output (Standard_Output);
   end Create_Test_Main;

end WisiToken.Generate.Tree_Sitter;
--  Local Variables:
--  ada-case-strict: nil
--  End:
