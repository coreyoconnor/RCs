--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c PROCESS wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with WisiToken.Syntax_Trees;
package Wisitoken_Grammar_Actions is

   Descriptor : aliased constant WisiToken.Descriptor :=
     (First_Terminal    => 3,
      Last_Terminal       => 42,
      First_Nonterminal   => 43,
      Last_Nonterminal    => 66,
      SOI_ID              => 67,
      EOI_ID              => 42,
      Accept_ID           => 43,
      Case_Insensitive    => False,
      New_Line_ID         => 1,
      String_1_ID         => 41,
      String_2_ID         => 40,
      Image               =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("ACCEPT_I"),
         new String'("CODE"),
         new String'("CONFLICT"),
         new String'("CONFLICT_RESOLUTION"),
         new String'("END"),
         new String'("ELSIF"),
         new String'("IF"),
         new String'("IN"),
         new String'("KEYWORD"),
         new String'("NON_GRAMMAR"),
         new String'("ON"),
         new String'("REDUCE_I"),
         new String'("SHIFT_I"),
         new String'("TOKEN"),
         new String'("RAW_CODE"),
         new String'("REGEXP"),
         new String'("ACTION"),
         new String'("BAR"),
         new String'("COLON"),
         new String'("COLON_COLON_EQUAL"),
         new String'("EQUAL"),
         new String'("GREATER"),
         new String'("LEFT_BRACE"),
         new String'("LEFT_BRACKET"),
         new String'("LEFT_PAREN"),
         new String'("LESS"),
         new String'("MINUS"),
         new String'("PERCENT"),
         new String'("PLUS"),
         new String'("QUESTION"),
         new String'("RIGHT_BRACE"),
         new String'("RIGHT_BRACKET"),
         new String'("RIGHT_PAREN"),
         new String'("SEMICOLON"),
         new String'("STAR"),
         new String'("NUMERIC_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL_1"),
         new String'("STRING_LITERAL_2"),
         new String'("Wisi_EOI"),
         new String'("wisitoken_accept"),
         new String'("regexp_string"),
         new String'("conflict_item"),
         new String'("conflict_item_list"),
         new String'("token_name"),
         new String'("declaration"),
         new String'("identifier_list"),
         new String'("IDENTIFIER_BAR_list"),
         new String'("declaration_item"),
         new String'("declaration_item_list"),
         new String'("nonterminal"),
         new String'("semicolon_opt"),
         new String'("rhs_list"),
         new String'("rhs"),
         new String'("rhs_attribute"),
         new String'("rhs_element"),
         new String'("rhs_item_list"),
         new String'("rhs_item"),
         new String'("rhs_group_item"),
         new String'("rhs_optional_item"),
         new String'("rhs_multiple_item"),
         new String'("rhs_alternative_list"),
         new String'("compilation_unit"),
         new String'("compilation_unit_list"),
         new String'("Wisi_SOI")),
      Terminal_Image_Width => 19,
      Image_Width          => 21,
      Last_Lookahead       => 43);

   type Token_Enum_ID is
     (WHITESPACE_ID,
      NEW_LINE_ID,
      COMMENT_ID,
      ACCEPT_I_ID,
      CODE_ID,
      CONFLICT_ID,
      CONFLICT_RESOLUTION_ID,
      END_ID,
      ELSIF_ID,
      IF_ID,
      IN_ID,
      KEYWORD_ID,
      NON_GRAMMAR_ID,
      ON_ID,
      REDUCE_I_ID,
      SHIFT_I_ID,
      TOKEN_ID,
      RAW_CODE_ID,
      REGEXP_ID,
      ACTION_ID,
      BAR_ID,
      COLON_ID,
      COLON_COLON_EQUAL_ID,
      EQUAL_ID,
      GREATER_ID,
      LEFT_BRACE_ID,
      LEFT_BRACKET_ID,
      LEFT_PAREN_ID,
      LESS_ID,
      MINUS_ID,
      PERCENT_ID,
      PLUS_ID,
      QUESTION_ID,
      RIGHT_BRACE_ID,
      RIGHT_BRACKET_ID,
      RIGHT_PAREN_ID,
      SEMICOLON_ID,
      STAR_ID,
      NUMERIC_LITERAL_ID,
      IDENTIFIER_ID,
      STRING_LITERAL_1_ID,
      STRING_LITERAL_2_ID,
      Wisi_EOI_ID,
      wisitoken_accept_ID,
      regexp_string_ID,
      conflict_item_ID,
      conflict_item_list_ID,
      token_name_ID,
      declaration_ID,
      identifier_list_ID,
      IDENTIFIER_BAR_list_ID,
      declaration_item_ID,
      declaration_item_list_ID,
      nonterminal_ID,
      semicolon_opt_ID,
      rhs_list_ID,
      rhs_ID,
      rhs_attribute_ID,
      rhs_element_ID,
      rhs_item_list_ID,
      rhs_item_ID,
      rhs_group_item_ID,
      rhs_optional_item_ID,
      rhs_multiple_item_ID,
      rhs_alternative_list_ID,
      compilation_unit_ID,
      compilation_unit_list_ID,
      Wisi_SOI_ID);

   type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;
   use all type WisiToken.Token_ID;
   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));
   function To_Token_Enum (Item : in WisiToken.Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));
   function "-" (Item : in WisiToken.Token_ID) return Token_Enum_ID renames To_Token_Enum;

   procedure declaration_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_4
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_5
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_6
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_7
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_8
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_9
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_10
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_11
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_12
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_13
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_14
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure declaration_15
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure nonterminal_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure nonterminal_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_item_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_item_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_item_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_item_4
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_item_5
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure rhs_optional_item_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);
end Wisitoken_Grammar_Actions;
