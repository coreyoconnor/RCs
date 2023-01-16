--  Abstract :
--
--  Types and operations for a packrat parser runtime.
--
--  References:
--
--  [ford thesis] Bryan Ford thesis http://bford.info/pub/lang/thesis
--
--  [langkit]     AdaCore langkit   https://github.com/adacore/langkit
--
--  [tratt 2010]  http://tratt.net/laurie/research/pubs/papers/
--                tratt__direct_left_recursive_parsing_expression_grammars.pdf
--
--  [warth 2008]  Warth, A., Douglass, J.R. and Millstein, T.D., 2008. Packrat
--                parsers can support left recursion. PEPM, 8, pp.103-110.
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

--  Design:
--
--  [ford thesis] uses Haskell lazy evaluation, and does not use a
--  lexer. We use a lexer to reduce the memory requirement. Although
--  eliminating the lexer would make it easier to support additional
--  syntax for a preprocessor or template generator.
--
--  [langkit] uses a lexer, and implements lazy evaluation via
--  Memo_State, Memo_Entry as we do here, except that their result
--  type is a specific AST type provided by a generic parameter; we
--  use the general purpose Syntax_Tree.Tree type.
--
--  [langkit] also applies a memory optimization; it only saves the
--  last 16 results for each nonterminal. We don't do that yet, so we
--  can get some data on how well that works.

pragma License (Modified_GPL);
with WisiToken.Syntax_Trees;
package WisiToken.Parse.Packrat is

   type Memo_State is (No_Result, Failure, Success);
   subtype Result_States is Memo_State range Failure .. Success;

   type Memo_Entry (State : Memo_State := No_Result) is record
      Max_Examined_Pos : Syntax_Trees.Stream_Index;
      --  For error message.

      case State is
      when No_Result =>
         Recursive : Boolean := False;

      when Failure =>
         null;

      when Success =>
         Result   : Syntax_Trees.Node_Access;
         Last_Pos : Syntax_Trees.Stream_Index; -- Last terminal in Result

      end case;
   end record;
   subtype Success_Memo_Entry is Memo_Entry (Success);
   subtype Result_Type is Memo_Entry with Dynamic_Predicate => Result_Type.State in Result_States;

   No_Result_Memo : constant Memo_Entry := (No_Result, WisiToken.Syntax_Trees.Invalid_Stream_Index, False);

   function Image_Pos (Element : in Syntax_Trees.Stream_Index) return String;
   --  "0" for Invalid_Stream_Index, Node_Index'Image otherwise.

   function Image (Item : in Memo_Entry; Tree : in Syntax_Trees.Tree) return String;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String;

   subtype Positive_Node_Index is Syntax_Trees.Node_Index range 1 .. Syntax_Trees.Node_Index'Last;
   package Memos is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Node_Index, Memo_Entry, Default_Element => (others => <>));
   --  Memos is indexed by Node_Index of terminals in Shared_Stream
   --  (incremental parse is not supported).

   type Derivs is array (Token_ID range <>) of Memos.Vector;

   procedure Clear (Derivs : in out Packrat.Derivs);
   --  Free memory allocated by Derivs; set all to Empty_Vector.

   function Get_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index)
     return Memo_Entry;
   --  Return Derivs (Nonterm)(Pos) if present; No_Result_Memo if not.

   procedure Set_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index;
      Memo    : in     Memo_Entry);
   --  Add or replace Derivs (Nonterm)(Pos).

   type Parser (First_Nonterminal, Last_Nonterminal : Token_ID) is abstract new WisiToken.Parse.Base_Parser with
   record
      Direct_Left_Recursive : Token_ID_Set (First_Nonterminal .. Last_Nonterminal);
      Derivs                : Packrat.Derivs (First_Nonterminal .. Last_Nonterminal);
   end record;

   overriding procedure Finalize (Object : in out Parser);

   procedure Finish_Parse
     (Parser : in out Packrat.Parser'Class;
      Result : in out Memo_Entry);
   --  If a single parser succeeded, leaves Parser.Tree in
   --  Fully_Parsed state.
   --
   --  If there were recovered errors, the error information is in
   --  Parser.Tree.
   --
   --  If the parse did not succeed, raise Parse_Error with an error
   --  message, and Parser.Parsers is left intact for error recover.

end WisiToken.Parse.Packrat;
