--  Abstract :
--
--  A generalized LR parser.
--
--  In a child package of Parser.LR partly for historical reasons,
--  partly to allow McKenzie_Recover to be in a sibling package.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 - 2022 Free Software Foundation, Inc.
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

with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
limited with WisiToken.Parse.LR.McKenzie_Recover.Base;
package WisiToken.Parse.LR.Parser is
   type Parser;

   type Language_Fixes_Access is access procedure
     (Super             : in out Base.Supervisor;
      Shared_Parser     : in out Parser;
      Parser_Index      : in     SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration);
   --  Config encountered a parse table Error action, or failed a
   --  semantic check; attempt to provide a language-specific fix,
   --  enqueuing new configs on Local_Config_Heap.
   --
   --  For a failed semantic check, Config.Stack is in the pre-reduce
   --  state, Config.Error_Token gives the nonterm token,
   --  Config.Check_Token_Count the token count for the reduce. May be
   --  called with Nonterm.Virtual = True or Tree.Valid_Indices (stack
   --  top token_count items) false.
   --
   --  For an Error action, Config.Error_Token gives the terminal that
   --  caused the error.

   type Language_Matching_Begin_Tokens_Access is access procedure
     (Super                   :         in out Base.Supervisor;
      Shared_Parser           :         in out Parser;
      Tokens                  :         in     Token_ID_Array_1_3;
      Config                  : aliased in     Configuration;
      Matching_Tokens         :         in out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :         in out Boolean);
   --  Tokens (1) is the current token; Tokens (2 .. 3) are the following
   --  tokens (Invalid_Token_ID if none). Set Matching_Tokens to a
   --  terminal token sequence that starts a production matching Tokens.
   --  If Minimal_Complete would produce a bad solution at this error
   --  point, set Forbid_Minimal_Complete True.
   --
   --  For example, if Tokens is a block end, return tokens that are the
   --  corresponding block begin. If the error point is inside a
   --  multi-token 'end' (ie 'end if;', or 'end <name>;'), set
   --  Forbid_Minimal_Complete True.
   --
   --  ada-mode uses Peek_Sequential_Start in this subprogram, so it
   --  requires Super, Shared_Parser, aliased Config.

   type Language_String_ID_Set_Access is access function
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;
   --  Return a Token_ID_Set containing String_Literal_ID and
   --  nonterminals that can contain String_Literal_ID as part of an
   --  expression. Used in placing a missing string quote.

   type Parser is new WisiToken.Parse.Base_Parser with record
      Table                          : Parse_Table_Ptr;
      Language_Fixes                 : Language_Fixes_Access;
      Language_Matching_Begin_Tokens : Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : Language_String_ID_Set_Access;

      String_Quote_Checked : Base_Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Resume_Active : Boolean := False;
      Min_Sequential_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
      Max_Sequential_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'First;

      Parsers : aliased Parser_Lists.List;

      Partial_Parse_Active    : access Boolean;
      Partial_Parse_Byte_Goal : access WisiToken.Buffer_Pos;
      --  Used by In_Parse_Actions to terminate Partial_Parse.
   end record;

   --  It is tempting to declare Finalize here, to free Parser.Table. But
   --  Wisi.Parse_Context reuses the table between parser instances, so
   --  we can't do that. Other applications must explicitly free
   --  Parser.Table if they care.

   procedure New_Parser
     (Parser                         :    out LR.Parser.Parser;
      Lexer                          : in     WisiToken.Lexer.Handle;
      Table                          : in     Parse_Table_Ptr;
      Productions                    : in     Syntax_Trees.Production_Info_Trees.Vector;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access;
      User_Data                      : in     WisiToken.Syntax_Trees.User_Data_Access);

   procedure Edit_Tree
     (Parser : in out LR.Parser.Parser;
      Edits  : in     KMN_Lists.List)
   with Pre => Parser.Tree.Editable,
     Post => Parser.Tree.Stream_Count = 1;
   --  Assumes Parser.Lexer.Source has changed in a way reflected in
   --  Edits. Uses Edits to direct editing Parser.Tree to reflect lexing
   --  the changed source, in preparation for Incremental_Parse; result
   --  is in Tree.Shared_Stream.

   overriding procedure Parse
     (Shared_Parser    : in out LR.Parser.Parser;
      Recover_Log_File : in     Ada.Text_IO.File_Type;
      Edits            : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited       : in     Boolean        := False);

   overriding procedure Execute_Actions
     (Parser              : in out LR.Parser.Parser;
      Action_Region_Bytes : in     WisiToken.Buffer_Region);
   --  Call Parser.User_Data.Insert_Token, Parser.User_Data.Delete_Token
   --  on any tokens inserted/deleted by error recovery. Update
   --  Parser.Line_Begin_Tokens to reflect error recovery. Then call
   --  User_Data.Reduce and the grammar post parse actions on all
   --  nonterms in the syntax tree that overlap Action_Region_Bytes, by
   --  traversing the tree in depth-first order.

end WisiToken.Parse.LR.Parser;
