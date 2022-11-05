--  Abstract :
--
--  Utilities for translating input file structures to WisiToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015, 2017 - 2022 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Iterator_Interfaces;
with WisiToken.Generate.LR;
with WisiToken.Parse.LR;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Productions;
with WisiToken_Grammar_Runtime;
package WisiToken.BNF.Generate_Utils is

   EOI_Name : constant String := "Wisi_EOI";
   --  EOI_Name is used for Descriptor.EOI_ID token. It must be a valid
   --  Ada identifier when "_ID" is appended.

   SOI_Name : constant String := "Wisi_SOI";
   --  Similar to EOI_Name

   WisiToken_Accept_Name : constant String := "wisitoken_accept";

   type Generate_Data (Tokens : not null access constant WisiToken.BNF.Tokens) is limited record
      Descriptor : WisiToken.Descriptor_Access;
      Grammar    : WisiToken.Productions.Prod_Arrays.Vector;

      Action_Names : Names_Array_Array_Access;
      Check_Names  : Names_Array_Array_Access;
      --  Names of subprograms for each grammar semantic action and check;
      --  non-null only if there is an action or check in the grammar.

      Start_ID        : WisiToken.Token_ID;
      Source_Line_Map : WisiToken.Productions.Source_Line_Maps.Vector;

      --  The following fields are LR specific; so far, it's not worth
      --  splitting them out.

      Ignore_Conflicts   : Boolean                       := False;
      Conflicts          : WisiToken.Generate.LR.Conflict_Lists.Tree;
      LR_Parse_Table     : WisiToken.Parse.LR.Parse_Table_Ptr;
      Parser_State_Count : WisiToken.Unknown_State_Index := 0;
   end record;

   function Initialize
     (Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Access;
      Grammar_File_Name : in String;
      Ignore_Conflicts  : in Boolean := False)
     return Generate_Data;

   procedure Parse_Grammar_File
     (Grammar_Parser    : in out WisiToken.Parse.LR.Parser_No_Recover.Parser;
      Grammar_File_Name : in     String);

   function Parse_Grammar_File
     (Grammar_File_Name  : in     String;
      Input_Data         : in     WisiToken_Grammar_Runtime.User_Data_Access;
      Generate_Algorithm : in     WisiToken.BNF.Generate_Algorithm;
      Lexer              : in     WisiToken.BNF.Lexer_Type;
      Trace              : in out WisiToken.Trace'Class;
      Ignore_Conflicts   : in     Boolean)
     return Generate_Data;

   type Token_Container
     (Tokens     : not null access constant WisiToken.BNF.Tokens;
      Descriptor : WisiToken.Descriptor_Access_Constant)
     is tagged null record
   with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Ada.Strings.Unbounded.Unbounded_String;
   --  The Iterator_Element is given by Token_Name below.

   function All_Tokens (Data : aliased in Generate_Data) return Token_Container;

   function Find_Token_ID (Data : in Generate_Data; Token : in String) return Token_ID;

   type Token_Constant_Reference_Type
     (Element : not null access constant Ada.Strings.Unbounded.Unbounded_String)
     is null record
   with Implicit_Dereference => Element;

   type Token_Cursor (<>) is private;
   --  Iterate thru Keywords, Tokens, Rules in a canonical order:
   --
   --  1. Non_Grammar
   --  2. Keywords
   --  3. other terminal tokens, in declaration order
   --  4. EOI
   --  5. Accept
   --  6. Nonterminals
   --  7. SOI
   --
   --  Within each group, tokens occur in the order they were declared in
   --  the grammar file.

   function Constant_Reference
     (Container : aliased in Token_Container'Class;
      Cursor    :         in Token_Cursor)
     return Token_Constant_Reference_Type;

   function Is_Done (Cursor : in Token_Cursor) return Boolean;
   function Has_Element (Cursor : in Token_Cursor) return Boolean is (not Is_Done (Cursor));
   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Token_Cursor, Has_Element);
   function Iterate
     (Container    : aliased in Token_Container;
      Non_Grammar  :         in Boolean := True;
      Nonterminals :         in Boolean := True;
      Include_SOI  :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class;

   function First
     (Data         : in Generate_Data;
      Non_Grammar  : in Boolean := True;
      Nonterminals : in Boolean := True)
     return Token_Cursor;

   procedure Next
     (Data         : in     Generate_Data;
      Cursor       : in out Token_Cursor;
      Nonterminals : in     Boolean := True;
      Include_SOI  : in     Boolean := True);

   function ID (Cursor : in Token_Cursor) return Token_ID;

   function Name (Data : in Generate_Data; Cursor : in Token_Cursor) return String;
   --  Return the token name from the .wy file:
   --  Keywords: Keywords (i).name
   --  Tokens  : Tokens (i).Tokens (j).name
   --  Rules   : Rules (i).Left_Hand_Side

   function Kind (Data : in Generate_Data; Cursor : in Token_Cursor) return String;
   --  Return the token kind from the .wy file:
   --  Keywords: "keyword"
   --  Tokens  : Tokens (i).Kind
   --  Rules   : "nonterminal"

   function Value (Data : in Generate_Data; Cursor : in Token_Cursor) return String;
   --  Return the token value from the .wy file:
   --  Keywords: Keywords (i).value
   --  Tokens  : Tokens (i).Tokens (j).Value
   --  Rules   : empty string (they have no Value)

   function Repair_Image (Data : in Generate_Data; Cursor : in Token_Cursor) return String;
   --  Return the token repair image from the .wy file:
   --  Keywords: empty string
   --  Tokens  : Tokens (i).Tokens (j).Repair_Image
   --  Rules   : empty string

   function To_Nonterminal_ID_Set
     (Data : aliased in Generate_Data;
      Item :         in String_Lists.List)
     return Token_ID_Set;

   function To_McKenzie_Param
     (Data : aliased in Generate_Data;
      Item :         in McKenzie_Recover_Param_Type)
     return WisiToken.Parse.LR.McKenzie_Param_Type;

   procedure Put_Stats
     (Input_Data    : in WisiToken_Grammar_Runtime.User_Data_Type;
      Generate_Data : in Generate_Utils.Generate_Data);

private

   type Token_Cursor_Kind is
     (Non_Grammar_Kind, Terminals_Keywords, Terminals_Others, EOI, WisiToken_Accept, Nonterminal, SOI, Done);

   type Token_Cursor is record
      Kind        : Token_Cursor_Kind;
      ID          : Token_ID;
      Token_Kind  : WisiToken.BNF.Token_Lists.Cursor; -- Non_Grammar or Tokens, depending on Kind
      Token_Item  : String_Triple_Lists.Cursor;
      Keyword     : String_Pair_Lists.Cursor;
      Nonterminal : Rule_Lists.Cursor;
   end record;

end WisiToken.BNF.Generate_Utils;
