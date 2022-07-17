--  Abstract :
--
--  Config parsing subprograms.
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

with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
private package WisiToken.Parse.LR.McKenzie_Recover.Parse is
   use all type WisiToken.Syntax_Trees.Node_Label;

   function Reduce_Stack
     (Shared_Parser : in out LR.Parser.Parser;
      Stack         : in out Recover_Stacks.Stack;
      Action        : in     Reduce_Action_Rec;
      Nonterm       :    out Syntax_Trees.Recover_Token)
     return Syntax_Trees.In_Parse_Actions.Status;
   --  Reduce Stack according to Action, setting Nonterm.

   procedure Current_Token_ID_Peek_3
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in     Configuration;
      Tokens        :            out Token_ID_Array_1_3)
   with Post =>
     (for all Tok of Tokens => Tok = Invalid_Token_ID or else
        Is_Terminal (Tok, Shared_Parser.Tree.Lexer.Descriptor.all));
   --  Return the current terminal token from Config in Tokens (1).
   --  Return the two following terminal tokens in Tokens (2 .. 3). In
   --  incremental parse, they may be virtual.
   --
   --  In Parse because it has similar code to Current_Token, Next_Token.

   function Peek_Current_Element_Node
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  Stream element from Config.Shared_Token or Config.Input_Stream.

   function Peek_Current_First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  First_Terminal from Shared_Stream starting at Config.Shared_Token, or Config.Input_Stream.

   function Peek_Current_First_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in     Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  Calls Peek_Current_First_Terminal, then
   --  Super.Extend_Sequential_Index on the result.

   function First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Bounded_Streams.List)
     return Syntax_Trees.Node_Access;
   --  Return first terminal in Stream; Invalid_Node_Access if none.

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents);

   procedure First_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Ref           :    out Config_Stream_Parents);
   --  Calls Extend_Sequential_Terminal as needed.

   procedure Last_Sequential_Terminal
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Ref           : in out Config_Stream_Parents);
   --  Calls Extend_Sequential_Terminal as needed.

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   with Pre => Bounded_Streams.Has_Element (Ref.Element) and Ref.Node /= Syntax_Trees.Invalid_Node_Access;
   --  Can step past EOI.
   --  Calls Extend_Sequential_Terminal as needed.

   procedure Prev_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   with Pre => Bounded_Streams.Has_Element (Ref.Element) and Ref.Node /= Syntax_Trees.Invalid_Node_Access;

   procedure Left_Breakdown
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in out Bounded_Streams.List)
   with Pre => Stream.Length > 0 and then
               (declare Node : constant Syntax_Trees.Node_Access := Stream (Stream.First);
                begin Node /= Syntax_Trees.Invalid_Node_Access and then
                   (Tree.Label (Node) = Syntax_Trees.Nonterm and
                      Tree.First_Terminal (Node) /= Syntax_Trees.Invalid_Node_Access)),
     Post =>
       (declare Node : constant Syntax_Trees.Node_Access := Stream (Stream.First);
        begin Node /= Syntax_Trees.Invalid_Node_Access and then
           (Tree.Label (Node) in Syntax_Trees.Terminal_Label));
   --  Bring the first terminal in Stream (which cannot be empty) to
   --  Stream; delete any preceding empty nonterms.

   procedure Do_Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration);
   --  Delete Config Current_Token. Does not append to Config.Ops.

   type Parse_Item is record
      Config       : aliased Configuration;
      Action       : Parse_Action_Node_Ptr := null;
      Parsed       : Boolean               := False;
      Shift_Count  : Natural               := 0;
      Reduce_Count : Natural               := 0;

      --  On return from Parse, if Parsed = False, this item was queued by a
      --  conflict, but not parsed; it should be ignored.
      --
      --  Otherwise, if Config.Error_Token.ID = Invalid_Token_ID and
      --  Config.User_Parse_Action_Status.Label = Ok, Config was parsed
      --  successfully to the goal.
      --
      --  Otherwise, the parser failed a semantic check, or encountered an
      --  Error action. Action gives the last action processed. Shift_Count
      --  gives the number of shifts performed. If
      --  User_Parse_Action_Status.Label is Error, Action.Item.Verb must be
      --  Reduce, and Config is in the pre-reduce state.
   end record;

   package Parse_Item_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive, Parse_Item, Default_Element => (others => <>), Capacity => 10);
   --  Parse_Item_Arrays.Capacity sets maximum conflicts in one call to Parse

   package Parse_Item_Array_Refs is new Parse_Item_Arrays.Gen_Refs;

   function Parse
     (Super             :         in out Base.Supervisor;
      Shared_Parser     :         in out WisiToken.Parse.LR.Parser.Parser;
      Parser_Index      :         in     SAL.Peek_Type;
      Parse_Items       : aliased    out Parse_Item_Arrays.Vector;
      Config            :         in     Configuration;
      Shared_Token_Goal :         in     Syntax_Trees.Base_Sequential_Index;
      All_Conflicts     :         in     Boolean;
      Trace_Prefix      :         in     String)
     return Boolean;
   --  Attempt to parse Config and any conflict configs. A config is
   --  parsed when Config.Insert_Delete is all processed, and either
   --  Shared_Token_Goal = Invalid_Sequential_Index, or Shared_Token_Goal is
   --  shifted or an error is encountered. If All_Conflicts, return when
   --  all conflict configs have been parsed; if not All_Conflicts,
   --  return when one config is parsed without error.
   --
   --  Parsed configs are in Parse_Items; there is more than one if a
   --  conflict is encountered. Parse returns True if at least one
   --  Parse_Item parsed successfully to the goal. In that case, the
   --  other items are either not parsed or failed. See comment in
   --  Parse_Item for more detail.
   --
   --  Raises Bad_Config if parse encounters Unknown_State.

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
