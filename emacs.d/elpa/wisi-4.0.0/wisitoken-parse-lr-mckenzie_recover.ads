--  Abstract :
--
--  Implement [McKenzie] error recovery, extended to parallel parsers.
--
--  References:
--
--  [McKenzie] McKenzie, Bruce J., Yeatman, Corey, and De Vere,
--  Lorraine. Error repair in shift reduce parsers. ACM Trans. Prog.
--  Lang. Syst., 17(4):672-689, July 1995.  Described in [Grune 2008] ref 321.
--
--  [Grune 2008] Parsing Techniques, A Practical Guide, Second
--  Edition. Dick Grune, Ceriel J.H. Jacobs.
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
limited with WisiToken.Parse.LR.McKenzie_Recover.Base;
package WisiToken.Parse.LR.McKenzie_Recover is
   use all type WisiToken.Syntax_Trees.Stream_Index;
   use all type Ada.Containers.Count_Type;

   Bad_Config : exception;
   --  Raised when a config is determined to violate some programming
   --  convention; abandon it. In Debug_Mode, report it, so it can be
   --  fixed. We don't use SAL.Programmer_Error for this, because the
   --  programming bug can easily be ignored by abandoning the config.

   Invalid_Case : exception;
   --  Raised to abandon error recover cases that don't apply, when they
   --  are not easily abandoned by 'if' or 'case'. We don't use
   --  Bad_Config for that, because it is not a programmer error.

   type Recover_Status is (Fail_Check_Delta, Fail_Enqueue_Limit, Fail_No_Configs_Left, Fail_Programmer_Error, Success);

   function Recover (Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser) return Recover_Status;
   --  Attempt to modify Parser.Parsers state and Parser.Lookahead to
   --  allow recovering from an error state.

   Force_Full_Explore        : Boolean := False;
   --  Sometimes recover throws an exception in a race condition case
   --  that is hard to reproduce. Setting this True ignores all Success,
   --  so all configs are checked.

   Force_High_Cost_Solutions : Boolean := False;
   --  Similarly, setting this true keeps all solutions that are found,
   --  and forces at least three.

   procedure Clear_Sequential_Index (Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser);
   --  Reset nodes set by Set_Sequential_Index.

private

   ----------
   --  Visible for child packages. Alphabetical.
   --
   --  The various Check subprograms raise Bad_Config for check fail, and
   --  there are no preconditions, so the checks are always performed.

   type Config_Stream_Parents (Stream : access constant Bounded_Streams.List) is
   record
      --  Like Syntax_Trees.Stream_Node_Parents, but using a Configuration
      --  input stream; does not continue into Tree streams.
      Element : Bounded_Streams.Cursor;
      Node    : Syntax_Trees.Node_Access;
      Parents : Syntax_Trees.Node_Stacks.Stack;
   end record;

   type Peek_Sequential_State (Stream : access constant Bounded_Streams.List) is
   record
      --  Like Syntax_Trees.Stream_Node_Parents, but using a Configuration
      --  input stream; continues forward into Tree shared stream. There is
      --  no Prev operation; cannot continue backward into config stack.
      Input_Terminal      : Config_Stream_Parents (Stream);   -- in Config.Input_Stream
      Sequential_Terminal : Syntax_Trees.Stream_Node_Parents; -- in Tree.Shared_Stream
   end record;

   procedure Check
     (ID          : in Token_ID;
      Expected_ID : in Token_ID;
      Descriptor  : in WisiToken.Descriptor)
   with Inline => True;
   --  Check that ID = Expected_ID; raise Bad_Config if not.

   procedure Delete_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      ID            : in     Token_ID);
   --  Check that the next input token in Config has ID. Append a Delete op
   --  to Config.Ops, and append it to Config.Insert_Delete.
   --
   --  ID = Invalid_Token_ID skips the check.
   --
   --  This or the next routine must be used instead of Config.Ops.Append
   --  (Delete...) unless the code also takes care of changing
   --  Config.Current_Shared_Token or Config.Input_Stream. Note that this
   --  routine does _not_ increment Config.Current_Shared_Token or
   --  Config.Input_Stream, so it can only be used to delete one token.

   procedure Delete_Check
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in out Configuration;
      IDs           :         in     Token_ID_Array);
   --  Call Delete_Check for each ID in IDs, incrementing to the next
   --  token for each.

   procedure Delete_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Peek_State    : in out Peek_Sequential_State;
      ID            : in     Token_ID);
   --  If ID is not Invalid_Token_ID, check that
   --  Parse.Peek_Sequential_Terminal (Peek_State) has ID. Append a Delete op
   --  to Config.Ops, and append it to Config.Insert_Delete. Then
   --  increment Peek_State to the next shared terminal.
   --
   --  Peek_State is initialized by Peek_Sequential_Start.

   procedure Do_Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   with Pre => not Recover_Op_Arrays.Is_Full (Config.Ops);
   --  Push back Config.Stack top to Config.Input_Stream. Appends to
   --  Config.Ops. Nonterms are not broken down. We assume caller has
   --  checked Push_Back_Valid.

   function Find_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration;
      ID     : in Token_ID)
     return Boolean;
   --  Search Config.Stack for a token with ID, starting at
   --  stack top. Return True if found, False if not.

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with ID, starting at
   --  Matching_Index. If found, Matching_Index points to it.
   --  If not found, Matching_Index = Config.Stack.Depth.

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with ID in IDs, starting at
   --  Matching_Index. If found, Matching_Index points to it.
   --  If not found, Matching_Index = Config.Stack.Depth.

   procedure Find_Descendant_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      ID_Set         : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with id in ID_Set, with a
   --  descendant with id = ID, starting at Matching_Index. If found,
   --  Matching_Index points to it. If not found, Matching_Index =
   --  Config.Stack.Depth.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean);
   --  Search Config.Stack for a token matching Name, starting at
   --  Matching_Name_Index. If found, Matching_Name_Index points to it.
   --  If not found, Matching_Name_Index = Config.Stack.Depth.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean);
   --  Search Config.Stack for a token matching Name, starting at
   --  Matching_Name_Index. If found, Matching_Name_Index points to it.
   --  If not found, Matching_Name_Index = Config.Stack.Depth.
   --
   --  Also count tokens with ID = Other_ID.

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      ID            : in     Token_ID);
   --  Append an Insert before Config.Current_Shared_Token or
   --  Config.Input_Stream.First op to Config.Ops, and append it to
   --  Config.Insert_Deleted.

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      IDs           : in     Token_ID_Array);
   --  Call Insert for each item in IDs.

   procedure Insert
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Before        : in     Syntax_Trees.Valid_Node_Access;
      ID            : in     Token_ID);
   --  Same as Insert, but before Before.

   function Peek_Sequential_Start
     (Super         :         in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config        : aliased in     Configuration)
     return Peek_Sequential_State;

   function Peek_Sequential_Terminal (State : in Peek_Sequential_State) return Syntax_Trees.Node_Access;
   --  Return State current sequential terminal; set by
   --  Peek_Sequential_Start and Peek_Next_Sequential_Terminal.
   --
   --  Returns Invalid_Node_Access when the current sequential terminal
   --  is past EOI, possibly because shared EOI had an error, and was
   --  found in the config input stream.

   procedure Peek_Next_Sequential_Terminal
     (Tree  : in     Syntax_Trees.Tree;
      State : in out Peek_Sequential_State);
   --  Step State to next sequential terminal. Can step past EOI.

   procedure Set_Initial_Sequential_Index
     (Parsers    : in out WisiToken.Parse.LR.Parser_Lists.List;
      Tree       : in     Syntax_Trees.Tree;
      Streams    : in out Syntax_Trees.Stream_ID_Array;
      Terminals  : in out Syntax_Trees.Stream_Node_Parents_Array;
      Initialize : in     Boolean)
   with Pre => Terminals'First = 1 and Terminals'Last = (if Initialize then Parsers.Count else Parsers.Count + 1) and
               Streams'First = Terminals'First and Streams'Last = Terminals'Last,
     Post => (for all Term of Terminals =>
                (if Initialize
                 then Tree.Get_Sequential_Index (Term.Ref.Node) /= Syntax_Trees.Invalid_Sequential_Index
                 else Tree.Get_Sequential_Index (Term.Ref.Node) = Syntax_Trees.Invalid_Sequential_Index));
   --  If Initialize, prepare for setting sequential_index in the parse
   --  streams for error recover. If not Initialize, prepare for clearing
   --  sequential_index after recover is done. Terminals'Last is the
   --  shared stream (see body for rationale).
   --
   --  Set Terminals to a common starting point for
   --  Extend_Sequential_Index, nominally parser Current_Token for
   --  Initialize, stack top for not Initialize. If Initialize, set
   --  Sequential_Index in all Terminals nodes to 1; if not Initialize,
   --  set to Invalid_Sequential_Index.

   procedure Extend_Sequential_Index
     (Tree      : in     Syntax_Trees.Tree;
      Streams   : in     Syntax_Trees.Stream_ID_Array;
      Terminals : in out Syntax_Trees.Stream_Node_Parents_Array;
      Target    : in     Syntax_Trees.Base_Sequential_Index;
      Positive  : in     Boolean;
      Clear     : in     Boolean)
   with Pre =>
     (if Clear
      then (for all Term of Terminals =>
              Tree.Get_Sequential_Index (Term.Ref.Node) = Syntax_Trees.Invalid_Sequential_Index)
      else
        (for all Term of Terminals =>
           Tree.Get_Sequential_Index (Term.Ref.Node) /= Syntax_Trees.Invalid_Sequential_Index)
           and then
          (for some Term of Terminals =>
             Tree.ID (Term.Ref.Node) /=
             (if Positive
              then Tree.Lexer.Descriptor.EOI_ID
              else Tree.Lexer.Descriptor.SOI_ID) and
             (if Positive
              then Tree.Get_Sequential_Index (Term.Ref.Node) < Target
              else Tree.Get_Sequential_Index (Term.Ref.Node) > Target))),
     Post =>
       (for all Term of Terminals =>
          (if Clear
           then Tree.Get_Sequential_Index (Term.Ref.Node) = Syntax_Trees.Invalid_Sequential_Index
           else Tree.Get_Sequential_Index (Term.Ref.Node) /= Syntax_Trees.Invalid_Sequential_Index));
   --  If Clear, clear all Sequential_Index,
   --  starting at Terminals, and moving in Positive direction.
   --  Otherwise, set Sequential_Index in Tree nodes before/after
   --  Terminals, thru Target.

   function Push_Back_Valid
     (Super                 : in out Base.Supervisor;
      Shared_Parser :         in out LR.Parser.Parser;
      Config                : in     Configuration;
      Push_Back_Undo_Reduce : in     Boolean)
     return Boolean;
   --  True if Push_Back is a valid op for Config.
   --
   --  Normally Push_Back_Valid forbids push_back of an entire
   --  Undo_Reduce; Language_Fixes may override that by setting
   --  Push_Back_Undo_Reduce True.

   procedure Push_Back
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Push_Back_Undo_Reduce : in     Boolean);
   --  If not Push_Back_Valid, raise Invalid_Case. Otherwise do
   --  Push_Back.
   --
   --  Normally Push_Back_Valid forbids push_back of an entire
   --  Undo_Reduce; Language_Fixes may override that by setting
   --  Push_Back_Undo_Reduce True.

   procedure Push_Back_Check
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Expected_ID           : in     Token_ID;
      Push_Back_Undo_Reduce : in     Boolean);
   --  Check that Config.Stack top has Expected_ID; raise Bad_Config if
   --  not. Then call Push_Back.

   procedure Push_Back_Check
     (Super                 : in out Base.Supervisor;
      Shared_Parser         : in out LR.Parser.Parser;
      Config                : in out Configuration;
      Expected              : in     Token_ID_Array;
      Push_Back_Undo_Reduce : in     Boolean);
   --  Call Push_Back_Check for each item in Expected.

   procedure Put
     (Message      : in     String;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Config       : in     Configuration;
      Strategy     : in     Boolean := False);
   --  Put Message and an image of Config to Tree.Lexer.Trace.

   procedure Put_Line
     (Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Message      : in     String);
   --  Put message to Tree.Lexer.Trace, with parser and task info.

   function Undo_Reduce_Valid
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration)
     return Boolean;
   --  True if Undo_Reduce is valid for Config.

   procedure Unchecked_Undo_Reduce
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration);
   --  Undo the reduction that produced the top stack item, append op.

   procedure Undo_Reduce_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Expected      : in     Token_ID)
   with Inline => True;
   --  If not Undo_Reduce_Valid, raise Invalid_Case. Else call Check,
   --  Unchecked_Undo_Reduce. Caller should check for space in
   --  Config.Ops.

   procedure Undo_Reduce_Check
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Config        : in out Configuration;
      Expected      : in     Token_ID_Array);
   --  Call Undo_Reduce_Check for each item in Expected.

end WisiToken.Parse.LR.McKenzie_Recover;
