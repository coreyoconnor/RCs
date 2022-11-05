--  Abstract :
--
--  Root package of an implementation of an LR (Left-to-right scanning
--  Rightmost-deriving) parser. Includes operations for building the
--  parse table at runtime. See the child packages .Parse and
--  .Parse_No_Recover for running the parser.
--
--  References :
--
--  See wisitoken.ads
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2022 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
with SAL.Gen_Array_Image;
with SAL.Gen_Bounded_Definite_Doubly_Linked_Lists.Gen_Image_Aux;
with SAL.Gen_Bounded_Definite_Stacks.Gen_Image_Aux;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Vectors_Sorted;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR is
   use all type WisiToken.Syntax_Trees.Node_Access;
   use all type WisiToken.Syntax_Trees.Base_Sequential_Index;
   use all type SAL.Base_Peek_Type;

   type All_Parse_Action_Verbs is (Pause, Shift, Reduce, Accept_It, Error);
   subtype Parse_Action_Verbs is All_Parse_Action_Verbs range Shift .. Error;
   subtype Minimal_Verbs is All_Parse_Action_Verbs range Shift .. Reduce;
   subtype Conflict_Parse_Actions is Parse_Action_Verbs range Shift .. Accept_It;
   --  Pause is only used for error recovery, to allow parallel parsers
   --  to re-sync on the same input terminal.

   --  WORKAROUND: GNAT Community 2020 with -gnat2020 S'Image outputs
   --  integer when S is a subtype.
   function Image (Item : in All_Parse_Action_Verbs) return String
   is (case Item is
       when Pause     => "PAUSE",
       when Shift     => "SHIFT",
       when Reduce    => "REDUCE",
       when Accept_It => "ACCEPT_IT",
       when Error     => "ERROR");

   subtype Token_ID_Array_1_3 is Token_ID_Array (1 .. 3);
   --  For Language_Matching_Begin_Tokens.

   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is
   record
      Production : Production_ID;
      --  The production that produced this action. Used to find kernel
      --  items during error recovery.

      case Verb is
      when Shift =>
         State : State_Index := State_Index'Last;

      when Reduce | Accept_It =>
         --  Production.LHS is the result nonterm
         Token_Count : Ada.Containers.Count_Type := 0;

      when Error =>
         null;
      end case;
   end record;
   subtype Shift_Action_Rec is Parse_Action_Rec (Shift);
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor) return String;
   --  Ada aggregate syntax, leaving out Action, Check in reduce; for debug output

   function Trace_Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor) return String;
   --  Used in parser trace, for compatibility with existing unit tests.
   --  Respects Trace_Parse_No_State_Numbers.

   function Equal (Left, Right : in Parse_Action_Rec) return Boolean;
   --  Ignore items not used by the canonical shift-reduce algorithm.

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Parse_Action_Node, Parse_Action_Node_Ptr);

   function Is_In (Item : in Parse_Action_Rec; List : in Parse_Action_Node_Ptr) return Boolean;
   --  True if Item is Equal to any element of List.

   type Action_Node is record
      Symbol  : Token_ID := Invalid_Token_ID; -- ignored if Action is Error
      Actions : Parse_Action_Node_Ptr := null;
   end record;

   procedure Delete
     (Container : in out Action_Node;
      Prev      : in     Parse_Action_Node_Ptr;
      Current   : in out Parse_Action_Node_Ptr);
   --  Delete Current from an action list. Container.Actions is the
   --  root of the list; updated as needed. Prev is the previous element
   --  in the list; null if none. Prev.Next is updated to
   --  Current.Next. Current is updated to Current.Next.

   function To_Key (Item : in Action_Node) return Token_ID is (Item.Symbol);

   function Compare (Left, Right : in Token_ID) return SAL.Compare_Result
   is (if Left < Right then SAL.Less
       elsif Left = Right then SAL.Equal
       else SAL.Greater);

   package Action_Arrays is new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Action_Node, Token_ID, To_Key, Compare, Default_Element => (others => <>));

   procedure Add
     (List   : in out Action_Arrays.Vector;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec);
   --  Add action to List, sorted on ascending Symbol.

   type Goto_Node is record
      Symbol : Token_ID    := Invalid_Token_ID;
      State  : State_Index := State_Index'Last;
   end record;

   function To_Key (Item : in Goto_Node) return Token_ID is (Item.Symbol);

   package Goto_Arrays is  new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Goto_Node, Token_ID, To_Key, Compare, Default_Element => (others => <>));

   type Kernel_Info is record
      Production       : Production_ID;
      Before_Dot       : Token_ID                  := Token_ID'First;
      Length_After_Dot : Ada.Containers.Count_Type := 0;

      Reduce_Production : Production_ID;
      Reduce_Count      : Ada.Containers.Count_Type := 0;
      --  The reduction that error recovery should do for this item if
      --  Length_After_Dot = 0. Reduce_Production /= Production when item
      --  after dot is nullable.
      --
      --  It is tempting to make Length_After_Dot a discriminant to
      --  eliminate Reduce_* when they are not needed, but we don't have a
      --  static value of Length_After_Dot when it is non-zero.
   end record;

   function Strict_Image (Item : in Kernel_Info) return String;
   --  Ada positional aggregate, for code generation

   function Image (Item : in Kernel_Info; Descriptor : in WisiToken.Descriptor) return String;
   --  For debug

   type Kernel_Info_Array is array (Ada.Containers.Count_Type range <>) of Kernel_Info;
   package Kernel_Info_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Ada.Containers.Count_Type, Kernel_Info, (others => <>));

   function To_Vector (Item : in Kernel_Info_Array) return Kernel_Info_Arrays.Vector;

   function Image is new Kernel_Info_Arrays.Gen_Image (Strict_Image);

   type Minimal_Action (Verb : Minimal_Verbs := Shift) is
   record
      Production  : Production_ID := Invalid_Production_ID;

      case Verb is
      when Shift =>
         ID    : Token_ID    := Invalid_Token_ID;
         State : State_Index := State_Index'Last;

      when Reduce =>
         Token_Count : Ada.Containers.Count_Type;
      end case;
   end record;

   function Strict_Image (Item : in Minimal_Action) return String;
   --  Strict Ada aggregate syntax, for generated code.

   function Image (Item : in Minimal_Action; Descriptor : in WisiToken.Descriptor) return String;
   --  For debugging

   type Minimal_Action_Array is array (Ada.Containers.Count_Type range <>) of Minimal_Action;
   package Minimal_Action_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Ada.Containers.Count_Type, Minimal_Action, (others => <>));

   function To_Vector (Item : in Minimal_Action_Array) return Minimal_Action_Arrays.Vector;

   function Image is new Minimal_Action_Arrays.Gen_Image_Aux (Descriptor, Trimmed_Image, Image);
   function Strict_Image is new Minimal_Action_Arrays.Gen_Image (Strict_Image);

   type Parse_State is record
      Action_List : Action_Arrays.Vector;
      Goto_List   : Goto_Arrays.Vector;

      --  The following are used in error recovery.
      Kernel                   : Kernel_Info_Arrays.Vector;
      Minimal_Complete_Actions : Minimal_Action_Arrays.Vector;
      --  Parse actions that will most quickly complete a production in this
      --  state. Kernel is used to reduce the number of actions.
   end record;

   type Parse_State_Array is array (State_Index range <>) of Parse_State;

   procedure Add_Action
     (State       : in out Parse_State;
      Symbol      : in     Token_ID;
      Production  : in     Production_ID;
      State_Index : in     WisiToken.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     Parse_Action_Verbs;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type);
   --  Add a Reduce or Accept_It action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbols         : in     Token_ID_Array;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type);
   --  Add duplicate Reduce actions, and final Error action, to tail of
   --  State action list.

   procedure Add_Conflict
     (State             : in out Parse_State;
      Symbol            : in     Token_ID;
      Reduce_Production : in     Production_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type);
   --  Add a Reduce conflict to State.

   procedure Add_Goto
     (State    : in out Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     State_Index);
   --  Add a goto item to State goto list; keep goto list sorted in ascending order on Symbol.

   type McKenzie_Param_Type
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
   is record
      Insert      : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Delete      : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Push_Back   : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      Undo_Reduce : Token_ID_Array_Natural (First_Nonterminal .. Last_Nonterminal);
      --  Cost of operations on config stack, input.

      Minimal_Complete_Cost_Delta : Integer;
      --  Added to cost when using Minimal_Complete_Action; typically negative.

      Matching_Begin : Integer;
      --  Cost of Matching_Begin strategy (applied once, independent of
      --  token count).

      Fast_Forward : Integer;
      --  Cost of moving the edit point forward over input tokens.

      Ignore_Check_Fail : Natural;
      --  Cost of ignoring a semantic check failure. Should be at least the
      --  cost of a typical fix for such a failure.

      Zombie_Limit : Positive;
      --  Terminal tokens to wait before terminating parser that encountered
      --  an error. See test_mckenzie_recover.adb Revive_Zombie for example
      --  of why this is not hard-coded at 0. Setting it the same as
      --  Check_Limit is often a good choice.

      Check_Limit : Syntax_Trees.Sequential_Index;
      --  Max count of shared tokens to parse ahead when checking a
      --  configuration.

      Check_Delta_Limit : Natural;     -- max configs checked, delta over successful parser.
      Enqueue_Limit     : Natural;     -- max configs enqueued.
   end record;

   Default_McKenzie_Param : constant McKenzie_Param_Type :=
     (First_Terminal              => Token_ID'Last,
      Last_Terminal               => Token_ID'First,
      First_Nonterminal           => Token_ID'Last,
      Last_Nonterminal            => Token_ID'First,
      Insert                      => (others => 0),
      Delete                      => (others => 0),
      Push_Back                   => (others => 0),
      Undo_Reduce                 => (others => 0),
      Minimal_Complete_Cost_Delta => -1,
      Fast_Forward                => 0,
      Matching_Begin              => 0,
      Ignore_Check_Fail           => 0,
      Zombie_Limit                => 4,
      Check_Limit                 => 4,
      Check_Delta_Limit           => Natural'Last,
      Enqueue_Limit               => Natural'Last);

   procedure Set_McKenzie_Options (Param : in out McKenzie_Param_Type; Config : in String);
   --  Set options from Config. Config contains space-separated name=value
   --  pairs. See body for exact names.

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
     is tagged
   record
      States                : Parse_State_Array (State_First .. State_Last);
      Error_Action          : Parse_Action_Node_Ptr;
      Error_Recover_Enabled : Boolean;
      McKenzie_Param        : McKenzie_Param_Type (First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
      Max_Parallel          : SAL.Base_Peek_Type := 15;
   end record;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index
   with Pre => ID in Table.First_Nonterminal .. Table.Last_Nonterminal;
   --  Return next state after reducing stack by nonterminal ID;
   --  Unknown_State if none (only possible during error recovery).

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr
   with Pre => ID in Table.First_Terminal .. Table.Last_Terminal,
     Post => Action_For'Result /= null;
   --  Return the action for State, terminal ID.

   function Shift_State (Action_List : in Parse_Action_Node_Ptr) return State_Index;
   --  Return State from the shift action in Action_List.

   procedure Undo_Reduce
     (Tree      : in out Syntax_Trees.Tree;
      Table     : in     Parse_Table;
      Stream    : in     Syntax_Trees.Stream_ID;
      User_Data : in     Syntax_Trees.User_Data_Access_Constant);
   --  Undo reduction of nonterm at Stream.Stack_Top; Stack_Top is then
   --  the last Child of the nonterm.
   --
   --  If Stream.Stack_Top has an error, it is moved to the first terminal.
   --
   --  This duplicates Parser_Lists.Undo_Reduce; that is used by the main
   --  parser when there is a Parser_State; this is used by Edit_Tree and
   --  error reccover when there is not.

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set;

   type Parse_Table_Ptr is access Parse_Table;
   procedure Free_Table (Table : in out Parse_Table_Ptr);

   function Get_Text_Rep (File_Name : in String) return Parse_Table_Ptr;
   --  Read machine-readable text format of states (as output by
   --  WisiToken.Generate.LR.Put_Text_Rep) from file File_Name.

   ----------
   --  For McKenzie_Recover. Declared here because Parser_Lists needs
   --  these, Mckenzie_Recover needs Parser_Lists.
   --
   --  We don't maintain a syntax tree during recover; it's too slow, and
   --  not needed for any operations. The parser syntax tree is used for
   --  Undo_Reduce, which is only done on nonterms reduced by the main
   --  parser, not virtual nonterms produced by recover.

   type Recover_Stack_Item is record
      State : Unknown_State_Index := Unknown_State;

      Token : Syntax_Trees.Recover_Token;
      --  Virtual is False if token is from input text; True if inserted
      --  during recover. If not Virtual, Element_Node = Node (ie rooted
      --  stream ref).
   end record;

   package Recover_Stacks is new SAL.Gen_Bounded_Definite_Stacks (Recover_Stack_Item);

   function Image (Item : in Recover_Stack_Item; Tree : in Syntax_Trees.Tree) return String
     is ((if Item.State = Unknown_State then " " else Trimmed_Image (Item.State)) & " : " &
           Syntax_Trees.Image (Tree, Item.Token));

   function Recover_Stack_Image is new Recover_Stacks.Gen_Image_Aux (Syntax_Trees.Tree, Image);
   --  Unique name for calling from debugger

   function Image
     (Stack : in Recover_Stacks.Stack;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
     return String
     renames Recover_Stack_Image;

   function Stack_Has
     (Tree  : in Syntax_Trees.Tree;
      Stack : in Recover_Stacks.Stack;
      ID    : in Token_ID)
     return Boolean;
   --  True if some item in Stack has ID.

   function Valid_Tree_Indices (Stack : in Recover_Stacks.Stack; Depth : in SAL.Base_Peek_Type) return Boolean with
     Pre => Stack.Depth >= Depth;
   --  Return True if Stack top Depth items are not Virtual, which is
   --  true if they were copied from the parser stack, and not pushed by
   --  recover.

   package Bounded_Streams is new SAL.Gen_Bounded_Definite_Doubly_Linked_Lists (Syntax_Trees.Node_Access);

   function Image (Item : in Syntax_Trees.Node_Access; Tree : in Syntax_Trees.Tree) return String
   is (Tree.Image (Item, Node_Numbers => True));

   function Image is new Bounded_Streams.Gen_Image_Aux (Syntax_Trees.Tree, LR.Image);

   type Strategies is
     (Ignore_Error, Language_Fix, Minimal_Complete, Matching_Begin,
      Push_Back, Undo_Reduce, Insert, Delete, String_Quote);

   type Strategy_Counts is array (Strategies) of Natural;
   function Image is new SAL.Gen_Array_Image (Strategies, Natural, Strategy_Counts, Trimmed_Image);

   type Minimal_Complete_State is (None, Active, Done);

   No_Insert_Delete : constant SAL.Base_Peek_Type := 0;

   type Configuration is record
      Stack : Recover_Stacks.Stack (90);
      --  Initially built from the parser stack, then the stack after the
      --  Ops below have been performed.
      --
      --  Required size is determined by source code structure nesting;
      --  larger size slows down recover due to memory cache thrashing and
      --  allocation.
      --
      --  Emacs ada-mode wisi.adb needs > 50
      --  wisitoken-parse.adb Edit_Tree needs > 70

      Current_Shared_Token : Syntax_Trees.Terminal_Ref := Syntax_Trees.Invalid_Stream_Node_Ref;
      --  Current input token in Shared_Stream; to be input after all of
      --  Input_Stream and Insert_Delete is input. Initially the error
      --  token. In batch parse, always a single Source_Terminal; in
      --  incremental parse, always the first terminal in the stream
      --  element, which may be Invalid_Node_Access if the stream element is
      --  empty.

      Input_Stream : aliased Bounded_Streams.List (20);
      --  Holds tokens copied from Shared_Stream when Push_Back operations
      --  are performed, or added by Insert. Delete may be applied to these,
      --  which requires that nonterms be broken down (similar to
      --  Syntax_Trees.Left_Breakdown).
      --
      --  Current token is root of Input_Stream.First.
      --
      --  To justify the size; in a typical recover we might need to push
      --  back a few terminals, and one nonterm that is then broken down
      --  (max of 15 tokens for most languages). For
      --  test_mckenzie_recover.adb, 10 is too small, 20 is enough.

      Insert_Delete : aliased Recover_Op_Arrays.Vector;
      --  Edits to the input stream that are not yet parsed; contains only
      --  Insert and Delete ops, in node_index order.

      Current_Insert_Delete : SAL.Base_Peek_Type := No_Insert_Delete;
      --  Index of the next op in Insert_Delete. If No_Insert_Delete, use
      --  Current_Tree_Token.

      Resume_Token_Goal : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
      --  A successful solution shifts this terminal token from
      --  Tree.Shared_Stream. Per-config because it increases with Delete;
      --  we set Shared_Parser.Resume_Token_Goal only from successful
      --  configs.

      String_Quote_Checked_Line     : Base_Line_Number_Type := Invalid_Line_Number;
      String_Quote_Checked_Byte_Pos : Base_Buffer_Pos       := Invalid_Buffer_Pos;
      --  Max line, line_end_pos checked for missing string quote.

      Error_Token                 : Syntax_Trees.Recover_Token;
      In_Parse_Action_Token_Count : SAL.Base_Peek_Type := 0;
      In_Parse_Action_Status      : Syntax_Trees.In_Parse_Actions.Status;
      --  If parsing this config ended with a parse error, Error_Token is
      --  the token that failed to shift, Check_Status.Label is Ok.
      --
      --  If parsing this config ended with an In_Parse_Action fail,
      --  Error_Token is the nonterm created by the reduction,
      --  In_Parse_Action_Token_Count the number of tokens in the right hand
      --  side, and In_Parse_Action_Status is the error.
      --
      --  Error_Token is set to Invalid_Token_ID when Config is parsed
      --  successfully, or modified so the error is no longer meaningful (ie
      --  in explore when adding an op, or in language_fixes when adding a
      --  fix).

      Ops : aliased Recover_Op_Arrays.Vector;
      --  Record of operations applied to this Config, in application order.
      --  Insert and Delete ops that are not yet parsed are reflected in
      --  Insert_Delete, in token_index order.

      Cost : Natural := 0;

      Strategy_Counts : LR.Strategy_Counts := (others => 0);
      --  Count of strategies that produced Ops.

      Minimal_Complete_State : LR.Minimal_Complete_State := None;
      Matching_Begin_Done    : Boolean                   := False;
   end record;

   function Key (A : in Configuration) return Integer is (A.Cost);

   procedure Set_Key (Item : in out Configuration; Key : in Integer);

   package Config_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci
     (Element_Type   => Configuration,
      Key_Type       => Integer,
      Key            => Key,
      Set_Key        => Set_Key);

   type Check_Status is (Success, Abandon, Continue);
   subtype Non_Success_Status is Check_Status range Abandon .. Continue;

   type McKenzie_Data is tagged record
      Config_Heap       : Config_Heaps.Heap_Type;
      Enqueue_Count     : Integer := 0;
      Config_Full_Count : Integer := 0;
      Check_Count       : Integer := 0;
      Results           : Config_Heaps.Heap_Type;
      Success           : Boolean := False;
   end record;
   type McKenzie_Access is access all McKenzie_Data;

   procedure Accumulate (Data : in McKenzie_Data; Counts : in out Strategy_Counts);
   --  Sum Results.Strategy_Counts.

end WisiToken.Parse.LR;
