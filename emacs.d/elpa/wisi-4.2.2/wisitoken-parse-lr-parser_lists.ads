--  Abstract :
--
--  Generalized LR parser state.
--
--  Copyright (C) 2014-2015, 2017 - 2022 Free Software Foundation, Inc.
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

with Ada.Iterator_Interfaces;
with SAL.Gen_Indefinite_Doubly_Linked_Lists;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR.Parser_Lists is
   use all type WisiToken.Syntax_Trees.Stream_ID;
   use all type WisiToken.Syntax_Trees.Stream_Error_Ref;

   function Parser_Stack_Image
     (Stack : in Syntax_Trees.Stream_ID;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
     return String;
   --  If Depth = 0, put all of Stack. Otherwise put Min (Depth,
   --  Stack.Depth) items.
   --
   --  Unique name for calling from debugger

   function Image
     (Stack : in Syntax_Trees.Stream_ID;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
     return String renames Parser_Stack_Image;

   type Base_Parser_State is tagged
   record
      --  Visible components for direct access
      --
      --  The parse stack is in Shared_Parser.Tree (Parser_State.Stream).

      Recover : aliased LR.McKenzie_Data := (others => <>);

      Recover_Insert_Delete : aliased Syntax_Trees.Valid_Node_Access_Lists.List;
      --  List of nodes containing errors that contain recover operations;
      --  tokens that were inserted or deleted during error recovery. Filled
      --  by error recover, used by Execute_Actions for
      --  User_Data.Insert_Token, .Delete_Token.
      --
      --  Not emptied between error recovery sessions, so Execute_Actions
      --  knows about all insert/delete.

      Total_Recover_Cost     : Integer                   := 0;
      Max_Recover_Ops_Length : Ada.Containers.Count_Type := 0;
      Error_Count            : Integer                   := 0;

      Zombie_Token_Count : Integer := 0;
      --  If Zombie_Token_Count > 0, this parser has errored, but is waiting
      --  to see if other parsers do also.

      Resume_Active : Boolean := False;

      Resume_Token_Goal : Syntax_Trees.Base_Sequential_Index := Syntax_Trees.Invalid_Sequential_Index;
      --  Set at the end of recovery, so during recovery it is the end of
      --  the previous recover session.

      Conflict_During_Resume : Boolean := False;

      Last_Action : Parse_Action_Rec := (others => <>);
   end record;

   type Parser_State is new Base_Parser_State with private;
   type State_Access is access all Parser_State;

   function Recover_Image
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree;
      Current_Only : in     Boolean := False)
     return String;

   function Current_Recover_Op (Parser_State : in Parser_Lists.Parser_State) return SAL.Base_Peek_Type;
   --  Index into Parser_State.Current_Error_Ref recover_ops;
   --  No_Insert_Delete if no current error (all ops done).

   procedure Set_Current_Error_Features
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree);
   --  Record Syntax_Trees.Error_Node_Features of
   --  Parser_State.Current_Error_Ref (called with default Features) to
   --  enable Current_Error_Ref to find it again while recover ops are
   --  processed.

   procedure Clear_Current_Error_Features
     (Parser_State : in out Parser_Lists.Parser_State);
   --  Reset to default, ready for a new error recover session.

   function Current_Error_Ref
     (Parser_State : in Parser_Lists.Parser_State;
      Tree         : in Syntax_Trees.Tree)
     return Syntax_Trees.Stream_Error_Ref
   with Post => Current_Error_Ref'Result /= Syntax_Trees.Invalid_Stream_Error_Ref;
   --  Must only be called when Parser_State has an error; return current
   --  error node. If Set_Current_Error_Features has been called, uses the
   --  recorded Error_Node_Features.

   procedure Do_Delete
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in out Syntax_Trees.Tree;
      Op           : in out Delete_Op_Nodes;
      User_Data    : in     Syntax_Trees.User_Data_Access_Constant);
   --  Perform Delete operation on Stream, set Op.Del_Node to
   --  deleted node. Update Parser_State.Current_Error_Features if deleted node =
   --  error node.

   procedure Undo_Reduce
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in out Syntax_Trees.Tree;
      Table        : in     Parse_Table;
      User_Data    : in     Syntax_Trees.User_Data_Access_Constant);
   --  Undo reduction of nonterm at Parser_State.Stream.Stack_Top; Stack_Top is then
   --  the last Child of the nonterm.
   --
   --  If Stream.Stack_Top has an error, it is moved to the first
   --  terminal; if that error is the current error, update
   --  Parser_State.Current_Error_Features.
   --
   --  Duplicates LR.Undo_Reduce; that is used by Edit_Tree, when there
   --  is no Parser_State.

   procedure First_Recover_Op (Parser_State : in out Parser_Lists.Parser_State);
   --  Set Parser_State.Current_Recover_Op to 1, indicating that there
   --  are insert/delete operations in the current error.

   procedure Next_Recover_Op
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree);
   --  Increment Parser_State.Current_Recover_Op.

   procedure Update_Error
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in out Syntax_Trees.Tree;
      Data         : in     Syntax_Trees.Error_Data'Class;
      User_Data    : in     Syntax_Trees.User_Data_Access_Constant);
   --  Update current error with Data. If Parser_State.Current_Recover_Op
   --  is the last op in the current error, append the current error node
   --  to Parser_State.Recover_Insert_Delete, and reset
   --  Parser_State.Current_Recover_Op.

   function Peek_Current_Sequential_Terminal
     (Parser_State : in Parser_Lists.Parser_State;
      Tree         : in Syntax_Trees.Tree)
     return Syntax_Trees.Terminal_Ref;
   --  Return first terminal with a valid Sequential_Index from current
   --  token or a following token if current is an empty nonterm. For
   --  comparison with insert/delete token index.

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Parser_State;

   function New_List (Tree : in out Syntax_Trees.Tree) return List
   with Pre => Tree.Parseable;
   --  Create the first parse stream in Tree.

   procedure Clear (List : in out Parser_Lists.List);
   --  Empty list.

   function Count (List : in Parser_Lists.List) return SAL.Base_Peek_Type;

   type Cursor (<>) is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Has_Element (Cursor : in Parser_Lists.Cursor) return Boolean is (not Is_Done (Cursor));
   function Stream (Cursor : in Parser_Lists.Cursor) return Syntax_Trees.Stream_ID;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs;

   procedure Terminate_Parser
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in out Syntax_Trees.Tree;
      Message : in     String;
      Trace   : in out WisiToken.Trace'Class);
   --  Terminate Current. Current is set to next element.
   --
   --  Tree is used to report the current token in the message.

   procedure Duplicate_State
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in out Syntax_Trees.Tree;
      Trace   : in out WisiToken.Trace'Class);
   --  If any other parser in Parsers has a stack equivalent to Current,
   --  Terminate one of them. Current is either unchanged, or advanced to
   --  the next parser.
   --
   --  Terminals is used to report the current token in the message.

   type State_Reference (Element : not null access Parser_State) is null record
   with Implicit_Dereference => Element;

   function State_Ref (Position : in Cursor) return State_Reference
   with Pre => Has_Element (Position);
   --  Direct access to visible components of Parser_State

   function First_State_Ref (List : in Parser_Lists.List'Class) return State_Reference
   with Pre => List.Count > 0;
   --  Direct access to visible components of first parser's Parser_State

   type Constant_State_Reference (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function First_Constant_State_Ref (List : in Parser_Lists.List'Class) return Constant_State_Reference
   with Pre => List.Count > 0;
   --  Direct access to visible components of first parser's Parser_State

   procedure Prepend_Copy
     (List      : in out Parser_Lists.List;
      Cursor    : in     Parser_Lists.Cursor'Class;
      Tree      : in out Syntax_Trees.Tree;
      User_Data : in     Syntax_Trees.User_Data_Access_Constant;
      Trace     : in out WisiToken.Trace'Class);
   --  Copy parser at Cursor, prepend to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).
   --
   --  Copy.Recover is set to default.

   ----------
   --  Stuff for iterators, to allow
   --  'for Parser of Parsers loop'
   --  'for I in Parsers.Iterate loop'
   --
   --  requires Parser_State to be not an incomplete type.

   --  We'd like to use Cursor here, but we want that to be tagged, to
   --  allow 'Cursor.operation' syntax, and the requirements of
   --  iterators prevent a tagged iterator type (two tagged types on
   --  First in this package body). So we use Parser_Node_Access as
   --  the iterator type for Iterators, and typical usage is:
   --
   --  for I in Parsers.Iterate loop
   --     declare
   --        Cursor : Parser_Lists.Cursor renames To_Cursor (Parsers, I);
   --     begin
   --        Cursor.<cursor operation>
   --
   --        ... Parsers (I).<visible parser_state component> ...
   --     end;
   --  end loop;
   --
   --  or:
   --  for Current_Parser of Parsers loop
   --     ... Current_Parser.<visible parser_state component> ...
   --  end loop;

   type Parser_Node_Access (<>) is private;

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor;
   function To_Parser_Node_Access (Cur : in Cursor) return Parser_Node_Access;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference;
   pragma Inline (Reference);

   function Unchecked_State_Ref (Position : in Parser_Node_Access) return State_Access;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class;

   --  Access to private Parser_State components

   function Stream (State : in Parser_State) return Syntax_Trees.Stream_ID;
   procedure Set_Verb (State : in out Parser_State; Verb : in All_Parse_Action_Verbs);
   function Verb (State : in Parser_State) return All_Parse_Action_Verbs;

   procedure Clear_Stream (State : in out Parser_State);
   --  Clear all references to Syntax_Tree streams, so the tree can be
   --  finalized.

private

   type Parser_State is new Base_Parser_State with record

      Current_Recover_Op : SAL.Base_Peek_Type := No_Insert_Delete;
      --  Next op in Parser_State.Current_Error_Ref.Error.Recover_Ops to be
      --  processed by main parse; No_Insert_Delete if all done.
      --
      --  We do not keep a copy of Parser_State.Current_Error_Ref, because
      --  the main parser can change the Stream_Node_Ref by shift or reduce.

      Current_Error_Features : Syntax_Trees.Error_Node_Features;

      Stream : Syntax_Trees.Stream_ID;

      Verb : All_Parse_Action_Verbs := Shift; -- current action to perform
   end record;

   package Parser_State_Lists is new SAL.Gen_Indefinite_Doubly_Linked_Lists (Parser_State);

   type List is tagged record
      Elements : aliased Parser_State_Lists.List;
   end record;

   type Cursor is tagged record
      Ptr : Parser_State_Lists.Cursor;
   end record;

   type Parser_Node_Access is record
      Ptr : Parser_State_Lists.Cursor;
   end record;

end WisiToken.Parse.LR.Parser_Lists;
