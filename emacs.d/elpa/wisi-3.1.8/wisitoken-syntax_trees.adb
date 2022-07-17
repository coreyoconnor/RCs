--  Abstract :
--
--  See spec.
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

with Ada.Containers;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Maps;
with Ada.Tags;
with GNAT.Traceback.Symbolic;
package body WisiToken.Syntax_Trees is

   --  Body specs, alphabetical, as needed

   type Visit_Parent_Mode is (Before, After);

   function Append_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     Unknown_State_Index)
     return Terminal_Ref
   with Post => Tree.Valid_Terminal (Append_Stream_Element'Result);
   --  Add Node at Stream.Last; if not Shared_Stream, set Stack_Top to
   --  element containing Node. If Node is from Shared_Stream, it has
   --  been copied

   function Child_Index (Parent : in Node; Child : in Valid_Node_Access) return SAL.Peek_Type;

   procedure Copy_Ancestors
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      New_Node  : in     Valid_Node_Access;
      User_Data : in     User_Data_Access_Constant);
   --  New_Node is a copy of Ref.Ref.Node; copy all ancestors, updating
   --  child links.

   function Copy_Node
     (Tree                   : in out Syntax_Trees.Tree;
      Node                   : in     Valid_Node_Access;
      Parent                 : in     Node_Access;
      User_Data              : in     User_Data_Access_Constant;
      Copy_Children          : in     Boolean;
      Copy_Following_Deleted : in     Boolean;
      New_Error_List         : in     Error_List_Access := null;
      Set_Error_List         : in     Boolean           := False;
      Set_Copied_Node        : in     Boolean           := False)
     return Valid_Node_Access
   with Pre => (if Copy_Children then Tree.Parents_Set);
   --  If Set_Error_List is False, new node has copy of Node.Error_List.
   --  Otherwise, new node has New_Error_List.

   function Find_Match
     (Error_List : in Error_List_Access;
      Predicate  : in Error_Predicate)
     return Error_Data_Lists.Cursor;

   procedure First_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean)
   with Pre => Rooted (Ref.Ref) and Ref.Parents.Depth = 0;
   --  Update Ref to first source terminal in Ref.Node, initialize Ref.Parents.

   procedure Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element);
   --  If Before is No_Element, add Node after Stream.Stack_Top (at
   --  beginning of input). Otherwise add Node before Before.
   --
   --  Caller must change Stream.Stack_Top if necessary.

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Rooted_Ref;
   --  Same as procedure, return new element.

   function Is_Optimized_List
     (Productions : in Production_Info_Trees.Vector;
      ID          : in Token_ID)
     return Boolean;

   function Last_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;

   procedure Last_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean)
   with Pre => Rooted (Ref.Ref) and Ref.Parents.Depth = 0;
   --  Update Ref to last source terminal in Ref.Node, initialize Ref.Parents.

   procedure Move_Element
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Ref       : in out Stream_Node_Parents;
      New_Node  : in     Valid_Node_Access;
      User_Data : in     User_Data_Access_Constant);
   --  Move Ref to Stream, replacing Ref.Node with New_Node,
   --  copying all ancestors. Update Ref to point to new stream element
   --  with copied nodes.

   procedure Next_Node (Node : in out Node_Access);
   --  Assumes Tree.Parents_Set.

   procedure Next_Node (Tree : in Syntax_Trees.Tree; Node : in out Stream_Node_Parents)
   with Pre => Node.Ref.Node /= Invalid_Node_Access;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Ref);

   procedure Next_Nonterm (Tree : in Syntax_Trees.Tree; Ref : in out Stream_Node_Ref);

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean)
     return Stream_Node_Ref;

   procedure Next_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean);

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;

   procedure Prev_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack);

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Access;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Access)
        return Boolean)
     return Boolean;
   --  Call Process_Node on nodes in tree rooted at Node. Return when
   --  Process_Node returns False (Process_Tree returns False), or when
   --  all nodes have been processed (Process_Tree returns True).

   procedure Replace_Node (Element : in Stream_Index; New_Node : in Valid_Node_Access);

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in out Valid_Node_Access;
      Children : in     Node_Access_Array)
   with Pre => Tree.Parents_Set;

   function Subtree_Image
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Node_Access;
      Node_Numbers : in Boolean := True;
      Non_Grammar  : in Boolean := False;
      Augmented    : in Boolean := False;
      Line_Numbers : in Boolean := False;
      Level        : in Integer := 0)
     return String;

   ----------
   --  Public and body operations, alphabetical

   procedure Add_Deleted
     (Tree          : in out Syntax_Trees.Tree;
      Deleted_Node  : in     Valid_Node_Access;
      Prev_Terminal : in out Stream_Node_Parents;
      User_Data     : in     User_Data_Access_Constant)
   is
      --  We need to copy Prev_Terminal.Node, and replace any links to it.
      --  It is tempting to attempt to optimize this; if no parsers have
      --  spawned since Following_Deleted was last edited, we don't need to
      --  copy Prev_Terminal.Ref.Node again. But that would require a
      --  reference count on Prev_Terminal.Ref.Node, which would be
      --  non-incremental in spawn and terminate parser.

      New_Node : constant Valid_Node_Access := Copy_Node
        (Tree, Prev_Terminal.Ref.Node,
         Parent        =>
           (if Tree.Parents_Set
            then Prev_Terminal.Ref.Node
            else Invalid_Node_Access),
         User_Data              => User_Data,
         Copy_Children          => False,
         Copy_Following_Deleted => True);
   begin
      if Prev_Terminal.Parents.Depth = 0 then
         pragma Assert (Rooted (Prev_Terminal.Ref));
         --  There are no child links yet
         Replace_Node (Prev_Terminal.Ref.Element, New_Node);

         Prev_Terminal.Ref.Node := New_Node;

      else
         --  Need to edit child link, which requires copying parent node, up to
         --  the element root.
         Copy_Ancestors (Tree, Prev_Terminal, New_Node, User_Data);
      end if;

      Prev_Terminal.Ref.Node.Following_Deleted.Append (Deleted_Node);

      --  We need to move the non_grammar now, so they are correct for later
      --  error recover sessions. test_incremental.adb : Edit_String_10

      if Trace_Action > WisiToken.Outline then
         --  Trace_Action to be consistent with messages from user Delete_Token.
         Tree.Lexer.Trace.Put_Line
           ("delete token " & Tree.Image (Deleted_Node, Node_Numbers => True, Non_Grammar => True));
         if Deleted_Node.Non_Grammar.Length > 0 then
            Tree.Lexer.Trace.Put_Line
              (" ... move non_grammar to " & Tree.Image
                 (Prev_Terminal.Ref.Node, Node_Numbers => True, Non_Grammar => True));
         end if;
      end if;

      Prev_Terminal.Ref.Node.Non_Grammar.Append (Deleted_Node.Non_Grammar);

      --  We don't do Deleted_Node.Non_Grammar.Clear here; we are not
      --  editing the shared stream. That is done in Finish_Parse.
   end Add_Deleted;

   procedure Add_Errors
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Errors : in     Error_Data_Lists.List)
   is
      pragma Unreferenced (Stream); --  Only used in precondition.
   begin
      if Node.Error_List = null then
         Node.Error_List := new Error_Data_Lists.List'(Errors);
      else
         for Err of Errors loop
            Node.Error_List.Append (Err);
         end loop;
      end if;
   end Add_Errors;

   function Add_Error
     (Tree      : in out Syntax_Trees.Tree;
      Node      : in     Valid_Node_Access;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access_Constant)
     return Valid_Node_Access
   --  Copy Node, adding Data to its error list. Return new node.
   is
      function Copy_Errors return Error_List_Access
      is begin
         if Node.Error_List = null then
            return new Error_Data_Lists.List'(Error_Data_Lists.To_List (Data));
         else
            return Result : constant Error_List_Access := new Error_Data_Lists.List'(Node.Error_List.all)
            do
               Result.Append (Data);
            end return;
         end if;
      end Copy_Errors;
   begin
      return Copy_Node
        (Tree, Node,
         Parent         =>
           (if Tree.Parents_Set
            then Node.Parent
            else Invalid_Node_Access),
         User_Data              => User_Data,
         Copy_Children          => False,
         Copy_Following_Deleted => True,
         New_Error_List         => Copy_Errors,
         Set_Error_List         => True);
   end Add_Error;

   function Add_Errors
     (Tree      : in out Syntax_Trees.Tree;
      Node      : in     Valid_Node_Access;
      Errors    : in     Error_Data_Lists.List;
      User_Data : in     User_Data_Access_Constant)
     return Valid_Node_Access
   --  Copy Node, adding Errors to its error list. Return new node.
   is
      function Copy_Errors return Error_List_Access
      is begin
         if Node.Error_List = null then
            return new Error_Data_Lists.List'(Errors);
         else
            return Result : constant Error_List_Access := new Error_Data_Lists.List'(Node.Error_List.all)
            do
               for Error of Errors loop
                  Result.Append (Error);
               end loop;
            end return;
         end if;
      end Copy_Errors;
   begin
      return Copy_Node
        (Tree, Node,
         Parent         =>
           (if Tree.Parents_Set
            then Node.Parent
            else Invalid_Node_Access),
         User_Data              => User_Data,
         Copy_Children          => False,
         Copy_Following_Deleted => True,
         New_Error_List         => Copy_Errors,
         Set_Error_List         => True);
   end Add_Errors;

   procedure Add_Error_To_Input
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access_Constant)
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Error_Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last
         then (Tree.Shared_Stream,
               (Cur => Parse_Stream.Shared_Link),
               Element (Parse_Stream.Shared_Link).Node)
         else (Stream,
               (Cur => Next (Parse_Stream.Stack_Top)),
               Element (Next (Parse_Stream.Stack_Top)).Node));
   begin
      Tree.First_Terminal (Error_Ref, Following => False);

      Move_Element
        (Tree, Stream, Error_Ref, Add_Error (Tree, Error_Ref.Ref.Node, Data, User_Data), User_Data);

      if Parse_Stream.Elements.Last = Parse_Stream.Stack_Top then
         Next (Parse_Stream.Shared_Link);
      end if;
   end Add_Error_To_Input;

   procedure Add_Error_To_Stack_Top
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access_Constant)
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Orig : Stream_Element := Element (Parse_Stream.Stack_Top);
   begin
      Orig.Node := Add_Error
        (Tree, Element (Parse_Stream.Stack_Top).Node, Data, User_Data);
      Replace_Element (Parse_Stream.Stack_Top, Orig);
   end Add_Error_To_Stack_Top;

   procedure Add_Errors
     (Tree      : in out Syntax_Trees.Tree;
      Error_Ref : in out Stream_Node_Parents;
      Errors    : in     Error_Data_Lists.List;
      User_Data : in     User_Data_Access_Constant)
   is
      function Copy_Errors return Error_List_Access
      is begin
         if Error_Ref.Ref.Node.Error_List = null then
            return new Error_Data_Lists.List'(Errors);
         else
            return Result : constant Error_List_Access := new Error_Data_Lists.List'(Error_Ref.Ref.Node.Error_List.all)
            do
               for Err of Errors loop
                  Result.Append (Err);
               end loop;
            end return;
         end if;
      end Copy_Errors;

      New_Node : constant Valid_Node_Access := Copy_Node
        (Tree, Error_Ref.Ref.Node,
         Parent         =>
           (if Tree.Parents_Set
            then Error_Ref.Ref.Node.Parent
            else Invalid_Node_Access),
         User_Data              => User_Data,
         Copy_Children          => False,
         New_Error_List         => Copy_Errors,
         Copy_Following_Deleted => True,
         Set_Error_List         => True);
   begin
      Copy_Ancestors (Tree, Error_Ref, New_Node, User_Data);
   end Add_Errors;

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Identifier,
         Child_Count => 0,
         ID          => ID,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         Identifier  => Identifier,
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Identifier;

   function Add_Nonterm_1
     (Tree             : in out Syntax_Trees.Tree;
      Production       : in     WisiToken.Production_ID;
      Children         : in     Valid_Node_Access_Array;
      Clear_Parents    : in     Boolean;
      Recover_Conflict : in     Boolean)
     return Valid_Node_Access
   is
      Nonterm_Node : constant Valid_Node_Access := new Node'
        (Label            => Syntax_Trees.Nonterm,
         Child_Count      => Children'Last,
         ID               => Production.LHS,
         Node_Index       => -(Tree.Nodes.Last_Index + 1),
         Children         => To_Node_Access (Children),
         RHS_Index        => Production.RHS,
         Virtual          => False,
         Recover_Conflict => Recover_Conflict,
         others           => <>);
   begin
      Tree.Nodes.Append (Nonterm_Node);

      for Child of Children loop
         case Child.Label is
         when Source_Terminal | Virtual_Identifier =>
            null;

         when Virtual_Terminal =>
            Nonterm_Node.Virtual := True;

         when Nonterm =>
            if Child.Virtual then
               Nonterm_Node.Virtual := True;
            end if;
         end case;

         if Child.Parent /= Invalid_Node_Access then
            if Clear_Parents then
               declare
                  Other_Parent : constant Node_Access := Child.Parent;
                  Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                    (Other_Parent.all, Child);
               begin
                  Other_Parent.Children (Child_Index) := Invalid_Node_Access;
               end;
            else
               raise SAL.Programmer_Error with "attempt to use children with existing parents";
            end if;
         end if;

         if Tree.Parents_Set then
            Child.Parent := Nonterm_Node;
         end if;
      end loop;

      return Nonterm_Node;
   end Add_Nonterm_1;

   function Add_Nonterm
     (Tree          : in out Syntax_Trees.Tree;
      Production    : in     WisiToken.Production_ID;
      Children      : in     Valid_Node_Access_Array;
      Clear_Parents : in     Boolean)
     return Valid_Node_Access
   is begin
      return Add_Nonterm_1 (Tree, Production, Children, Clear_Parents, Recover_Conflict => False);
   end Add_Nonterm;

   function Add_Source_Terminal_1
     (Tree             : in out Syntax_Trees.Tree;
      Terminal         : in     WisiToken.Lexer.Token;
      In_Shared_Stream : in     Boolean;
      Errors           : in     Error_Data_Lists.List)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Source_Terminal,
         Child_Count => 0,
         ID          => Terminal.ID,

         Node_Index  =>
           (if In_Shared_Stream
            then Tree.Next_Terminal_Node_Index
            else -(Tree.Nodes.Last_Index + 1)),

         Byte_Region    => Terminal.Byte_Region,
         Char_Region    => Terminal.Char_Region,
         New_Line_Count => New_Line_Count (Terminal.Line_Region),
         Error_List     =>
           (if Errors.Length = 0
            then null
            else new Error_Data_Lists.List'(Errors)),
         others      => <>)
      do
         if Terminal.ID = Tree.Lexer.Descriptor.EOI_ID then
            pragma Assert (In_Shared_Stream);
            Tree.EOI := Result;
            Result.Non_Grammar.Append (Terminal);
         end if;
         if In_Shared_Stream then
            Tree.Next_Terminal_Node_Index := @ + 1;
         end if;
         Tree.Nodes.Append (Result);
      end return;
   end Add_Source_Terminal_1;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Lexer.Token;
      Errors   : in     Error_Data_Lists.List)
     return Single_Terminal_Ref
   is begin
      return Append_Stream_Element
        (Tree, Stream,
         Add_Source_Terminal_1
           (Tree, Terminal,
            In_Shared_Stream => Stream = Tree.Shared_Stream,
            Errors           => Errors),
         State               => Unknown_State);
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Lexer.Token;
      Errors   : in     Error_Data_Lists.List)
     return Valid_Node_Access
   is begin
      return Add_Source_Terminal_1 (Tree, Terminal, In_Shared_Stream => False, Errors => Errors);
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Terminal;

   function Append_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     Unknown_State_Index)
     return Terminal_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Stream_Element_Lists.Cursor := Parse_Stream.Elements.Append
        ((Node  => Node,
          State => State));
   begin
      if Stream = Tree.Shared_Stream then
         --  Stack_Top is always Invalid_Stream_Element.
         null;
      else
         Parse_Stream.Stack_Top := New_Element;
      end if;

      return (Stream, (Cur => New_Element), Node);
   end Append_Stream_Element;

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access
   is begin
      return Node.Augmented;
   end Augmented;

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access_Constant
   is begin
      return Augmented_Class_Access_Constant (Node.Augmented);
   end Augmented_Const;

   procedure Breakdown
     (Tree           : in out Syntax_Trees.Tree;
      Ref            : in out Stream_Node_Parents;
      Productions    : in     Production_Info_Trees.Vector;
      User_Data      : in     Syntax_Trees.User_Data_Access_Constant;
      First_Terminal : in     Boolean)
   is
      use Stream_Element_Lists;

      Stream    : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Ref.Stream.Cur);
      Cur       : Cursor                    renames Ref.Ref.Element.Cur;
      Target    : Valid_Node_Access         renames Ref.Ref.Node;
      To_Delete : Cursor                    := Cur;

      Inverted_Parents : Node_Stacks.Stack := Ref.Parents.Invert;
      --  Parents from Target to Cur. Inverted_Parents.Peek (1) is
      --  Ref.Ref.Element.Node, and Inverted_Parents.Peek
      --  (Inverted_Parents.Depth) is Target parent.
      --
      --  As Breakdown proceeds, Inverted_Parents is popped;
      --  Inverted_Parents.Peek (1) is always the stream node that is the
      --  ancestor of Target.

      procedure Move_Errors
      --  Move errors on To_Delete.Element.Node to first terminal. Update
      --  Inverted_Parents.
      is
         Node       : constant Valid_Node_Access := Element (To_Delete).Node;
         New_Errors : Error_Data_Lists.List;
      begin
         if Node.Error_List = null or else Node.Error_List.Length = 0 then
            return;
         else
            for Err of Node.Error_List.all loop
               New_Errors.Append (To_Message (Err, Tree, Node));
            end loop;

            --  At this point, To_Delete is still in the stream; the children of
            --  To_Delete have been pushed on the stream before it; Cur is the
            --  first of those children. Find the terminal to move errors to, and
            --  the new stream element containing it.
            declare
               First_Terminal      : Stream_Node_Parents := Tree.To_Stream_Node_Parents
                 (Tree.To_Rooted_Ref (Ref.Ref.Stream, (Cur => To_Delete)));
               Modify_Element_Node : Node_Access;
               Modify_Element      : Cursor              := Cur;
            begin
               Tree.First_Terminal (First_Terminal, Following => False);

               --  We know To_Delete has at least one terminal, because we don't put
               --  errors on empty nonterms, we just delete them.
               pragma Assert (First_Terminal.Ref.Node /= Invalid_Node_Access);

               --  We are deleting To_Delete; don't include it in
               --  First_Terminal.Parents, to avoid an unnecessary copy.
               First_Terminal.Parents.Bottom_Pop;
               if First_Terminal.Parents.Depth = 0 then
                  --  ada_mode-recover_debbugs_36548.adb
                  Modify_Element_Node := First_Terminal.Ref.Node;
               else
                  Modify_Element_Node := First_Terminal.Parents.Peek (First_Terminal.Parents.Depth);
               end if;
               loop
                  exit when Element (Modify_Element).Node = Modify_Element_Node;
                  Next (Modify_Element);
               end loop;
               First_Terminal.Ref.Element := (Cur => Modify_Element);

               declare
                  Update_Target : constant Boolean := First_Terminal.Ref.Node = Target;

                  --  Inverted_Parents holds the path from To_Delete to Target; if
                  --  Add_Errors copies any of those nodes, update the Inverted_Parents
                  --  value.
                  Update_Parents_Index         : SAL.Base_Peek_Type := 0;
                  Inverted_Parents_Index       : SAL.Base_Peek_Type := 1;
                  First_Terminal_Parents_Index : SAL.Base_Peek_Type := First_Terminal.Parents.Depth;
               begin
                  loop
                     exit when First_Terminal_Parents_Index = 0 or Inverted_Parents_Index > Inverted_Parents.Depth;
                     exit when Inverted_Parents.Peek (Inverted_Parents_Index) /=
                       First_Terminal.Parents.Peek (First_Terminal_Parents_Index);
                     Update_Parents_Index := Inverted_Parents_Index;
                     Inverted_Parents_Index := @ + 1;
                     First_Terminal_Parents_Index := @ - 1;
                  end loop;

                  Tree.Add_Errors (First_Terminal, New_Errors, User_Data);

                  for I in 1 .. Update_Parents_Index loop
                     Inverted_Parents.Set
                       (I, Inverted_Parents.Depth, First_Terminal.Parents.Peek (First_Terminal.Parents.Depth - I + 1));
                  end loop;
                  if Update_Target then
                     Target           := First_Terminal.Ref.Node;
                  end if;
               end;
            end;
         end if;
      end Move_Errors;
   begin
      Undo_Reduce_Loop :
      loop
         declare
            Stream_Node : constant Valid_Node_Access := Element (Cur).Node;
         begin
            exit Undo_Reduce_Loop when
              (if First_Terminal
               then Tree.First_Terminal (Stream_Node) = Target
               else Stream_Node = Target);

            pragma Assert (Stream_Node.Label = Nonterm);
            pragma Assert (Stream_Node.Child_Count > 0); -- otherwise we would not get here.

            if Is_Optimized_List (Productions, Stream_Node.ID) and Stream_Node.RHS_Index = 1 then
               --  Split list at list ancestor of Ref.Node
               --
               --  From test_syntax_trees.adb Breakdown_Optimized_List_01, stream looks
               --  like:
               --
               --  <prev_stream_element>
               --
               --  -32:declarations_1
               --  | -30:declarations_1         Split_Node parent 2
               --  | | -28:declarations_1       Split_Node parent 1
               --  | | | -26:declarations_1
               --  | | | | -24:declarations_0
               --  | | | | | -23:declaration_0
               --  | | | | | | <a : A;>
               --  | | | | -25:declaration_0
               --  | | | | | <b : B;>
               --  | | | -27:declaration_0      Split_Node
               --  | | | | <c : C;>
               --  | | -29:declaration_0
               --  | | | <d : D;>
               --  | -31:declaration_0
               --  | | <e : E;>
               --
               --  <next_stream_element>
               --
               --  Stream_Node is node -32; the top declarations node with RHS_Index
               --  1.
               --
               --  Note that if this list has been edited before, it may have top
               --  nodes with RHS_Index 2; each of those declarations node has two
               --  declarations children, and are already optimized, so they are
               --  broken down the same as non-list nodes. Other cases leave
               --  RHS_Index 2 in lower nodes; test_incremental.adb Recover_08*.
               declare
                  List_ID          : constant Token_ID  := Stream_Node.ID;
                  Target_Anc_Index : SAL.Base_Peek_Type := 1;
                  Split_Node       : Node_Access;

                  Insert_Following : constant Stream_Element_Lists.Cursor := Next (Cur);
                  --  Insert nodes following Split_Node before this.

                  Insert_Leading : Stream_Element_Lists.Cursor := Cur;
                  --  Insert nodes preceding Split_node before this.

                  procedure Find_Target_Element
                  --  Update Cur to stream element at or after Cur containing Target
                  is
                     Target_Anc_Node : constant Valid_Node_Access :=
                       (if Target_Anc_Index = Inverted_Parents.Depth
                        then Target
                        else Inverted_Parents.Peek (Target_Anc_Index + 1));
                  begin
                     Find_Element :
                     loop
                        exit Find_Element when Element (Cur).Node = Target_Anc_Node;
                        Next (Cur);
                     end loop Find_Element;
                  end Find_Target_Element;

                  procedure Insert_Children (Node : in Valid_Node_Access)
                  --  Insert Node.Children (2 .. Node.Child_Count) into Stream before
                  --  Insert_Following.
                  --
                  --  There is more than one child when the list has a separator
                  --  (optimized_list_ebnf term, ada_annex_p
                  --  parameter_specification_list,
                  --  ada_mode-incremental_recover_02.adb), or multiple list elements
                  --  (ada_lite_ebnf case_statement_alternative).
                  is begin
                     for I in 2 .. Node.Child_Count loop
                        if I = 2 then
                           Insert_Leading :=
                             Stream.Elements.Insert (Insert_Following, (Node.Children (I), Unknown_State));
                        else
                           Stream.Elements.Insert (Insert_Following, (Node.Children (I), Unknown_State));
                        end if;
                        Node.Children (I).Parent := Invalid_Node_Access;
                     end loop;
                  end Insert_Children;
               begin
                  --  Find Split_Node, the first ancestor of Target that is a
                  --  declarations. Start from the tree root, to handle nested lists;
                  --  test_incremental.adb Recover_07.
                  declare
                     Temp : SAL.Base_Peek_Type := 2;
                  begin
                     loop
                        exit when Temp > Inverted_Parents.Depth;
                        exit when Inverted_Parents.Peek (Temp).ID /= List_ID;
                        Target_Anc_Index := Temp;
                        Temp := @ + 1;
                     end loop;
                  end;
                  Split_Node := Inverted_Parents.Peek (Target_Anc_Index);

                  --  Bring Split_Node to the stream, with previous and following list
                  --  nodes each under a single stream element.
                  --
                  --  Suppose Split_Node is node -27 (test_syntax_trees.adb
                  --  Breakdown_Optimized_List_01 case "c"). The desired stream is:
                  --
                  --  <prev_stream_element>
                  --
                  --  -26:declarations_1
                  --  | -24:declarations_0
                  --  | | -23:declaration_0
                  --  | | | <a : A;>
                  --  | -25:declaration_0
                  --  | | <b : B;>
                  --
                  --  -27:declaration_0       Split_Node
                  --  | <c : C;>
                  --
                  --  -35:declarations_1      New node
                  --  | -34:declarations_0    New node
                  --  | | | -29:declaration_0
                  --  | | | | <d : D;>
                  --  | | -31:declaration_0
                  --  | | | <e : E;>
                  --
                  --  <next_stream_element>
                  --
                  --  The other test cases in Breakdown_Optimized_List_01 test special
                  --  cases where Split_Node is near or at the beginning or end of the
                  --  list.

                  if Target.ID = List_ID or
                     --  The target is a list node in the list we are breaking down.
                     --  test_incremental.adb Recover_06a .. e.

                    Split_Node.RHS_Index = 0
                     --  There are no list elements preceding Split_Node; handle separator,
                     --  multi-element list element. test_incremental.adb Edit_Code_18
                  then
                     Replace_Element (Cur, (Split_Node.Children (1), Unknown_State));
                     Insert_Children (Split_Node);
                     Find_Target_Element;

                  elsif Split_Node.Children (1).RHS_Index = 0 then
                     --  There is one list element preceding Split_Node
                     Replace_Element (Cur, (Split_Node.Children (1).Children (1), Unknown_State));
                     Insert_Children (Split_Node.Children (1));
                     Split_Node.Children (1).Children (1).Parent := Invalid_Node_Access;
                     Insert_Children (Split_Node);
                     Find_Target_Element;

                  else
                     --  There are more than one list elements preceding Split_Node
                     Replace_Element (Cur, (Split_Node.Children (1), Unknown_State));
                     Insert_Children (Split_Node);
                     Find_Target_Element;
                  end if;

                  Split_Node.Children (1).Parent := Invalid_Node_Access;
                  Split_Node.Parent := Invalid_Node_Access;

                  if Target_Anc_Index - 1 = 0 then
                     --  There are no list elements following Split_node
                     null;

                  elsif Target_Anc_Index - 1 = 1 then
                     --  There is only one list element following Split_Node
                     --  test_syntax_trees.adb Breakdown_Optimized_List_01 case d1
                     --
                     --  Or Deleting_Node is an RHS_Index = 2 node, and all of the list
                     --  elements following Split_Node are already under an RHS_Index = 1
                     --  node; test_incremental.adb Recover_08a.
                     declare
                        Deleting_Node : constant Valid_Node_Access := Inverted_Parents.Pop;
                     begin
                        if Deleting_Node.Error_List /= null then
                           --  FIXME: Move errors. need test case
                           raise SAL.Not_Implemented with "error on optimized_list";
                        end if;

                        Insert_Children (Deleting_Node);

                        Deleting_Node.Parent := Invalid_Node_Access;
                        Deleting_Node.Children (1).Parent := Invalid_Node_Access;
                     end;

                  else
                     --  Multiple list elements following Split_Node
                     --
                     --  Split_Node children are now in the stream. Build a new list node
                     --  with content of following nodes, insert it before Insert_Following.
                     declare
                        Following_Node : Node_Access := Invalid_Node_Access;
                        --  The root of the new list.
                     begin
                        for I in reverse 1 .. Target_Anc_Index - 1 loop
                           declare
                              Deleting_Node : constant Valid_Node_Access := Inverted_Parents.Peek (I);
                           begin
                              pragma Assert (Deleting_Node.ID = Stream_Node.ID and Deleting_Node.RHS_Index in 1 | 2);
                              --  test_incremental.adb Recover_08a, b have RHS_Index = 2 here.

                              if Deleting_Node.Error_List /= null then
                                 --  FIXME: Move errors. need test case
                                 raise SAL.Not_Implemented with "error on optimized_list";
                              end if;

                              for Child of Deleting_Node.Children loop
                                 Child.Parent := Invalid_Node_Access;
                              end loop;

                              if Deleting_Node.RHS_Index = 2 then
                                 pragma Assert
                                   (Deleting_Node.Children'Length = 2 and
                                      Insert_Leading /= Insert_Following);

                                 pragma Assert (I /= 1);
                                 --  If I = 1 here, Deleting_Node would be Stream_Node, and would be
                                 --  handled by the not optimizing_list branch of Undo_Reduce_Loop.

                                 if Inverted_Parents.Peek (I + 1) = Deleting_Node.Children (1) then
                                    pragma Assert (Following_Node = null);
                                    --  Split_Node is under Deleting_Node.Children (1).
                                    --  test_incremental.adb Recover_08c.

                                    --  Example tree from test_incremental.adb Recover_08c step 3
                                    --
                                    --  125:declaration_list_1       ; Inverted_Parents.Peek (Target_Anc_Index - 1)
                                    --  | 118:declaration_list_2     ; Deleting_Node
                                    --  | | 117:declaration_list_1
                                    --  | | | 105:declaration_list_0 ; Split_Node
                                    --  | | | | 48 declaration
                                    --  | | | | | <a>
                                    --  | | | 116:declaration
                                    --  | | | | <b>               ; b contains by the deleted ';'
                                    --  | | 103:declaration_list_1   ;
                                    --  | | | 102:declaration_list_0 ; Split_Node
                                    --  | | | | <c d>                ; Insert_Leading
                                    --
                                    --  105:Split_Node children are on the stream, 103 goes in
                                    --  Following_Node. Other children of 118 are handled in the
                                    --  next iteration of this loop.

                                    Following_Node := Add_Nonterm_1
                                      (Tree, (List_ID, 0),
                                       Children         => (1 => Deleting_Node.Children (2)),
                                       Clear_Parents    => False,
                                       Recover_Conflict => False);

                                 else
                                    pragma Assert (Following_Node /= null);
                                    --  Split_Node is under Deleting_Node.Children (2).

                                    --  Example tree from test_incremental.adb Recover_08d step 3
                                    --
                                    --  118:declaration_list_1       ; Inverted_Parents.Peek (Target_Anc_Index - 1)
                                    --  | 111:declaration_list_2     ; Deleting_Node
                                    --  | | 110:declaration_list_1   ;
                                    --  | | |   <a b c>              ; c is followed by the deleted ';'
                                    --  | | 103:declaration_list_1   ;
                                    --  | | | 102:declaration_list_0 ; Split_Node
                                    --  | | | | <d>                  ; Insert_Leading
                                    --
                                    --  102:Split_Node children are on the stream, rest of children of 103
                                    --  are in Following_Node. 110 goes before Insert_Leading; nothing is
                                    --  added to Following_Node. Other children of 118 are handled in the
                                    --  next iteration of this loop.
                                    Insert_Leading := Stream.Elements.Insert
                                      (Insert_Leading, (Deleting_Node.Children (1), Unknown_State));
                                 end if;

                              elsif Following_Node = Invalid_Node_Access then
                                 pragma Assert
                                   (I = Target_Anc_Index - 1 and
                                      Deleting_Node.Children (1).ID = List_ID and
                                      Deleting_Node.RHS_Index in 0 | 1);
                                 --  Deleting_Node.Children (1).RHS_Index any of 0 .. 2, depending on
                                 --  how many nodes were before Split_Node and whether that sublist was
                                 --  edited.

                                 declare
                                    New_Children : Valid_Node_Access_Array (1 .. Deleting_Node.Child_Count - 1) :=
                                      (others => Dummy_Node);
                                 begin
                                    for I in New_Children'Range loop
                                       --  If the list has a separator, it should go on the stream, not in
                                       --  Following. But we can't distinguish that from a multi-item list
                                       --  element. And the parser handles both by breaking this down and
                                       --  shifting the items individually. test_incremental.adb
                                       --  Edit_Code_18.
                                       New_Children (I) := Deleting_Node.Children (I + 1);
                                    end loop;

                                    Following_Node := Add_Nonterm_1
                                      (Tree, (List_ID, 0),
                                       Children         => New_Children,
                                       Clear_Parents    => False,
                                       Recover_Conflict => False);
                                 end;

                              else
                                 pragma Assert
                                   (Deleting_Node.RHS_Index = 1 and
                                      Deleting_Node.Children (1).ID = List_ID and
                                      Deleting_Node.Children (1).RHS_Index in 1 | 2);

                                 declare
                                    New_Children : Valid_Node_Access_Array (1 .. Deleting_Node.Child_Count) :=
                                      (others => Dummy_Node);
                                 begin
                                    New_Children (1) := Following_Node;
                                    for I in 2 .. New_Children'Last loop
                                       New_Children (I) := Deleting_Node.Children (I);
                                    end loop;

                                    Following_Node := Add_Nonterm_1
                                      (Tree, (List_ID, 1),
                                       Children         => New_Children,
                                       Clear_Parents    => False,
                                       Recover_Conflict => False);
                                 end;
                              end if;
                           end;
                        end loop;
                        Inverted_Parents.Pop (Target_Anc_Index - 1);

                        Stream.Elements.Insert (Insert_Following, (Following_Node, Unknown_State));
                     end;
                  end if;
                  Inverted_Parents.Pop; --  Split_Node

                  exit Undo_Reduce_Loop when Element (Cur).Node = Target;
               end;

            else
               --  Not an optimized_list node. Bring all children of Node to stream.
               for I in reverse 1 .. Stream_Node.Child_Count loop
                  Cur := Stream.Elements.Insert
                    (Element  =>
                       (Node  => Stream_Node.Children (I),
                        State => Unknown_State),
                     Before   => Cur);

                  Stream_Node.Children (I).Parent := Invalid_Node_Access;
               end loop;

               --  We need to do Move_Errors before the children of To_Delete are set
               --  Invalid. test_incremental.adb Missing_Name_1. Pop To_Delete from
               --  Inverted_Parents now, to match First_Terminal in Move_Errors.
               Inverted_Parents.Pop;
               Move_Errors;

               Stream.Elements.Delete (To_Delete);

               --  Find stream element containing Target
               declare
                  Node : constant Valid_Node_Access :=
                    (if Inverted_Parents.Depth = 0
                     then Target
                     else Inverted_Parents.Peek);
               begin
                  Find_Element_1 :
                  loop
                     exit Undo_Reduce_Loop when Element (Cur).Node = Target;
                     exit Find_Element_1 when Element (Cur).Node = Node;
                     Next (Cur);
                  end loop Find_Element_1;
               end;
            end if;
         end;
         To_Delete := Cur;
      end loop Undo_Reduce_Loop;
      Ref.Parents := Inverted_Parents.Invert;
   end Breakdown;

   procedure Breakdown
     (Tree           : in out Syntax_Trees.Tree;
      Ref            : in out Stream_Node_Ref;
      Productions    : in     Production_Info_Trees.Vector;
      User_Data      : in     Syntax_Trees.User_Data_Access_Constant;
      First_Terminal : in     Boolean)
   is
      Ref_Parents : Stream_Node_Parents := Tree.To_Stream_Node_Parents (Ref);
   begin
      Ref := Invalid_Stream_Node_Ref; --  Allow Delete Ref.Element.
      Breakdown (Tree, Ref_Parents, Productions, User_Data, First_Terminal);
      Ref := Ref_Parents.Ref;
   end Breakdown;

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return WisiToken.Buffer_Region
   is
      Prev_Source_Terminal : Node_Access := Invalid_Node_Access;
      Next_Source_Terminal : Node_Access := Invalid_Node_Access;

      procedure Set_Prev
      is begin
         if Node.ID = Tree.Lexer.Descriptor.SOI_ID then
            Prev_Source_Terminal := Node;
         else
            Prev_Source_Terminal := Tree.Prev_Source_Terminal (Node, Trailing_Non_Grammar => True);
         end if;
      end Set_Prev;

   begin
      case Node.Label is
      when Source_Terminal =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            pragma Assert (Node.Byte_Region /= Null_Buffer_Region);
            return
              (First => Node.Byte_Region.First,
               Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);
         else
            return Node.Byte_Region;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         case Node.Insert_Location is
         when After_Prev | Between =>
            --  It is tempting to assert Node.Non_Grammar.Length > 0 for between
            --  (because there should be a new_line), but that is not true for
            --  "null" when we insert "null;" - it is only true for the last
            --  virtual token on the line.
            if Node.Non_Grammar.Length > 0 then
               return
                 (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);

            elsif Tree.Parents_Set then
               Set_Prev;
               if Prev_Source_Terminal = Invalid_Node_Access then
                  --  Node is in an all virtual parse stream element.
                  return Null_Buffer_Region;
               else
                  return
                    (First => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last,
                     Last  => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last - 1);
               end if;
            else
               return Null_Buffer_Region;
            end if;

         when Before_Next =>
            if Node.Non_Grammar.Length > 0 then
               return
                 (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);

            elsif Tree.Parents_Set then
               if Node.ID = Tree.Lexer.Descriptor.EOI_ID then
                  Next_Source_Terminal := Node;
               else
                  Next_Source_Terminal := Tree.Next_Source_Terminal (Node, Trailing_Non_Grammar => True);
               end if;

               if Next_Source_Terminal = Invalid_Node_Access then
                  --  Node is in an all virtual parse stream element.
                  return Null_Buffer_Region;
               else
                  return
                    (First => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First,
                     Last  => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First - 1);
               end if;
            else
               return Null_Buffer_Region;
            end if;
         end case;

      when Nonterm =>
         if Node.Child_Count = 0 then
            if Tree.Parents_Set then
               Set_Prev;
               if Prev_Source_Terminal = Invalid_Node_Access then
                  --  Node is the root of an empty tree or parse stream element.
                  return Null_Buffer_Region;
               else
                  declare
                     First : constant Buffer_Pos := Tree.Byte_Region
                       (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last + 1;
                  begin
                     return (First, First - 1);
                  end;
               end if;
            else
               return Null_Buffer_Region;
            end if;
         else
            Prev_Source_Terminal := First_Source_Terminal
              (Tree, Node, Trailing_Non_Grammar => False, Following => False);
            Next_Source_Terminal := Last_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
         end if;

         if Prev_Source_Terminal = Invalid_Node_Access then
            --  Node.Child_Count > 0, but Node contains no source_terminals; it is
            --  all virtual. Find best estimate for First.
            if Tree.Parents_Set then
               Set_Prev;
               if Prev_Source_Terminal = Invalid_Node_Access then
                  --  Tree is corrupt
                  return Null_Buffer_Region;
               else
                  declare
                     First : constant Buffer_Pos := Tree.Char_Region
                       (Prev_Source_Terminal, Trailing_Non_Grammar).First + 1;
                  begin
                     return (First, First - 1);
                  end;
               end if;

            else
               return Null_Buffer_Region;
            end if;
         end if;

         pragma Assert (Prev_Source_Terminal /= Invalid_Node_Access and Next_Source_Terminal /= Invalid_Node_Access);
         return
           (First => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar).First,
            Last  => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar).Last);
      end case;
   end Byte_Region;

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   is begin
      return Byte_Region (Tree, Stream_Element_Lists.Element (Index.Cur).Node, Trailing_Non_Grammar => False);
   end Byte_Region;

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region
   is
      Prev_Source_Terminal : Terminal_Ref := Invalid_Stream_Node_Ref;
      Next_Source_Terminal : Terminal_Ref := Invalid_Stream_Node_Ref;
   begin
      if Ref.Node = Invalid_Node_Access or else --  Empty nonterm
        Ref.Node.Label = Nonterm
      then
         Prev_Source_Terminal :=
           (Ref.Stream, Ref.Element, First_Source_Terminal (Tree, Ref.Node, Trailing_Non_Grammar, Following => False));
         Next_Source_Terminal :=
           (Ref.Stream, Ref.Element, Last_Source_Terminal (Tree, Ref.Node, Trailing_Non_Grammar));

         if Prev_Source_Terminal.Node = Invalid_Node_Access then
            Prev_Source_Terminal := Tree.Prev_Source_Terminal (Ref, Trailing_Non_Grammar);
            Next_Source_Terminal := Tree.Next_Source_Terminal (Ref, Trailing_Non_Grammar);
         end if;

         return
           (First => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar).Last,
            Last => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar).First);
      else
         case Terminal_Label'(Ref.Node.Label) is
         when Source_Terminal =>
            if Trailing_Non_Grammar and Ref.Node.Non_Grammar.Length > 0 then
               pragma Assert (Ref.Node.Byte_Region /= Null_Buffer_Region);
               return
                 (First => Ref.Node.Byte_Region.First,
                  Last  => Ref.Node.Non_Grammar (Ref.Node.Non_Grammar.Last_Index).Byte_Region.Last);
            else
               return Ref.Node.Byte_Region;
            end if;

         when Virtual_Terminal | Virtual_Identifier =>
            case Ref.Node.Insert_Location is
            when After_Prev | Between =>
               if Ref.Node.Non_Grammar.Length > 0 then
                  return
                    (First => Ref.Node.Non_Grammar (Ref.Node.Non_Grammar.First_Index).Byte_Region.First,
                     Last  => Ref.Node.Non_Grammar (Ref.Node.Non_Grammar.Last_Index).Byte_Region.Last);
               else
                  Prev_Source_Terminal := Tree.Prev_Source_Terminal (Ref, Trailing_Non_Grammar);
                  return
                    (First => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last,
                     Last  => Tree.Byte_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last - 1);
               end if;

            when Before_Next =>
               pragma Assert (Ref.Node.Non_Grammar.Length = 0);
               Next_Source_Terminal := Tree.Next_Source_Terminal (Ref, Trailing_Non_Grammar);
               return
                 (First => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First,
                  Last  => Tree.Byte_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First - 1);
            end case;

         end case;
      end if;
   end Byte_Region;

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Parents;
      Parse_Stream         : in Stream_ID;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region
   is
      Prev_Source_Terminal : Stream_Node_Parents := Invalid_Stream_Node_Parents;
      Next_Source_Terminal : Stream_Node_Parents := Invalid_Stream_Node_Parents;
   begin
      if Ref.Ref.Node = Invalid_Node_Access or else --  Empty nonterm
        Ref.Ref.Node.Label = Nonterm
      then
         Prev_Source_Terminal := Ref;
         Tree.First_Source_Terminal (Prev_Source_Terminal, Trailing_Non_Grammar);
         Next_Source_Terminal := Ref;
         Tree.Last_Source_Terminal (Next_Source_Terminal, Trailing_Non_Grammar);

         if Prev_Source_Terminal.Ref.Node = Invalid_Node_Access then
            Tree.Prev_Source_Terminal (Prev_Source_Terminal, Parse_Stream, Trailing_Non_Grammar);
            Tree.Next_Source_Terminal (Next_Source_Terminal, Trailing_Non_Grammar);
         end if;

         return
           (First => Tree.Byte_Region (Prev_Source_Terminal, Parse_Stream, Trailing_Non_Grammar).Last,
            Last => Tree.Byte_Region (Next_Source_Terminal, Parse_Stream, Trailing_Non_Grammar).First);
      else
         case Terminal_Label'(Ref.Ref.Node.Label) is
         when Source_Terminal =>
            if Trailing_Non_Grammar and Ref.Ref.Node.Non_Grammar.Length > 0 then
               pragma Assert (Ref.Ref.Node.Byte_Region /= Null_Buffer_Region);
               return
                 (First => Ref.Ref.Node.Byte_Region.First,
                  Last  => Ref.Ref.Node.Non_Grammar (Ref.Ref.Node.Non_Grammar.Last_Index).Byte_Region.Last);
            else
               return Ref.Ref.Node.Byte_Region;
            end if;

         when Virtual_Terminal | Virtual_Identifier =>
            if Ref.Ref.Node.Non_Grammar.Length > 0 then
               return
                 (First => Ref.Ref.Node.Non_Grammar (Ref.Ref.Node.Non_Grammar.First_Index).Byte_Region.First,
                  Last  => Ref.Ref.Node.Non_Grammar (Ref.Ref.Node.Non_Grammar.Last_Index).Byte_Region.Last);
            else
               case Ref.Ref.Node.Insert_Location is
               when After_Prev | Between =>
                  Prev_Source_Terminal := Ref;
                  Tree.Prev_Source_Terminal (Prev_Source_Terminal, Parse_Stream, Trailing_Non_Grammar);
                  return
                    (First => Tree.Byte_Region (Prev_Source_Terminal, Parse_Stream, Trailing_Non_Grammar => True).Last,
                     Last  => Tree.Byte_Region
                       (Prev_Source_Terminal, Parse_Stream, Trailing_Non_Grammar => True).Last - 1);

               when Before_Next =>
                  --  It doesn't make sense for Ref.Ref.Node.Non_Grammar.Length > 0
                  --  here, but we tolerate buggy language-specific code in Insert_Token
                  --  or Delete_Token.
                  Next_Source_Terminal := Ref;
                  Tree.Next_Source_Terminal (Next_Source_Terminal, Trailing_Non_Grammar);
                  return
                    (First => Tree.Byte_Region (Next_Source_Terminal, Parse_Stream, Trailing_Non_Grammar => True).First,
                     Last  => Tree.Byte_Region (Next_Source_Terminal, Parse_Stream, Trailing_Non_Grammar => True)
                       .First - 1);
               end case;
            end if;
         end case;
      end if;
   end Byte_Region;

   function Byte_Region (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region
   is begin
      if Item.Virtual then
         return Null_Buffer_Region;
      elsif Item.Element_Node = Invalid_Node_Access then
         return Null_Buffer_Region;
      else
         case Item.Element_Node.Label is
         when Source_Terminal =>
            return Item.Element_Node.Byte_Region;
         when Virtual_Terminal | Virtual_Identifier =>
            return Null_Buffer_Region;
         when Nonterm =>
            declare
               First : constant Node_Access := First_Source_Terminal
                 (Tree, Item.Element_Node, Trailing_Non_Grammar => False, Following => False);
               Last  : constant Node_Access := Last_Source_Terminal
                 (Tree, Item.Element_Node, Trailing_Non_Grammar => False);
            begin
               if First = Invalid_Node_Access then
                  return Null_Buffer_Region;
               else
                  return (First.Byte_Region.First, Last.Byte_Region.Last);
               end if;
            end;
         end case;
      end if;
   end Byte_Region;

   function Char_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Buffer_Region
   is
      Prev_Source_Terminal : Node_Access := Invalid_Node_Access;
      Next_Source_Terminal : Node_Access := Invalid_Node_Access;

      procedure Set_Prev
      is begin
         if Node.ID = Tree.Lexer.Descriptor.SOI_ID then
            Prev_Source_Terminal := Node;
         else
            Prev_Source_Terminal := Tree.Prev_Source_Terminal (Node, Trailing_Non_Grammar => True);
         end if;
      end Set_Prev;

   begin
      case Node.Label is
      when Source_Terminal =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            pragma Assert (Node.Char_Region /= Null_Buffer_Region);
            return
              (First => Node.Char_Region.First,
               Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last);
         else
            return Node.Char_Region;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if Node.Non_Grammar.Length > 0 then
            return
              (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Char_Region.First,
               Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last);

         else
            case Node.Insert_Location is
            when After_Prev | Between =>
               if Tree.Parents_Set then
                  Set_Prev;
                  if Prev_Source_Terminal = Invalid_Node_Access then
                     --  Node is in an all virtual parse stream element.
                     return Null_Buffer_Region;
                  else
                     return
                       (First => Tree.Char_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last,
                        Last  => Tree.Char_Region (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last - 1);
                  end if;
               else
                  return Null_Buffer_Region;
               end if;

            when Before_Next =>
               if Tree.Parents_Set then
                  if Node.ID = Tree.Lexer.Descriptor.EOI_ID then
                     Next_Source_Terminal := Node;
                  else
                     Next_Source_Terminal := Tree.Next_Source_Terminal (Node, Trailing_Non_Grammar => True);
                  end if;

                  if Next_Source_Terminal = Invalid_Node_Access then
                     --  Node is in an all virtual parse stream element.
                     return Null_Buffer_Region;
                  else
                     return
                       (First => Tree.Char_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First,
                        Last  => Tree.Char_Region (Next_Source_Terminal, Trailing_Non_Grammar => True).First - 1);
                  end if;
               else
                  return Null_Buffer_Region;
               end if;
            end case;
         end if;

      when Nonterm =>
         if Node.Child_Count = 0 then
            if Tree.Parents_Set then
               Set_Prev;
               if Prev_Source_Terminal = Invalid_Node_Access then
                  --  Node is the root of an empty tree or parse stream element.
                  return Null_Buffer_Region;
               else
                  declare
                     First : constant Buffer_Pos := Tree.Char_Region
                       (Prev_Source_Terminal, Trailing_Non_Grammar => True).Last + 1;
                  begin
                     return (First, First - 1);
                  end;
               end if;
            else
               return Null_Buffer_Region;
            end if;
         else
            Prev_Source_Terminal := First_Source_Terminal
              (Tree, Node, Trailing_Non_Grammar => False, Following => False);
            Next_Source_Terminal := Last_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
         end if;

         if Prev_Source_Terminal = Invalid_Node_Access then
            --  Node.Child_Count > 0, but Node contains no source_terminals; it is
            --  all virtual. Find best estimate for First.
            if Tree.Parents_Set then
               Set_Prev;
               declare
                  First : constant Buffer_Pos := Tree.Char_Region
                    (Prev_Source_Terminal, Trailing_Non_Grammar).First + 1;
               begin
                  return (First, First - 1);
               end;

            else
               return Null_Buffer_Region;
            end if;
         end if;

         pragma Assert (Prev_Source_Terminal /= Invalid_Node_Access and Next_Source_Terminal /= Invalid_Node_Access);
         return
           (First => Tree.Char_Region (Prev_Source_Terminal, Trailing_Non_Grammar).First,
            Last  => Tree.Char_Region (Next_Source_Terminal, Trailing_Non_Grammar).Last);
      end case;
   end Char_Region;

   function Check_Multi_Line
     (Tree       : in     Syntax_Trees.Tree;
      Node       : in     Valid_Node_Access;
      Line       : in     Line_Number_Type;
      Char_Pos   : in out Buffer_Pos;
      Start_Line : in     Line_Number_Type)
     return Boolean
   with Pre => Node.Label in Terminal_Label and Tree.Lexer.Can_Contain_New_Line (Node.ID)
   --  Return True if Node contains the new_line that ends Line - 1; set
   --  Char_Pos to the character position following the New_Line.
   --  Start_Line must be the line number at the first char of Node.
   is
      Temp : Base_Buffer_Pos := Node.Byte_Region.First;
   begin
      for I in Start_Line + 1 .. Line loop
         Temp := Tree.Lexer.Find_New_Line ((Temp, Node.Byte_Region.Last));
         if Temp = Invalid_Buffer_Pos then
            return False;
         end if;
         Temp := @ + 1;
      end loop;
      Char_Pos := Temp;
      return True;
   end Check_Multi_Line;

   function Check_Non_Grammar
     (Tree     : in     Syntax_Trees.Tree;
      Node     : in     Valid_Node_Access;
      Line     : in     Line_Number_Type;
      Char_Pos : in out Buffer_Pos)
     return Boolean
   with Pre => Node.Label in Terminal_Label
   --  Return True if Node contains non_grammar that ends Line - 1; set
   --  Char_Pos to the character position following the New_Line.
   is begin
      for Token of Node.Non_Grammar loop
         if Token.Line_Region.First <= Line - 1 and Token.Line_Region.Last >= Line then
            declare
               Temp : constant Base_Buffer_Pos := Tree.Lexer.Line_Begin_Char_Pos (Token, Line);
            begin
               if Temp /= Invalid_Buffer_Pos then
                  Char_Pos := Temp;
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Check_Non_Grammar;

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Child_Index : in Positive_Index_Type)
     return Node_Access
   is
   begin
      if Child_Index in Node.Children'Range then
         return Node.Children (Child_Index);
      else
         return Invalid_Node_Access;
      end if;
   end Child;

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   is begin
      return Node.Child_Count;
   end Child_Count;

   function Child_Index (Parent : in Node; Child : in Valid_Node_Access) return SAL.Peek_Type
   is begin
      for I in Parent.Children'Range loop
         if Parent.Children (I) = Child then
            return I;
         end if;
      end loop;
      raise SAL.Programmer_Error;
   end Child_Index;

   function Child_Index
     (Tree   : in Syntax_Trees.Tree;
      Parent : in Valid_Node_Access;
      Child  : in Valid_Node_Access)
     return SAL.Peek_Type
   is
      pragma Unreferenced (Tree);
   begin
      return Child_Index (Parent.all, Child);
   end Child_Index;

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access_Array
   is begin
      return Node.Children;
   end Children;

   function Children_Recover_Tokens
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Recover_Token_Array
   is
      Node : constant Node_Access := Stream_Element_Lists.Element (Element.Cur).Node;
   begin
      --  WORKAROUND: GNAT Community 2020 doesn't support 'of' here, and it
      --  hangs if there are any errors in the statement with 'in'.
      --  return (for I in Node.Children'Range => Tree.Get_Recover_Token (Node.Children (I)));
      return Result : Recover_Token_Array (1 .. Node.Child_Count) do
         for I in Node.Children'Range loop
            Result (I) := Tree.Get_Recover_Token (Node.Children (I));
         end loop;
      end return;
   end Children_Recover_Tokens;

   procedure Clear
     (Tree        : in out Syntax_Trees.Tree;
      Free_Memory : in     Boolean := False)
   is begin
      for N of Tree.Nodes loop
         Free (N.Augmented);
         Free (N.Error_List);
         Free (N);
      end loop;
      Tree.Nodes.Clear (Free_Memory);

      --  Clear saved element list cursors in parse streams before freeing
      --  the element lists, so they don't try to decrement reference counts
      --  in deallocated elements. We can't rely on cursor Finalize for
      --  this; that's done in arbitrary order. Loop in reverse order so
      --  shared stream is last; other streams have links to it.
      for Stream of reverse Tree.Streams loop
         Stream.Stack_Top   := Stream_Element_Lists.No_Element;
         Stream.Shared_Link := Stream_Element_Lists.No_Element;
         if Debug_Mode then
            Stream.Elements.Check_Ref_Counts;
         end if;
      end loop;
      Tree.Streams.Clear;

      Tree.Root                     := Invalid_Node_Access;
      Tree.SOI                      := Invalid_Node_Access;
      Tree.EOI                      := Invalid_Node_Access;
      Tree.Next_Stream_Label        := Shared_Stream_Label + 1;
      Tree.Next_Terminal_Node_Index := 1;
      Tree.Traversing               := False;
      Tree.Parents_Set              := False;
   end Clear;

   function Cleared (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Streams.Length = 0 and Tree.Nodes.Length = 0;
   end Cleared;

   procedure Clear_Augmented (Tree : in Syntax_Trees.Tree)
   is begin
      for Node of Tree.Nodes loop
         Free (Node.Augmented);
         if Node.Label = Source_Terminal then
            for N of Node.Following_Deleted loop
               Free (N.Augmented);
            end loop;
         end if;
      end loop;
   end Clear_Augmented;

   procedure Clear_Parent
     (Tree           : in out Syntax_Trees.Tree;
      Node           : in     Valid_Node_Access;
      Clear_Children : in     Boolean)
   is begin
      if Node.Parent /= Invalid_Node_Access and Clear_Children then
         Node.Parent.Children (Child_Index (Node.Parent.all, Node)) := null;

         if Node.Parent = Tree.Root then
            Tree.Root := Node;
         end if;
      end if;
      Node.Parent := Invalid_Node_Access;
   end Clear_Parent;

   procedure Clear_Parse_Streams
     (Tree       : in out Syntax_Trees.Tree;
      Keep_Nodes : in     Valid_Node_Access_Lists.List := Valid_Node_Access_Lists.Empty_List)
   is begin
      if Tree.Root = Invalid_Node_Access then
         Tree.Root := Syntax_Trees.Root (Tree);
      end if;

      --  Add SOI, EOI (from the parse stream, to include any
      --  Following_Deleted and Error_Data) to Root children, so
      --  Prev/Next_Non_Grammar can find them.
      declare
         Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Tree.Streams.Last);
         SOI : constant Valid_Node_Access := Stream_Element_Lists.Element (Parse_Stream.Elements.First).Node;

         Last_Node : constant Valid_Node_Access := Stream_Element_Lists.Element (Parse_Stream.Elements.Last).Node;

         EOI : constant Valid_Node_Access :=
           (if Tree.ID (Last_Node) = Tree.Lexer.Descriptor.EOI_ID
            then Last_Node
            else Tree.EOI);

         New_Children : Node_Access_Array (1 .. Tree.Root.Child_Count + 2);
      begin
         if Tree.Streams.Last = Tree.Shared_Stream.Cur and Tree.Root.Child_Count = 3 then
            --  This is a packrat parse; SOI, EOI already in tree
            pragma Assert (Tree.Root.Children (1) = Tree.SOI and Tree.Root.Children (3) = EOI);
         else
            --  There is a parse stream, or this is an incremental parse where the
            --  edit did not require a parse.
            New_Children (1) := SOI;
            New_Children (2 .. New_Children'Last - 1) := Tree.Root.Children;
            New_Children (New_Children'Last) := EOI;

            Tree.SOI := SOI;
            Tree.EOI := EOI;

            Tree.Root.Children := (others => Invalid_Node_Access);

            Tree.Root := new Node'
              (Label       => Nonterm,
               Copied_Node => Invalid_Node_Access,
               Child_Count => Tree.Root.Child_Count + 2,
               ID          => Tree.Root.ID,
               Node_Index  => Tree.Root.Node_Index,
               Parent      => null,
               Augmented   => Tree.Root.Augmented,
               Error_List  =>
                 (if Tree.Root.Error_List = null
                  then null
                  else new Error_Data_Lists.List'(Tree.Root.Error_List.all)),
               Virtual          => Tree.Root.Virtual,
               Recover_Conflict => False,
               RHS_Index        => Tree.Root.RHS_Index,
               Name_Offset      => Tree.Root.Name_Offset,
               Name_Length      => Tree.Root.Name_Length,
               Children         => New_Children);

            for Child of New_Children loop
               Child.Parent := Tree.Root;
            end loop;

            Tree.Nodes.Append (Tree.Root);
         end if;
      end;

      --  Clear saved element list cursors in parse streams before freeing
      --  the element lists, so they don't try to decrement reference counts
      --  in deallocated elements. We can't rely on cursor Finalize for
      --  this; that's done in arbitrary order.
      for Stream of Tree.Streams loop
         Stream.Stack_Top   := Stream_Element_Lists.No_Element;
         Stream.Shared_Link := Stream_Element_Lists.No_Element;
      end loop;

      Tree.Streams.Clear;
      Tree.Next_Stream_Label := Shared_Stream_Label + 1;

      Tree.Shared_Stream.Cur := Parse_Stream_Lists.No_Element;

      if not Tree.Parents_Set then
         Set_Parents (Tree);
      end if;

      for Node of Tree.Nodes loop
         --  Only nodes that have parents are part of the final parse result.
         --  In an incremental parse, breakdown removes nodes from a parse
         --  stream, and clears any parent pointers involved.
         if Node.Parent = null and then
           Node /= Tree.Root and then
           Node /= Tree.SOI and then
           Node /= Tree.EOI and then
           not (for some N of Keep_Nodes => N = Node)
         then
            --  It is tempting to try to enforce that all deleted nonterms have
            --  Children = (others => Invalid_Node_Access) here. However, that is
            --  not true when Breakdown is called by the main parser;
            --  Tree.Parents_Set is false, indicating there might be multiple
            --  streams, so Breakdown does not clear children.
            Free (Node);
         end if;
      end loop;

      --  Compact Tree.Nodes
      declare
         Free : Node_Index := Tree.Nodes.First_Index - 1;
      begin
         for I in Tree.Nodes.First_Index .. Tree.Nodes.Last_Index loop
            if Free < Tree.Nodes.First_Index then
               if Tree.Nodes (I) = Invalid_Node_Access then
                  Free := I;
               end if;
            else
               if Tree.Nodes (I) /= Invalid_Node_Access then
                  Tree.Nodes (Free) := Tree.Nodes (I);
                  Free := @ + 1;
               end if;
            end if;
         end loop;

         if Free > Tree.Nodes.First_Index then
            Tree.Nodes.Set_First_Last (First => Tree.Nodes.First_Index, Last => Free - 1);
         end if;
      end;
   end Clear_Parse_Streams;

   function Column (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Ada.Text_IO.Count
   is
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node, Trailing_Non_Grammar => False);
   begin
      if Char_Region = Null_Buffer_Region then
         return 0;
      else
         declare
            Begin_Char_Pos : constant Buffer_Pos := Line_Begin_Char_Pos
              (Tree, Line_Region (Tree, Node, Trailing_Non_Grammar => True).First);
         begin
            return
              (if Begin_Char_Pos = Invalid_Buffer_Pos
               then 0
               else Ada.Text_IO.Count (Char_Region.First - Begin_Char_Pos));
         end;
      end if;
   end Column;

   function Column
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Stream : in Stream_ID)
     return Ada.Text_IO.Count
   is
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node, Trailing_Non_Grammar => False);
   begin
      if Char_Region.First = Invalid_Buffer_Pos then
         return 0;
      else
         declare
            Begin_Char_Pos : constant Buffer_Pos := Line_Begin_Char_Pos
              (Tree, Tree.Line_Region (Node, Trailing_Non_Grammar => True).First, Stream);
         begin
            return
              (if Begin_Char_Pos = Invalid_Buffer_Pos
               then 0
               else Ada.Text_IO.Count (Char_Region.First - Begin_Char_Pos));
         end;
      end if;
   end Column;

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean
   is begin
      return (Tree.Is_Valid (Stream) and Token /= Invalid_Stream_Index) and then
        Tree.Streams (Stream.Cur).Elements.Contains (Token.Cur);
   end Contains;

   function Contains_Error
     (Tree       : in Syntax_Trees.Tree;
      Error_Node : in Valid_Node_Access;
      Data       : in Error_Data'Class)
     return Boolean
   is begin
      if Error_Node.Error_List = null then
         return False;
      else
         return (for some Err of Error_Node.Error_List.all => Dispatch_Equal (Data, Err));
      end if;
   end Contains_Error;

   function Contains_Virtual_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Item.Virtual
         then Item.Contains_Virtual_Terminal
         else
           (case Item.Element_Node.Label is
            when Source_Terminal => False,
            when Virtual_Terminal | Virtual_Identifier => True,
            when Nonterm => Item.Element_Node.Virtual));
   end Contains_Virtual_Terminal;

   function Contains_Virtual_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return
        (case Node.Label is
         when Source_Terminal => False,
         when Virtual_Terminal | Virtual_Identifier => True,
         when Nonterm => Node.Virtual);
   end Contains_Virtual_Terminal;

   function Copied_Node (Node : in Valid_Node_Access) return Node_Access
   is begin
      return Node.Copied_Node;
   end Copied_Node;

   procedure Copy_Ancestors
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      New_Node  : in     Valid_Node_Access;
      User_Data : in     User_Data_Access_Constant)
   --  Replace Ref.Node with New_Node, copying all parents thru
   --  Ref.Element, updating Ref.Element and Ref.Parents to match.
   is
      Temp      : Valid_Node_Access := New_Node;
      Old_Child : Valid_Node_Access := Ref.Ref.Node;
      New_Stack : Node_Stacks.Stack;
   begin
      Ref.Ref.Node := New_Node;
      if Ref.Parents.Depth = 0 then
         --  ada_mode-recover_debbugs_36548.adb
         return;
      end if;
      loop
         declare
            Old_Parent  : constant Valid_Node_Access := Ref.Parents.Pop;
            New_Child   : constant Valid_Node_Access := Temp;
            Child_Index : constant SAL.Peek_Type     := Syntax_Trees.Child_Index
              (Parent => Old_Parent.all, Child => Old_Child);
         begin
            Temp := Copy_Node
              (Tree, Old_Parent,
               Parent        =>
                 (if Tree.Parents_Set
                  then Old_Parent.Parent
                  else Invalid_Node_Access),
               User_Data              => User_Data,
               Copy_Children          => False,
               Copy_Following_Deleted => True);
            Temp.Children (Child_Index) := New_Child;
            Old_Child.Parent            := Invalid_Node_Access;
            if Tree.Parents_Set then
               New_Child.Parent := Temp;
            end if;
            New_Stack.Push (Temp);

            if Ref.Parents.Depth = 0 then
               if Old_Parent = Tree.Root then
                  Tree.Root := Temp;
               end if;
               exit;
            end if;

            Old_Child := Old_Parent;
         end;
      end loop;

      Replace_Node (Ref.Ref.Element, New_Stack.Peek);

      loop
         Ref.Parents.Push (New_Stack.Pop);
         exit when New_Stack.Depth = 0;
      end loop;
   end Copy_Ancestors;

   function Copy_Augmented
     (User_Data : in User_Data_Type;
      Augmented : in Augmented_Class_Access)
     return Augmented_Class_Access
   is begin
      raise SAL.Programmer_Error;
      return null;
   end Copy_Augmented;

   function Copy_Node
     (Tree                   : in out Syntax_Trees.Tree;
      Node                   : in     Valid_Node_Access;
      Parent                 : in     Node_Access;
      User_Data              : in     User_Data_Access_Constant;
      Copy_Children          : in     Boolean;
      Copy_Following_Deleted : in     Boolean;
      New_Error_List         : in     Error_List_Access := null;
      Set_Error_List         : in     Boolean           := False;
      Set_Copied_Node        : in     Boolean           := False)
     return Valid_Node_Access
   is
      use all type Error_Data_Lists.List;
      New_Node : Node_Access;
   begin
      case Node.Label is
      when Source_Terminal =>
         New_Node := new Syntax_Trees.Node'
           (Label          => Source_Terminal,
            Copied_Node    => Invalid_Node_Access,
            Child_Count    => 0,
            ID             => Node.ID,
            Node_Index     => Tree.Next_Terminal_Node_Index,
            Byte_Region    => Node.Byte_Region,
            Char_Region    => Node.Char_Region,
            New_Line_Count => Node.New_Line_Count,
            Parent         => Parent,
            Augmented      =>
              (if Node.Augmented = null or User_Data = null
               then null
               else Copy_Augmented (User_Data.all, Node.Augmented)),
            Error_List        =>
              (if Set_Error_List
               then New_Error_List
               else
                 (if Node.Error_List = null
                  then null
                  else new Error_Data_Lists.List'(Node.Error_List.all))),
            Non_Grammar       => Node.Non_Grammar,
            Sequential_Index  => Node.Sequential_Index,
            Following_Deleted => Valid_Node_Access_Lists.Empty_List);

         if Copy_Following_Deleted then
            if Copy_Children then
               for Deleted_Node of Node.Following_Deleted loop
                  New_Node.Following_Deleted.Append
                    (Copy_Node (Tree, Deleted_Node, New_Node, User_Data, Copy_Children => False,
                                Copy_Following_Deleted => False));
               end loop;
            else
               New_Node.Following_Deleted := Node.Following_Deleted;
            end if;
         end if;

         Tree.Next_Terminal_Node_Index := @ + 1;

      when Virtual_Terminal =>
         New_Node := new Syntax_Trees.Node'
           (Label           => Virtual_Terminal,
            Copied_Node     => Invalid_Node_Access,
            Child_Count     => 0,
            ID              => Node.ID,
            Node_Index      => -(Tree.Nodes.Last_Index + 1),
            Parent          => Parent,
            Augmented       =>
              (if Node.Augmented = null or User_Data = null
               then null
               else Copy_Augmented (User_Data.all, Node.Augmented)),
            Error_List       => New_Error_List,
            Non_Grammar      => Node.Non_Grammar,
            Sequential_Index => Node.Sequential_Index,
            Insert_Location  => Node.Insert_Location);

      when Virtual_Identifier =>

         New_Node := new Syntax_Trees.Node'
           (Label       => Virtual_Identifier,
            Copied_Node => Invalid_Node_Access,
            Child_Count => 0,
            ID          => Node.ID,
            Node_Index  => -(Tree.Nodes.Last_Index + 1),
            Parent      => Parent,
            Augmented   =>
              (if Node.Augmented = null or User_Data = null
               then null
               else Copy_Augmented (User_Data.all, Node.Augmented)),
            Error_List       => New_Error_List,
            Non_Grammar      => Node.Non_Grammar,
            Sequential_Index => Node.Sequential_Index,
            Identifier       => Node.Identifier,
            Insert_Location  => Node.Insert_Location);

      when Nonterm =>
         --  Copy children first to preserve Node_Index order = parse order in a batch parsed tree.
         declare
            New_Children : Node_Access_Array (Node.Children'Range);
         begin
            if Copy_Children then
               for I in New_Children'Range loop
                  New_Children (I) := Copy_Node
                    (Tree, Node.Children (I), Invalid_Node_Access, User_Data, Copy_Children,
                     Copy_Following_Deleted => True);
               end loop;
            else
               New_Children := Node.Children;
            end if;

            New_Node := new Syntax_Trees.Node'
              (Label       => Nonterm,
               Copied_Node => Invalid_Node_Access,
               Child_Count => Node.Child_Count,
               ID          => Node.ID,
               Node_Index  => -(Tree.Nodes.Last_Index + 1),
               Parent      => Parent,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               Error_List       => New_Error_List,
               Virtual          => Node.Virtual,
               Recover_Conflict => Node.Recover_Conflict,
               RHS_Index        => Node.RHS_Index,
               Name_Offset      => Node.Name_Offset,
               Name_Length      => Node.Name_Length,
               Children         => New_Children);

            if Copy_Children then
               pragma Assert (Tree.Parents_Set);
               for Child of New_Node.Children loop
                  Child.Parent := New_Node;
               end loop;

            else
               if Tree.Parents_Set then
                  Set_Children (Tree, New_Node, Node.Children);
               else
                  New_Node.Children := Node.Children;
               end if;
            end if;
         end;
      end case;

      Tree.Nodes.Append (New_Node);

      if Set_Copied_Node then
         Node.Copied_Node := New_Node;
      end if;

      return New_Node;
   end Copy_Node;

   function Copy_Subtree
     (Tree      : in out Syntax_Trees.Tree;
      Root      : in     Node_Access;
      User_Data : in     User_Data_Access_Constant)
     return Node_Access
   is begin
      if Root = Invalid_Node_Access then
         return Invalid_Node_Access;
      else
         return Copy_Node (Tree, Root, Invalid_Node_Access, User_Data, Copy_Children => True,
                           Copy_Following_Deleted => True);
      end if;
   end Copy_Subtree;

   procedure Copy_Tree
     (Source      : in     Tree;
      Destination :    out Tree;
      User_Data   : in     User_Data_Access_Constant)
   is
      Next_Terminal_Node_Index : Node_Index := 0;

      function Copy_Node
        (Source_Node : in Valid_Node_Access;
         Dest_Parent : in Node_Access)
        return Valid_Node_Access
      is
         New_Dest_Node : Node_Access;

         function Copy_Errors return Error_List_Access
         is (if Source_Node.Error_List = null
             then null
             else new Error_Data_Lists.List'(Source_Node.Error_List.all));

      begin
         case Source_Node.Label is
         when Source_Terminal =>

            if Next_Terminal_Node_Index = 0 and Source_Node.ID /= Source.Lexer.Descriptor.SOI_ID then
               --  SOI is normally the first terminal seen.
               Next_Terminal_Node_Index := @ + 1;
            end if;

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Source_Terminal,
               Copied_Node => Invalid_Node_Access,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => Next_Terminal_Node_Index,
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Error_List        => Copy_Errors,
               Byte_Region       => Source_Node.Byte_Region,
               Char_Region       => Source_Node.Char_Region,
               New_Line_Count    => Source_Node.New_Line_Count,
               Non_Grammar       => Source_Node.Non_Grammar,
               Sequential_Index  => Source_Node.Sequential_Index,
               Following_Deleted => Valid_Node_Access_Lists.Empty_List);

            Next_Terminal_Node_Index := @ + 1;

            if New_Dest_Node.ID = Source.Lexer.Descriptor.SOI_ID then
               Destination.SOI := New_Dest_Node;
            elsif New_Dest_Node.ID = Source.Lexer.Descriptor.EOI_ID then
               Destination.EOI  := New_Dest_Node;
            end if;

            for Deleted of Source_Node.Following_Deleted loop
               New_Dest_Node.Following_Deleted.Append (Copy_Node (Deleted, New_Dest_Node));
            end loop;

         when Virtual_Terminal =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Copied_Node => Invalid_Node_Access,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => -(Destination.Nodes.Last_Index + 1),
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Error_List       => Copy_Errors,
               Non_Grammar      => Source_Node.Non_Grammar,
               Sequential_Index => Source_Node.Sequential_Index,
               Insert_Location  => Source_Node.Insert_Location);

         when Virtual_Identifier =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Copied_Node => Invalid_Node_Access,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => -(Destination.Nodes.Last_Index + 1),
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Error_List       => Copy_Errors,
               Non_Grammar      => Source_Node.Non_Grammar,
               Sequential_Index => Source_Node.Sequential_Index,
               Identifier       => Source_Node.Identifier,
               Insert_Location  => Source_Node.Insert_Location);

         when Nonterm =>
            --  Copy children first to preserve Node_Index order = parse order in a batch parsed tree.
            declare
               New_Children : Node_Access_Array (Source_Node.Children'Range);
            begin
               for I in New_Children'Range loop
                  New_Children (I) := Copy_Node (Source_Node.Children (I), Dummy_Node);
               end loop;

               New_Dest_Node := new Syntax_Trees.Node'
                 (Label       => Nonterm,
                  Copied_Node => Invalid_Node_Access,
                  Child_Count => Source_Node.Child_Count,
                  ID          => Source_Node.ID,
                  Node_Index  => -(Destination.Nodes.Last_Index + 1),
                  Parent      => Dest_Parent,
                  Augmented   =>
                    (if Source_Node.Augmented = null or User_Data = null
                     then null
                     else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
                  Error_List       => Copy_Errors,
                  Virtual          => Source_Node.Virtual,
                  Recover_Conflict => Source_Node.Recover_Conflict,
                  RHS_Index        => Source_Node.RHS_Index,
                  Name_Offset      => Source_Node.Name_Offset,
                  Name_Length      => Source_Node.Name_Length,
                  Children         => New_Children);

               for Child of New_Dest_Node.Children loop
                  Child.Parent := New_Dest_Node;
               end loop;
            end;

         end case;
         Source_Node.Copied_Node := New_Dest_Node;
         Destination.Nodes.Append (New_Dest_Node);
         return New_Dest_Node;
      end Copy_Node;
   begin
      Destination.Clear (Free_Memory => False);
      Destination.Lexer                    := Source.Lexer;
      Destination.Next_Terminal_Node_Index := Source.Next_Terminal_Node_Index;
      Destination.Traversing               := False;
      Destination.Parents_Set              := True;

      Destination.Root := Copy_Node (Source.Root, Invalid_Node_Access);

      for Err in Destination.Error_Iterate loop
         Error_Data_Lists.Variable_Ref (Err.Error).Adjust_Copy;
      end loop;

      for Node of Source.Nodes loop
         Node.Copied_Node := Invalid_Node_Access;
      end loop;
   end Copy_Tree;

   function Correct_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is begin
      return Ref = Invalid_Stream_Node_Ref or else
        (Ref.Element /= Invalid_Stream_Index and then
           (Ref.Node = Invalid_Node_Access or else
              (not Tree.Parents_Set or else
                 Tree.Subtree_Root (Ref.Node) = Tree.Get_Node (Ref.Stream, Ref.Element))));
   end Correct_Stream_Node;

   function Count_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return SAL.Base_Peek_Type
   is begin
      return Result : SAL.Base_Peek_Type := 0 do
         if Node.ID = ID then
            Result := 1;
         end if;
         case Node.Label is
         when Nonterm =>
            for I of Node.Children loop
               --  We don't check for deleted child here; encountering one indicates
               --  an error in the user algorithm.
               Result := @ + Count_IDs (Tree, I, ID);
            end loop;
         when others =>
            null;
         end case;
      end return;
   end Count_IDs;

   function Count_Terminals
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   --  Count_Terminals must return Integer for Get_Terminals,
   --  Positive_Index_Type for Get_Terminal_IDs.
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return 1;

      when Nonterm =>
         return Result : Natural := 0 do
            for C of Node.Children loop
               --  This can be called to build a debugging image while editing the tree
               if C /= null then
                  Result := Result + Count_Terminals (Tree, C);
               end if;
            end loop;
         end return;
      end case;
   end Count_Terminals;

   function Current_Error_Ref_No_Search
     (Tree             : in Syntax_Trees.Tree;
      Stream           : in Stream_ID;
      Terminal_Predicate  : in Error_Predicate;
      Nonterm_Predicate : in Error_Predicate)
     return Stream_Error_Ref
   is
      --  IMPROVEME: move Parse_Error etc into syntax_trees so we don't have
      --  to pass predicates here.

      use Stream_Element_Lists;
      use all type Error_Data_Lists.Cursor;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      return Result : Stream_Error_Ref do

         --  First look in Parse_Stream input.
         Result.Ref.Ref.Stream  := Stream;
         Result.Ref.Ref.Element := (Cur => Next (Parse_Stream.Stack_Top));

         if Result.Ref.Ref.Element.Cur /= No_Element then
            Result.Ref.Ref.Node := Element (Result.Ref.Ref.Element.Cur).Node;
            if Result.Ref.Ref.Node.Label = Nonterm then
               Tree.First_Terminal (Result.Ref, Following => False);
            end if;
            Result.Error := Find_Match (Result.Ref.Ref.Node.Error_List, Terminal_Predicate);
         end if;

         if Result.Error = Error_Data_Lists.No_Element then
            --  Try Shared_Stream
            Result.Ref.Ref :=
              (Stream  => Tree.Shared_Stream,
               Element => (Cur => Parse_Stream.Shared_Link),
               Node    => Element (Parse_Stream.Shared_Link).Node);

            if Result.Ref.Ref.Node.Label = Nonterm then
               Tree.First_Terminal (Result.Ref, Following => False);
            end if;
            Result.Error := Find_Match (Result.Ref.Ref.Node.Error_List, Terminal_Predicate);
         end if;

         if Result.Error = Error_Data_Lists.No_Element then
            --  Must be an In_Parse_Error on the last reduce.
            Result.Ref.Ref := (Stream, (Cur => Parse_Stream.Stack_Top), Element (Parse_Stream.Stack_Top).Node);

            if Result.Ref.Ref.Node /= Syntax_Trees.Invalid_Node_Access then
               Result.Error := Find_Match (Result.Ref.Ref.Node.Error_List, Nonterm_Predicate);
            end if;
         end if;

         if Result.Error = Error_Data_Lists.No_Element then
            raise SAL.Programmer_Error with "current_error_ref: no matching error found";
         end if;
      end return;
   end Current_Error_Ref_No_Search;

   function Current_Error_Ref
     (Tree                : in Syntax_Trees.Tree;
      Stream              : in Stream_ID;
      Terminal_Predicate  : in Error_Predicate;
      Nonterm_Predicate   : in Error_Predicate;
      Error_Node_Features : in Syntax_Trees.Error_Node_Features := (others => <>))
     return Stream_Error_Ref
   is
      --  IMPROVEME: move Parse_Error etc into syntax_trees so we don't have
      --  to pass predicates here.

      use Stream_Element_Lists;
      use all type Error_Data_Lists.Cursor;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Last_Terminal  : Stream_Node_Parents;
      First_Terminal : Stream_Node_Parents;
      --  Of the current search node

      Target_Seq_Index : constant Base_Sequential_Index :=
        (if Error_Node_Features.Deleted
         then Error_Node_Features.Prev_Term_Seq_Index
         else Error_Node_Features.Seq_Index);

      Last_Step_Towards_EOI : Boolean;

      procedure Step (Ref : in out Stream_Node_Parents; Towards_EOI : in Boolean)
      --  Step Ref.Ref.Element in direction given by Towards_EOI, skipping empty nonterms.
      is begin
         Last_Step_Towards_EOI := Towards_EOI;
         loop
            if Towards_EOI then
               Next (Ref.Ref.Element.Cur);

               if Ref.Ref.Element.Cur = Stream_Element_Lists.No_Element then
                  if Ref.Ref.Stream /= Tree.Shared_Stream then
                     --  Try Shared_Stream
                     Ref.Ref.Stream  := Tree.Shared_Stream;
                     Ref.Ref.Element := (Cur => Parse_Stream.Shared_Link);
                  else
                     raise SAL.Programmer_Error with "current_error_ref: no matching error found";
                  end if;
               end if;
            else
               Previous (Ref.Ref.Element.Cur);
               if Ref.Ref.Element.Cur = Stream_Element_Lists.No_Element then
                  raise SAL.Programmer_Error with "current_error_ref: no matching error found";
               end if;
            end if;

            Ref.Ref.Node := Element (Ref.Ref.Element.Cur).Node;
            Ref.Parents  := Node_Stacks.Empty_Stack;

            Last_Terminal := Ref;
            Tree.Last_Sequential_Terminal (Last_Terminal, Stream, Preceding => False);
            exit when Last_Terminal.Ref.Node /= Invalid_Node_Access;
         end loop;
         First_Terminal := Ref;
         Tree.First_Sequential_Terminal (First_Terminal, Following => False);
      end Step;

      function Handle_Deleted (Result : in out Stream_Error_Ref) return Boolean
      is begin
         if Result.Ref.Ref.Node.Label = Source_Terminal and then
           Result.Ref.Ref.Node.Sequential_Index = Error_Node_Features.Prev_Term_Seq_Index
         then
            for Cur in Result.Ref.Ref.Node.Following_Deleted.Iterate loop
               declare
                  Node : Valid_Node_Access renames Result.Ref.Ref.Node.Following_Deleted (Cur);
               begin
                  if Node.Sequential_Index = Error_Node_Features.Seq_Index then
                     Result.Error   := Find_Match (Node.Error_List, Terminal_Predicate);
                     Result.Deleted := Cur;
                     return True;
                  end if;
               end;
            end loop;
            return True;
         else
            return False;
         end if;
      end Handle_Deleted;

      procedure Set_First_Last (Ref : in Stream_Node_Parents)
      --  Set First_, Last_Terminal; null if Ref.Ref.Node is an empty nonterm.
      is begin
         Last_Terminal := Ref;
         Tree.Last_Sequential_Terminal (Last_Terminal, Stream, Preceding => False);
         First_Terminal := Ref;
         Tree.First_Sequential_Terminal (First_Terminal, Following => False);
      end Set_First_Last;

      function First_Child (Ref : in out Stream_Node_Parents) return Boolean
      with Pre => Ref.Ref.Node.Label = Nonterm
      --  If Ref.Ref.Node is an empty nonterm, return False. Otherwise,
      --  update Ref first child of Ref.Ref.Node, return True.
      is begin
         if Ref.Ref.Node.Child_Count = 0 then
            return False;
         end if;

         Ref.Parents.Push (Ref.Ref.Node);
         Ref.Ref.Node := Ref.Ref.Node.Children (1);
         return True;
      end First_Child;

      function Next_Sibling (Ref : in out Stream_Node_Parents) return Boolean
      --  Step Ref to next sibling of Ref.Ref.Node. If Ref.Ref.Node is last
      --  child of parent, leave Ref at next sibling of parent. Do not step
      --  Ref.Ref.Element. Return True if there is such a sibling, False
      --  when not (ie ref.parents is or becomes empty).
      is begin
         loop
            if Ref.Parents.Is_Empty then
               return False;
            end if;

            declare
               Child_Index : constant SAL.Peek_Type := Syntax_Trees.Child_Index
                 (Parent => Ref.Parents.Peek.all, Child => Ref.Ref.Node);
            begin
               if Child_Index = Ref.Parents.Peek.Child_Count then
                  Ref.Ref.Node := Ref.Parents.Pop;
               else
                  Ref.Ref.Node := Ref.Parents.Peek.Children (Child_Index + 1);
                  return True;
               end if;
            end;
         end loop;
      end Next_Sibling;

   begin
      if Error_Node_Features.Seq_Index = Invalid_Sequential_Index then
         return Current_Error_Ref_No_Search (Tree, Stream, Terminal_Predicate, Nonterm_Predicate);
      end if;

      return Result : Stream_Error_Ref do
         --  In the worst case, the error node can be anywhere in the stack or
         --  stream input. In the stack, it can be on an arbitrary descendent
         --  of the element root. In the stream input, it can be on the first
         --  terminal of a nonterm.
         --
         --  So we assume the worst case, and use first/last terminal
         --  sequential_index to guide the search when descenting a subtree.

         Result.Ref.Ref := (Stream, (Cur => Parse_Stream.Stack_Top), Element (Parse_Stream.Stack_Top).Node);
         Last_Terminal  := Result.Ref;
         Tree.Last_Sequential_Terminal (Last_Terminal, Stream, Preceding => False);
         if Last_Terminal.Ref.Node = Invalid_Node_Access then
            Step (Result.Ref, Towards_EOI => False);
         else
            First_Terminal := Result.Ref;
            Tree.First_Sequential_Terminal (First_Terminal, Following => False);
         end if;

         Search_Stream :
         loop
            if Target_Seq_Index < First_Terminal.Ref.Node.Sequential_Index then
               Step (Result.Ref, Towards_EOI => False);

            elsif Target_Seq_Index > Last_Terminal.Ref.Node.Sequential_Index then
               Step (Result.Ref, Towards_EOI => True);

            else
               Search_Subtree :
               loop
                  case Error_Node_Features.Label is
                  when Terminal_Label =>
                     exit Search_Stream when Error_Node_Features.Deleted and then Handle_Deleted (Result);

                     if Result.Ref.Ref.Node.Label = Error_Node_Features.Label and then
                       Result.Ref.Ref.Node.Sequential_Index = Target_Seq_Index
                     then
                        Result.Error := Find_Match (Result.Ref.Ref.Node.Error_List, Terminal_Predicate);
                        exit Search_Stream;
                     end if;

                  when Nonterm =>
                     if Result.Ref.Ref.Node.Label = Error_Node_Features.Label and then
                       First_Terminal.Ref.Node.Sequential_Index = Target_Seq_Index
                     then
                        Result.Error := Find_Match (Result.Ref.Ref.Node.Error_List, Nonterm_Predicate);
                        exit Search_Stream when Result.Error /= Error_Data_Lists.No_Element;
                     end if;
                  end case;

                  --  Continue searching
                  case Result.Ref.Ref.Node.Label is
                  when Terminal_Label =>
                     exit Search_Subtree when not Next_Sibling (Result.Ref);

                  when Nonterm =>
                     if First_Child (Result.Ref) then

                        Find_Child :
                        loop
                           Set_First_Last (Result.Ref);

                           if Last_Terminal.Ref.Node /= Invalid_Node_Access and then
                             Target_Seq_Index <= Last_Terminal.Ref.Node.Sequential_Index
                           then
                              --  Search this child
                              exit Find_Child;
                           else
                              exit Search_Subtree when not Next_Sibling (Result.Ref);
                           end if;
                        end loop Find_Child;

                     else
                        exit Search_Subtree when not Next_Sibling (Result.Ref);
                     end if;
                  end case;
               end loop Search_Subtree;

               Step (Result.Ref, Last_Step_Towards_EOI);
            end if;

         end loop Search_Stream;

         if Result.Error = Error_Data_Lists.No_Element then
            raise SAL.Programmer_Error with "current_error_ref: no matching error found";
         end if;
      end return;
   end Current_Error_Ref;

   function Current_Token
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last then
         return
           (Stream  => Tree.Shared_Stream,
            Element => (Cur => Parse_Stream.Shared_Link),
            Node    => Stream_Element_Lists.Element (Parse_Stream.Shared_Link).Node);
      else
         declare
            El : constant Stream_Element_Lists.Cursor := Stream_Element_Lists.Next (Parse_Stream.Stack_Top);
         begin
            return
              (Stream  => Stream,
               Element => (Cur => El),
               Node    => Stream_Element_Lists.Element (El).Node);
         end;
      end if;
   end Current_Token;

   procedure Delete_Current_Token
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last then
         --  Input is Shared_Link
         Stream_Element_Lists.Next (Parse_Stream.Shared_Link);
      else
         --  Input is Stream input.
         declare
            use Stream_Element_Lists;
            To_Delete : Cursor := Next (Parse_Stream.Stack_Top);
         begin
            --  Any Non_Grammar on To_Delete should be moved in Delete_Token, called
            --  by Execute_Actions after parse is complete.
            Parse_Stream.Elements.Delete (To_Delete);
         end;
      end if;
   end Delete_Current_Token;

   procedure Delete_Error
     (Tree  : in out Syntax_Trees.Tree;
      Error : in out Error_Ref)
   is
      Error_Node : constant Valid_Node_Access :=
        (if Error.Deleted = Valid_Node_Access_Lists.No_Element
         then Error.Node
         else Error.Node.Following_Deleted (Error.Deleted));

      To_Delete : Error_Data_Lists.Cursor := Error.Error;
   begin
      Tree.Next_Error (Error);
      Error_Node.Error_List.Delete (To_Delete);
      if Error_Node.Error_List.Length = 0 then
         Free (Error_Node.Error_List);
      end if;
   end Delete_Error;

   procedure Delete_Error
     (Tree  : in out Syntax_Trees.Tree;
      Error : in out Stream_Error_Ref)
   is
      Error_Node : constant Valid_Node_Access :=
        (if Error.Deleted = Valid_Node_Access_Lists.No_Element
         then Error.Ref.Ref.Node
         else Error.Ref.Ref.Node.Following_Deleted (Error.Deleted));

      To_Delete : Error_Data_Lists.Cursor := Error.Error;
   begin
      Tree.Next_Error (Error);
      Error_Node.Error_List.Delete (To_Delete);
      if Error_Node.Error_List.Length = 0 then
         Free (Error_Node.Error_List);
      end if;
   end Delete_Error;

   procedure Delete_Errors_In_Input
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Predicate : in     Error_Predicate;
      User_Data : in     User_Data_Access_Constant)
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Error_Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last then
           (Stream  => Tree.Shared_Stream,
            Element => (Cur => Parse_Stream.Shared_Link),
            Node    => Element (Parse_Stream.Shared_Link).Node)
         else
           (Stream  => Stream,
            Element => (Cur => Next (Parse_Stream.Stack_Top)),
            Node    => Element (Next (Parse_Stream.Stack_Top)).Node));

      procedure Delete_Errors
      --  Delete errors matching Predicate from Error_Ref.Ref.Node
      is
         function Edit_Error_List return Error_List_Access
         is begin
            return Result : Error_List_Access := new Error_Data_Lists.List'(Error_Ref.Ref.Node.Error_List.all)
            do
               declare
                  use Error_Data_Lists;
                  Cur       : Error_Data_Lists.Cursor := Result.First;
                  To_Delete : Error_Data_Lists.Cursor;
               begin
                  loop
                     exit when Cur = Error_Data_Lists.No_Element;
                     if Predicate (Cur) then
                        To_Delete := Cur;
                        Next (Cur);
                        Result.Delete (To_Delete);
                     else
                        Next (Cur);
                     end if;
                  end loop;
               end;
               if Result.Length = 0 then
                  Free (Result);
               end if;
            end return;
         end Edit_Error_List;
      begin
         if Error_Ref.Ref.Node.Error_List /= null and then Error_Ref.Ref.Node.Error_List.Length > 0 then
            declare
               New_Node : constant Valid_Node_Access := Copy_Node
                 (Tree, Error_Ref.Ref.Node,
                  Parent         =>
                    (if Tree.Parents_Set
                     then Error_Ref.Ref.Node.Parent
                     else Invalid_Node_Access),
                  User_Data              => User_Data,
                  Copy_Children          => False,
                  Copy_Following_Deleted => True,
                  New_Error_List         => Edit_Error_List,
                  Set_Error_List         => True);
            begin
               Move_Element (Tree, Stream, Error_Ref, New_Node, User_Data);
            end;
         end if;

         case Error_Ref.Ref.Node.Label is
         when Terminal_Label =>
            null;

         when Nonterm =>
            declare
               Children : constant Node_Access_Array := Error_Ref.Ref.Node.Children;
            begin
               for Child of Children loop
                  Error_Ref.Parents.Push (Error_Ref.Ref.Node);
                  Error_Ref.Ref.Node := Child;
                  Delete_Errors;
               end loop;
            end;
         end case;

         if Error_Ref.Parents.Depth > 0 then
            Error_Ref.Ref.Node := Error_Ref.Parents.Pop;
         end if;
      end Delete_Errors;
   begin
      --  IMPROVEME incremental: This algorithm is not incremental, and a
      --  waste of time on almost all nonterms; cache Has_Errors Boolean in
      --  each nonterm.
      Delete_Errors;
   end Delete_Errors_In_Input;

   procedure Delete_Stream (Tree : in out Syntax_Trees.Tree; Stream : in out Stream_ID)
   is
      use Parse_Stream_Lists;
   begin
      declare
         Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      begin
         Parse_Stream.Stack_Top   := Stream_Element_Lists.No_Element;
         Parse_Stream.Shared_Link := Stream_Element_Lists.No_Element;
      end;
      Tree.Streams.Delete (Stream.Cur);
   end Delete_Stream;

   procedure Delete_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in out Node_Access)
   is
      procedure Delete_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in out Node_Access)
      is begin
         Free (Node.Augmented);
         Free (Node.Error_List);

         case Node.Label is
         when Source_Terminal =>
            Node.Following_Deleted.Clear;

         when Virtual_Terminal | Virtual_Identifier =>
            null;

         when Nonterm =>
            for I in Node.Children'Range loop
               Delete_Node (Tree, Node.Children (I));
            end loop;

         end case;
         Free (Node);
      end Delete_Node;
   begin
      if Root = Invalid_Node_Access then
         null;
      else
         Delete_Node (Tree, Root);
      end if;
   end Delete_Subtree;

   function Editable (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Parents_Set and Tree.Streams.Length = 0 and Tree.Shared_Stream.Cur = Parse_Stream_Lists.No_Element;
   end Editable;

   function Element_ID
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Item.Virtual
         then Item.ID
         else Item.Element_Node.ID);
   end Element_ID;

   function Element_Is_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Boolean
   is begin
      if Item.Virtual then
         return Is_Terminal (Item.ID, Tree.Lexer.Descriptor.all);
      else
         return Tree.Label (Item.Element_Node) in Terminal_Label;
      end if;
   end Element_Is_Terminal;

   function Empty_Line
     (Tree        : in Syntax_Trees.Tree;
      Non_Grammar : in Lexer.Token_Arrays.Vector;
      Line        : in Line_Number_Type)
     return Boolean
   with Pre =>
     (for some Token of Non_Grammar => Contains (Token.Line_Region, Line) and
        New_Line_Count (Token.Line_Region) > 0)
   --  Return True if Line in Non_Grammar contains no non_grammar tokens
   --  other than New_Line or EOI.
   is begin
      for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
         if Contains (Non_Grammar (I).Line_Region, Line) and New_Line_Count (Non_Grammar (I).Line_Region) > 0 then
            declare
               Line_Begin_Char_Pos : constant Base_Buffer_Pos := Tree.Lexer.Line_Begin_Char_Pos (Non_Grammar (I), Line);
            begin
               if Line_Begin_Char_Pos /= Invalid_Buffer_Pos then
                  if Non_Grammar (I).Char_Region.Last + 1 > Line_Begin_Char_Pos then
                     return True;

                  else
                     pragma Assert (Non_Grammar (I).Char_Region.Last + 1 = Line_Begin_Char_Pos);
                     return I < Non_Grammar.Last_Index and then
                       not (Non_Grammar (I + 1).ID in Tree.Lexer.Descriptor.New_Line_ID | Tree.Lexer.Descriptor.EOI_ID);
                  end if;
               end if;
            end;
         end if;
      end loop;
      --  Getting here violates the precondition
      raise SAL.Programmer_Error;
   end Empty_Line;

   procedure Enable_Ref_Count_Check (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID; Enable : in Boolean)
   is begin
      Tree.Streams (Stream.Cur).Elements.Enable_Ref_Count_Check (Enable);
   end Enable_Ref_Count_Check;

   function EOI (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      return Tree.EOI;
   end EOI;

   function Error (Item : in Error_Ref) return Error_Data'Class
   is begin
      return Error_Data_Lists.Element (Item.Error);
   end Error;

   function Error (Item : in Stream_Error_Ref) return Error_Data'Class
   is begin
      return Error_Data_Lists.Element (Item.Error);
   end Error;

   function Error (Item : in Stream_Error_Cursor) return Stream_Error_Ref
   is begin
      return Item.SER;
   end Error;

   function Error_Count (Tree : in Syntax_Trees.Tree) return Ada.Containers.Count_Type
   is
      Error : Error_Ref := Tree.First_Error;
   begin
      return Result : Ada.Containers.Count_Type := 0 do
         loop
            exit when Error.Node = Invalid_Node_Access;
            Result := @ + 1;
            Tree.Next_Error (Error);
         end loop;
      end return;
   end Error_Count;

   function Error_Count (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Ada.Containers.Count_Type
   is begin
      return Result : Ada.Containers.Count_Type := 0 do
         for Cur in Tree.Stream_Error_Iterate (Stream) loop
            Result := @ + 1;
         end loop;
      end return;
   end Error_Count;

   function Error_Deleted (Error : in Stream_Error_Ref) return Valid_Node_Access_Lists.Cursor
   is begin
      return Error.Deleted;
   end Error_Deleted;

   function Error_Iterate
     (Tree : aliased in Syntax_Trees.Tree)
     return Error_Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Error_Iterator'(Tree => Tree'Access);
   end Error_Iterate;

   function Error_List (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Error_Data_List_Const_Ref
   is begin
      return Error_Data_List_Const_Ref'
        (List  => (if Node.Error_List = null then Empty_Error_List'Access else Node.Error_List),
         Dummy => 1);
   end Error_List;

   function Error_Message_1
     (Tree             : in Syntax_Trees.Tree;
      Prev_Non_Grammar : in WisiToken.Lexer.Token_Arrays.Vector;
      First_Terminal   : in Node_Access;
      Message          : in String)
     return String
   is
      Line   : Line_Number_Type  := Line_Number_Type'First;
      Column : Ada.Text_IO.Count := Ada.Text_IO.Count'First;
   begin
      if Prev_Non_Grammar.Length > 0 then
         Line := Prev_Non_Grammar (Prev_Non_Grammar.Last_Index).Line_Region.Last;

         if First_Terminal /= Invalid_Node_Access then
            if First_Terminal.Label = Source_Terminal and then
              First_Terminal.Char_Region.First /= Invalid_Buffer_Pos
            then
               declare
                  Begin_Char_Pos : constant Buffer_Pos :=
                    (if Tree.Editable
                     then Tree.Line_Begin_Char_Pos (Line)
                     else Tree.Line_Begin_Char_Pos (Line, Tree.Shared_Stream));
               begin
                  Column :=
                    (if Begin_Char_Pos = Invalid_Buffer_Pos or
                       First_Terminal.Char_Region.First < Begin_Char_Pos
                     then 0
                     else Ada.Text_IO.Count (First_Terminal.Char_Region.First - Begin_Char_Pos));
               end;
            end if;
         else
            --  No char_pos, so no column
            null;
         end if;

      else
         --  No line information, so also no column.
         null;
      end if;
      return WisiToken.Error_Message (Tree.Lexer.File_Name, Line, Column, Message);
   end Error_Message_1;

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Access;
      Message : in String)
     return String
   is
      Non_Grammar : Node_Access := Invalid_Node_Access;
      Null_Non_Grammar : WisiToken.Lexer.Token_Arrays.Vector;
   begin
      begin
         --  Tolerate broken trees where Prev_Non_Grammar doesn't find SOI, or
         --  raises an exception.
         Non_Grammar := Tree.Prev_Non_Grammar (Node);
      exception
      when others =>
         null;
      end;
      return Error_Message_1
        (Tree,
         (if Non_Grammar = Invalid_Node_Access then Null_Non_Grammar else Non_Grammar.Non_Grammar),
         Tree.First_Terminal (Node), Message);
   end Error_Message;

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Ref     : in Stream_Node_Ref;
      Message : in String)
     return String
   is begin
      if Tree.Parents_Set then
         declare
            Non_Grammar    : Stream_Node_Ref := Ref;
            First_Terminal : Stream_Node_Ref := Ref;
         begin
            Tree.Prev_Non_Grammar (Non_Grammar);
            Tree.First_Terminal (First_Terminal);
            return Error_Message_1 (Tree, Non_Grammar.Node.Non_Grammar, First_Terminal.Node, Message);
         end;
      elsif Rooted (Ref) then
         declare
            Non_Grammar    : Stream_Node_Parents := (Ref, Parents => <>);
            First_Terminal : Stream_Node_Parents := (Ref, Parents => <>);
         begin
            Tree.Prev_Non_Grammar (Non_Grammar, Parse_Stream => Invalid_Stream_ID);
            Tree.First_Terminal (First_Terminal, Following => True);
            return Error_Message_1 (Tree, Non_Grammar.Ref.Node.Non_Grammar, First_Terminal.Ref.Node, Message);
         end;

      else
         declare
            Non_Grammar    : constant Node_Access := Tree.First_Non_Grammar  (Ref.Node);
            First_Terminal : constant Node_Access := Tree.First_Terminal (Ref.Node);
         begin
            return Error_Message_1
              (Tree,
               (if Non_Grammar = Invalid_Node_Access
                then Lexer.Token_Arrays.Empty_Vector
                else Non_Grammar.Non_Grammar),
               First_Terminal,
               Message);
         end;
      end if;
   end Error_Message;

   function Error_Node (Error : in Error_Ref) return Valid_Node_Access
   is begin
      if Valid_Node_Access_Lists.Has_Element (Error.Deleted) then
         pragma Assert (Error.Node.Label = Source_Terminal);
         return Error.Node.Following_Deleted (Error.Deleted);

      else
         return Error.Node;
      end if;
   end Error_Node;

   function Error_Node (Tree : in Syntax_Trees.Tree; Error : in Error_Ref) return Valid_Node_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Error_Node (Error);
   end Error_Node;

   function Error_Node (Error : in Stream_Error_Ref) return Valid_Node_Access
   is begin
      return Error_Node
        (Error_Ref'
           (Node    => Error.Ref.Ref.Node,
            Deleted => Error.Deleted,
            Error   => Error.Error));
   end Error_Node;

   function Error_Node (Tree : in Syntax_Trees.Tree; Error : in Stream_Error_Ref) return Valid_Node_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Error_Node (Error);
   end Error_Node;

   function Error_Stream_Node_Ref (Tree : in Syntax_Trees.Tree; Error : in Stream_Error_Ref) return Stream_Node_Ref
   is begin
      return
        (Stream        => Error.Ref.Ref.Stream,
         Element       => Error.Ref.Ref.Element,
         Node          => Tree.Error_Node
           (Error_Ref'
              (Node    => Error.Ref.Ref.Node,
               Deleted => Error.Deleted,
               Error   => Error.Error)));
   end Error_Stream_Node_Ref;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree)
   is begin
      Clear (Tree, Free_Memory => True);
      --  Tree.* array memory is freed by SAL Vectors Finalize.
   end Finalize;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      ID         : in Token_ID;
      Max_Parent : in Boolean := False)
     return Node_Access
   is
      N           : Node_Access := Node;
      Last_Parent : Node_Access := Invalid_Node_Access;
   begin
      loop
         N := N.Parent;

         exit when N = Invalid_Node_Access;
         Last_Parent := N;

         exit when ID = N.ID;
      end loop;

      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Access
   is
      N           : Node_Access := Node;
      Last_Parent : Node_Access := Invalid_Node_Access;
   begin
      loop
         N := N.Parent;

         exit when N = Invalid_Node_Access;
         Last_Parent := N;

         exit when (for some ID of IDs => ID = N.ID);
      end loop;

      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Byte_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Byte_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   --  Return terminal node in subtree under Node that contains
   --  (including non_grammar if Trailing_Non_Grammar) or is after
   --  Byte_Pos. Invalid_Node_Access if byte_Pos is after all of Node.
   is begin
      case Node.Label is
      when Source_Terminal =>
         if Byte_Pos <= Node.Byte_Region.Last then
            return Node;
         elsif (Trailing_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last
         then
            return Node;
         elsif Node.ID = Tree.Lexer.Descriptor.EOI_ID and Byte_Pos = Node.Byte_Region.First then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if (Trailing_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         for Child of Node.Children loop
            declare
               Region : constant Buffer_Region := Tree.Byte_Region (Child, Trailing_Non_Grammar);
            begin
               if Region = Null_Buffer_Region then
                  --  Child is empty or virtual; try next
                  null;
               elsif Byte_Pos <= Region.First then
                  return Tree.First_Terminal (Child);
               elsif Byte_Pos <= Region.Last then
                  return Find_Byte_Pos (Tree, Child, Byte_Pos, Trailing_Non_Grammar);
               else
                  null; -- try next child
               end if;
            end;
         end loop;
         --  Byte_Pos is after last child
         return Invalid_Node_Access;
      end case;
   end Find_Byte_Pos;

   function Find_Byte_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Byte_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      Node        : constant Node_Access   := Root (Tree);
      Byte_Region : constant Buffer_Region :=
        (First => Tree.SOI.Non_Grammar (1).Byte_Region.First,
         Last  => Tree.EOI.Non_Grammar (1).Byte_Region.First);
   begin
      if Byte_Pos <= Byte_Region.First then
         return Tree.First_Terminal (Node);
      elsif Byte_Pos > Byte_Region.Last then
         return Invalid_Node_Access;
      else
         return Find_Byte_Pos (Tree, Node, Byte_Pos, Trailing_Non_Grammar);
      end if;
   end Find_Byte_Pos;

   function Find_Byte_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Byte_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean;
      Start_At             : in Terminal_Ref;
      Stream               : in Stream_ID := Invalid_Stream_ID)
     return Terminal_Ref
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams
        (if Start_At = Invalid_Stream_Node_Ref
         then Stream.Cur
         else Start_At.Stream.Cur);

      function Find_Parent return Stream_Node_Ref
      is
         Node : Node_Access := Start_At.Node;
      begin
         loop
            if Node.Parent = null then
               declare
                  Cur : constant Stream_Element_Lists.Cursor := Stream_Element_Lists.Next (Start_At.Element.Cur);
               begin
                  if Stream_Element_Lists.Has_Element (Cur) then
                     return Tree.To_Rooted_Ref (Start_At.Stream, (Cur => Cur));
                  else
                     return Invalid_Stream_Node_Ref;
                  end if;
               end;

            elsif Contains (Tree.Byte_Region (Node.Parent, Trailing_Non_Grammar => False), Byte_Pos) then
               return (Start_At.Stream, Start_At.Element, Node.Parent);

            else
               Node := Node.Parent;
            end if;
         end loop;
      end Find_Parent;

      Result : Stream_Node_Ref :=
        (if Start_At = Invalid_Stream_Node_Ref
         then (Stream, (Cur => Parse_Stream.Elements.First), null)
         else Find_Parent);
   begin
      loop
         exit when Result.Element.Cur = No_Element;

         Result.Node := Find_Byte_Pos
           (Tree, Stream_Element_Lists.Element (Result.Element.Cur).Node, Byte_Pos, Trailing_Non_Grammar);
         if Result.Node = Invalid_Node_Access then
            --  Try next stream element
            Result.Element.Cur := Next (Result.Element.Cur);
         else
            return Result;
         end if;
      end loop;
      --  end of stream reached; Byte_Pos is after all of Stream
      return Result;
   end Find_Byte_Pos;

   function Find_Char_Pos
     (Tree                : in Syntax_Trees.Tree;
      Node                : in Node_Access;
      Char_Pos            : in Buffer_Pos;
      Include_Non_Grammar : in Boolean;
      After               : in Boolean)
     return Node_Access
   is begin
      case Node.Label is
      when Source_Terminal =>
         if Contains (Node.Char_Region, Char_Pos) then
            return Node;
         elsif After and Char_Pos <= Node.Char_Region.Last then
            return Node;
         elsif (Include_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Char_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if (Include_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Char_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         for Child of Node.Children loop
            declare
               Region : constant Buffer_Region := Tree.Char_Region (Child, Include_Non_Grammar);
            begin
               if Length (Region) = 0 then
                  --  Child is empty or all virtual; try next
                  null;
               elsif Char_Pos <= Region.Last then
                  declare
                     Result : constant Node_Access := Find_Char_Pos
                       (Tree, Child, Char_Pos, Include_Non_Grammar, After);
                  begin
                     if Tree.ID (Child) = Tree.Lexer.Descriptor.SOI_ID then
                        pragma Assert (Child.Label = Source_Terminal);
                        --  SOI does not have an empty region (see comment in Start_Lex), but
                        --  we define it to not contain any text. EOI does have an empty
                        --  region.
                        if Char_Pos < Child.Char_Region.Last then
                           if After then
                              return Child;
                           else
                              return Invalid_Node_Access;
                           end if;

                        elsif (Include_Non_Grammar and Child.Non_Grammar.Length > 1) and then
                          Char_Pos <= Child.Non_Grammar (Child.Non_Grammar.Last_Index).Char_Region.Last
                        then
                           return Child;
                        else
                           null; -- try next child
                        end if;

                     else
                        return Result;
                     end if;
                  end;

               else
                  null; -- try next child
               end if;
            end;
         end loop;
         --  Char_Pos is after last child
         return Invalid_Node_Access;
      end case;
   end Find_Char_Pos;

   function Find_Char_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Char_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean;
      After                : in Boolean := False)
     return Node_Access
   is
      Node : constant Node_Access := Root (Tree);
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node, Trailing_Non_Grammar);
   begin
      if Char_Pos < Char_Region.First then
         if After then
            return Tree.First_Terminal (Node);
         else
            return Invalid_Node_Access;
         end if;

      elsif Char_Pos > Char_Region.Last then
         return Invalid_Node_Access;

      else
         return Find_Char_Pos (Tree, Node, Char_Pos, Trailing_Non_Grammar, After);
      end if;
   end Find_Char_Pos;

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Invalid_Node_Access;
      when Nonterm =>
         for C of Node.Children loop
            if C /= null then
               if ID = C.ID then
                  return C;
               end if;
            end if;
         end loop;
         return Invalid_Node_Access;
      end case;
   end Find_Child;

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is
      Found : Node_Access := Invalid_Node_Access;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
      is
         pragma Unreferenced (Tree);
      begin
         if Node.ID = ID then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_Descendant
     (Tree      : in     Syntax_Trees.Tree;
      Node      : in     Valid_Node_Access;
      Predicate : access function (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean)
     return Node_Access
   is
      Found : Node_Access := Invalid_Node_Access;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
      is begin
         if Predicate (Tree, Node) then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_Match
     (Error_List : in Error_List_Access;
      Predicate  : in Error_Predicate)
     return Error_Data_Lists.Cursor
   is
      use Error_Data_Lists;
   begin
      if Error_List = null or Predicate = null then
         return No_Element;
      else
         return Result : Cursor := Error_List.First do
            loop
               exit when Result = No_Element;
               exit when Predicate (Result);
               Next (Result);
            end loop;
         end return;
      end if;
   end Find_Match;

   function Find_New_Line
     (Tree     : in     Syntax_Trees.Tree;
      Line     : in     Line_Number_Type;
      Node     : in     Node_Access;
      Char_Pos :    out Buffer_Pos)
     return Node_Access
   with Pre => Line > Line_Number_Type'First and Tree.Parents_Set
   --  Return node under Node that contains the non-grammar containing a
   --  new_line or EOI that ends Line - 1. Update Char_Pos to the
   --  position of the first character on Line (or EOI). If not found,
   --  result is Invalid_Node_Access, Char_Pos is Invalid_Buffer_Pos.
   is begin
      Char_Pos := Invalid_Buffer_Pos;

      if Node = Invalid_Node_Access then
         return Invalid_Node_Access;
      end if;

      case Node.Label is
      when Terminal_Label =>
         if Node.ID = Tree.Lexer.Descriptor.EOI_ID and then
           --  Handle 'Line' as well as 'Line - 1' to allow returning char_pos
           --  for the last line.
           Node.Non_Grammar (Node.Non_Grammar.First_Index).Line_Region.First in Line - 1 | Line
         then
            Char_Pos := Node.Char_Region.First;
            return Node;

         elsif Check_Non_Grammar (Tree, Node, Line, Char_Pos) then
            return Node;

         elsif Tree.Lexer.Can_Contain_New_Line (Node.ID) and then
           Check_Multi_Line (Tree, Node, Line, Char_Pos, Start_Line => Tree.Prev_New_Line (Node).Line)
         then
            return Node;

         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         declare
            Node_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
              (Node, Trailing_Non_Grammar => True);
         begin
            if Node.Child_Count = 0 then
               --  This must be an empty stream element.
               return Invalid_Node_Access;

            elsif Line - 1 in Node_Line_Region.First .. Node_Line_Region.Last then
               if Line - 1 = Node_Line_Region.Last then
                  --  Faster to check last child first.
                  for I in reverse Node.Children'Range loop
                     declare
                        Temp : constant Node_Access := Find_New_Line
                          (Tree, Line, Node.Children (I), Char_Pos);
                     begin
                        if Temp = Invalid_Node_Access then
                           if I = Node.Children'First then
                              return Invalid_Node_Access;
                           else
                              --  Check next child
                              null;
                           end if;
                        else
                           return Temp;
                        end if;
                     end;
                  end loop;
                  return Invalid_Node_Access;
               else
                  for I in Node.Children'Range loop
                     declare
                        Temp : constant Node_Access := Find_New_Line (Tree, Line, Node.Children (I), Char_Pos);
                     begin
                        if Temp = Invalid_Node_Access then
                           if I = Node.Children'Last then
                              return Invalid_Node_Access;
                           else
                              --  Check next child
                              null;
                           end if;
                        else
                           return Temp;
                        end if;
                     end;
                  end loop;
                  return Invalid_Node_Access;
               end if;
            else
               return Invalid_Node_Access;
            end if;
         end;
      end case;
   end Find_New_Line;

   function Find_New_Line
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   is
      Char_Pos : Buffer_Pos;
   begin
      return Find_New_Line (Tree, Line, Tree.Root, Char_Pos);
   end Find_New_Line;

   function Find_New_Line
     (Tree                : in     Syntax_Trees.Tree;
      Line                : in     Line_Number_Type;
      Line_Begin_Char_Pos :    out Buffer_Pos)
     return Node_Access
   is begin
      if Tree.SOI.Non_Grammar (Tree.SOI.Non_Grammar.First_Index).Line_Region.Last = Line then
         Line_Begin_Char_Pos := Tree.SOI.Char_Region.First;
         return Tree.SOI;
      else
         return Find_New_Line (Tree, Line, Tree.Root, Line_Begin_Char_Pos);
      end if;
   end Find_New_Line;

   procedure Find_New_Line_1
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Line         : in     Line_Number_Type;
      Char_Pos     :    out Buffer_Pos)
   with Pre => Line > Line_Number_Type'First and Ref.Ref.Element /= Invalid_Stream_Index,
     Post => Ref.Ref.Element = Ref.Ref.Element'Old and
             (Ref.Ref.Node = Invalid_Node_Access or else
                (Ref.Ref.Node.Label in Terminal_Label))
   --  Update Ref to node under Ref.Node in Ref.Stream that contains the
   --  Non_Grammar that ends Line - 1. Set Char_Pos to the position of
   --  the first character on Line. If not found, Ref.Ref.Node is
   --  Invalid_Node_Access, Char_Pos is Invalid_Buffer_Pos.
   is
   begin
      Char_Pos := Invalid_Buffer_Pos;

      if Ref.Ref.Node = Invalid_Node_Access then
         return;
      end if;

      case Ref.Ref.Node.Label is
      when Terminal_Label =>
         if Ref.Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID and then
           --  Handle 'Line' as well as 'Line - 1' to allow returning char_pos
           --  for the last line.
           Ref.Ref.Node.Non_Grammar (Ref.Ref.Node.Non_Grammar.First_Index).Line_Region.First in Line - 1 | Line
         then
            Char_Pos := Ref.Ref.Node.Char_Region.First;
            return;
         elsif Check_Non_Grammar (Tree, Ref.Ref.Node, Line, Char_Pos) then
            return;
         else
            Ref.Ref.Node := Invalid_Node_Access;
            return;
         end if;

      when Nonterm =>
         if Ref.Ref.Node.Child_Count = 0 then
            --  This must be an empty stream element.
            Ref.Ref.Node := Invalid_Node_Access;
            return;

         else
            declare
               Node_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
                 (Ref, Parse_Stream, Trailing_Non_Grammar => True);

               function Check_Child (I : in SAL.Peek_Type; Forward : in Boolean) return Boolean
               --  True => return from Find_New_Line; False => check next child.
               is
                  Temp : Stream_Node_Parents :=
                    ((Ref.Ref.Stream, Ref.Ref.Element, Ref.Ref.Node.Children (I)),
                     Ref.Parents);
               begin
                  Temp.Parents.Push (Ref.Ref.Node);
                  Find_New_Line_1 (Tree, Temp, Parse_Stream, Line, Char_Pos);

                  if Temp.Ref.Node = Invalid_Node_Access then
                     if I = (if Forward then Ref.Ref.Node.Children'Last else Ref.Ref.Node.Children'First) then
                        Ref.Ref.Node := Invalid_Node_Access;
                        return True;
                     else
                        --  Check next child
                        return False;
                     end if;
                  else
                     Ref := Temp;
                     return True;
                  end if;
               end Check_Child;

            begin
               if Contains (Node_Line_Region, Line - 1) then
                  if Line - 1 = Node_Line_Region.Last then
                     --  Faster to check last child first.
                     for I in reverse Ref.Ref.Node.Children'Range loop
                        if Check_Child (I, Forward => False) then
                           return;
                        end if;
                     end loop;

                  else
                     for I in Ref.Ref.Node.Children'Range loop
                        if Check_Child (I, Forward => True) then
                           return;
                        end if;
                     end loop;
                  end if;
               end if;
               Ref.Ref.Node := Invalid_Node_Access;
               return;
            end;
         end if;
      end case;
   end Find_New_Line_1;

   procedure Find_New_Line
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Line         : in     Line_Number_Type;
      Char_Pos     :    out Buffer_Pos)
   with Pre => Line > Line_Number_Type'First and Ref.Parents.Is_Empty and
               Ref.Ref.Node = Stream_Element_Lists.Element (Ref.Ref.Element.Cur).Node,
     Post => Ref.Ref = Invalid_Stream_Node_Ref or else
             (Ref.Ref.Node.Label in Terminal_Label)

   --  On entry, Ref.Ref should be Stream_First (Ref.Stream). Update Ref
   --  to node in Ref.Stream or Tree.Shared_Stream that ends Line - 1 (or
   --  EOI). Set Char_Pos to the position of the first character on Line
   --  (or EOI). If not found (ie Line < SOI.Line or Line > EOI.Line),
   --  Ref.Ref is Invalid_Stream_Node_Ref, Char_Pos is
   --  Invalid_Buffer_Pos.
   is
      Start_Stream : constant Stream_ID := Ref.Ref.Stream;
   begin
      loop
         Find_New_Line_1 (Tree, Ref, Parse_Stream, Line, Char_Pos);
         if Ref.Ref = Invalid_Stream_Node_Ref then
            return;

         elsif Ref.Ref.Node = Invalid_Node_Access then
            Stream_Next (Tree, Ref, Rooted => True);

            if Ref.Ref = Invalid_Stream_Node_Ref then
               if Start_Stream /= Tree.Shared_Stream then
                  declare
                     Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Start_Stream.Cur);
                  begin
                     Ref :=
                       (Ref => (Tree.Shared_Stream, (Cur => Parse_Stream.Shared_Link), Invalid_Node_Access),
                        Parents => <>);
                     First_Terminal (Tree, Ref, Following => True);
                  end;
               else
                  return;
               end if;
            end if;
         else
            return;
         end if;
      end loop;
   end Find_New_Line;

   procedure Find_Node (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access)
   is begin
      for I in Tree.Nodes.First_Index .. Tree.Nodes.Last_Index loop
         if Tree.Nodes (I) = Node then
            Tree.Lexer.Trace.Put_Line (I'Image);
            exit;
         end if;
      end loop;
   end Find_Node;

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is begin
      if Node.Parent = Invalid_Node_Access then
         return Invalid_Node_Access;

      else
         case Node.Parent.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Invalid_Node_Access;

         when Nonterm =>
            for C of Node.Parent.Children loop
               if C /= null then
                  if ID = C.ID then
                     return C;
                  end if;
               end if;
            end loop;
            return Invalid_Node_Access;
         end case;
      end if;
   end Find_Sibling;

   overriding function First (Object : Error_Iterator) return Error_Ref
   is begin
      return First_Error (Object.Tree.all);
   end First;

   overriding function First (Object : Stream_Error_Iterator) return Stream_Error_Cursor
   is begin
      return (SER => First_Error (Object.Tree.all, (Cur => Object.Stream)));
   end First;

   procedure First_Error (Error : in out Error_Ref)
   with Pre => Error.Node /= Invalid_Node_Access
   --  Update Error to first error on or following Deleted in Node.
   is
      use Valid_Node_Access_Lists;
   begin
      loop
         if not Has_Element (Error.Deleted) and then Error.Node.Error_List /= null then
            Error.Error := Error.Node.Error_List.First;
            return;
         end if;

         loop
            exit when not Has_Element (Error.Deleted);

            declare
               Deleted_Node : Valid_Node_Access renames Error.Node.Following_Deleted (Error.Deleted);
            begin
               if Deleted_Node.Label = Source_Terminal and then
                 Deleted_Node.Error_List /= null
               then
                  Error.Error := Deleted_Node.Error_List.First;
                  return;
               end if;
            end;
            Next (Error.Deleted);
         end loop;

         Next_Node (Error.Node);

         exit when Error.Node = Invalid_Node_Access;

         if Error.Node.Error_List /= null then
            Error.Error   := Error.Node.Error_List.First;
            Error.Deleted := No_Element;
            return;
         end if;

         Error.Deleted :=
           (if Error.Node.Label = Source_Terminal
            then Error.Node.Following_Deleted.First
            else No_Element);
      end loop;
   end First_Error;

   procedure First_Error
     (Tree  : in     Syntax_Trees.Tree;
      Error : in out Stream_Error_Ref)
   with Pre => Error.Ref.Ref.Node /= Invalid_Node_Access
   --  Update Error to first error on or following
   --  Error.Ref. On enter, Error.Ref is normally stream SOI.
   is
      use Valid_Node_Access_Lists;
   begin
      loop
         if not Has_Element (Error.Deleted) and then Error.Ref.Ref.Node.Error_List /= null then
            Error.Error := Error.Ref.Ref.Node.Error_List.First;
            return;
         end if;

         loop
            exit when not Has_Element (Error.Deleted);

            declare
               Deleted_Node : Valid_Node_Access renames Error.Ref.Ref.Node.Following_Deleted (Error.Deleted);
            begin
               if Deleted_Node.Error_List /= null then
                  Error.Error := Deleted_Node.Error_List.First;
                  return;
               end if;
            end;
            Next (Error.Deleted);
         end loop;

         Next_Node (Tree, Error.Ref);
         if Error.Ref.Ref.Node = Invalid_Node_Access then
            --  No errors in tree
            return;
         end if;

         Error.Deleted :=
           (if Error.Ref.Ref.Node.Label = Source_Terminal
            then Error.Ref.Ref.Node.Following_Deleted.First
            else No_Element);
      end loop;
   end First_Error;

   function First_Error (Tree : in Syntax_Trees.Tree) return Error_Ref
   is begin
      return Result : Error_Ref := (Tree.SOI, Tree.SOI.Following_Deleted.First, Error_Data_Lists.No_Element) do
         First_Error (Result);
      end return;
   end First_Error;

   function First_Error (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Stream_Error_Ref
   is begin
      return Result : Stream_Error_Ref :=
        (Ref     => Tree.To_Stream_Node_Parents
           (Tree.To_Rooted_Ref (Stream, Tree.Stream_First (Stream, Skip_SOI => True))),
         Deleted => Valid_Node_Access_Lists.No_Element,
         Error   => Error_Data_Lists.No_Element)
      do
         if Result.Ref.Ref.Node.Label = Source_Terminal then
            Result.Deleted := Result.Ref.Ref.Node.Following_Deleted.First;
         end if;
         First_Error (Tree, Result);
      end return;
   end First_Error;

   function First_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Cur : constant Cursor := Next (Parse_Stream.Stack_Top);
   begin
      return (Stream, (Cur => Cur), Element (Cur).Node);
   end First_Input;

   function First_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := First_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Non_Grammar.Length > 0;
         Next_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end First_Non_Grammar;

   function First_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      Parents : Node_Stacks.Stack;
   begin
      return First_Non_Grammar (Tree, Node, Parents);
   end First_Non_Grammar;

   function First_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is begin
      return (Cur => Parse_Stream_Lists.Next (Tree.Shared_Stream.Cur));
   end First_Parse_Stream;

   procedure First_Recover_Conflict (Tree : in Syntax_Trees.Tree; Ref : in out Stream_Node_Ref)
   is begin
      loop
         exit when Ref = Invalid_Stream_Node_Ref;
         exit when Ref.Node.Label = Nonterm and then Ref.Node.Recover_Conflict;

         Next_Nonterm (Tree, Ref);
      end loop;
   end First_Recover_Conflict;

   function First_Recover_Conflict (Tree : in Syntax_Trees.Tree) return Stream_Node_Ref
   is begin
      return Result : Stream_Node_Ref :=
        (Stream  => Tree.Shared_Stream,
         Element => (Cur => Tree.Streams (Tree.Shared_Stream.Cur).Elements.First),
         Node    => Tree.SOI)
      do
         First_Recover_Conflict (Tree, Result);
      end return;
   end First_Recover_Conflict;

   function First_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean;
      Following            : in Boolean)
     return Node_Access
   is
      --  If not Following, we use a Parents stack to limit Next_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access :=
        (if Following
         then First_Terminal (Tree, Node)
         else First_Terminal (Tree, Node, Parents));
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         if Following then
            Next_Terminal (Tree, Result);
         else
            Next_Terminal (Tree, Result, Parents);
         end if;
      end loop;
      return Result;
   end First_Source_Terminal;

   procedure First_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean)
   is begin
      Ref.Ref.Node := First_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      loop
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Ref.Ref.Node.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal => Ref.Ref.Node.Non_Grammar.Length > 0,
                  when Virtual_Identifier => Ref.Ref.Node.Non_Grammar.Length > 0)
            else Ref.Ref.Node.Label = Source_Terminal);

         Next_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      end loop;
   end First_Source_Terminal;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access
   is begin
      return
        (if Item.Virtual
         then Item.First_Terminal
         else First_Terminal (Tree, Item.Element_Node));
   end First_Terminal;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node;
      when Nonterm =>
         for C of Node.Children loop
            --  This is called from Error_Message; tolerate deleted children
            if C /= Invalid_Node_Access then
               declare
                  Term : constant Node_Access := First_Terminal (Tree, C);
               begin
                  if Term /= Invalid_Node_Access then
                     return Term;
                  end if;
               end;
            end if;
         end loop;
         return Invalid_Node_Access;
      end case;
   end First_Terminal;

   function First_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
   begin
      case Node.Label is
      when Terminal_Label =>
         return Node;

      when Nonterm =>
         for C of Node.Children loop
            --  We tolerate null C here because this function is called while
            --  printing a tree for debug.
            if C /= Invalid_Node_Access then
               Parents.Push (Node);
               declare
                  First_Term : constant Node_Access := First_Terminal (Tree, C, Parents);
               begin
                  if First_Term /= Invalid_Node_Access then
                     return First_Term;
                  else
                     Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to First_Terminal.
                  end if;
               end;
            end if;
         end loop;

         --  All children are empty
         return Invalid_Node_Access;
      end case;
   end First_Terminal;

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is
      use Stream_Element_Lists;
   begin
      Ref.Node := First_Terminal (Tree, Element (Ref.Element.Cur).Node);
      loop
         exit when Ref.Node /= Invalid_Node_Access;
         Stream_Next (Tree, Ref, Rooted => False);
         exit when not Has_Element (Ref.Element.Cur);
      end loop;
   end First_Terminal;

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   is
      Result : Stream_Node_Ref := Ref;
   begin
      First_Terminal (Tree, Result);
      return Result;
   end First_Terminal;

   function First_Terminal_In_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   is begin
      return
        (Ref.Stream,
         Ref.Element,
         First_Terminal (Tree, Ref.Node));
   end First_Terminal_In_Node;

   procedure First_Terminal
     (Tree      : in     Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      Following : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      if Ref.Ref.Node = Invalid_Node_Access then
         --  Ref is an empty nonterm.
         null;
      else
         Ref.Ref.Node := First_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      end if;

      if Following then
         loop
            exit when Ref.Ref.Node /= Invalid_Node_Access;
            Next_Terminal (Tree, Ref, Following);
            exit when not Has_Element (Ref.Ref.Element.Cur);
         end loop;
      elsif Ref.Ref.Node = Invalid_Node_Access then
         Ref.Parents.Clear;
      end if;
   end First_Terminal;

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Rooted_Ref)
     return Stream_Node_Parents
   is
      use Stream_Element_Lists;
   begin
      --  We'd like the result type to be Terminal_Stream_Node_Parents, but
      --  the dynamic predicate is checked on assigning the initial value of
      --  Result, and it fails if Ref is not a terminal.
      return Result : Stream_Node_Parents := (Ref, Parents => <>) do
         Result.Ref.Node := First_Terminal (Tree, Element (Result.Ref.Element.Cur).Node, Result.Parents);
         loop
            exit when Result.Ref.Node /= Invalid_Node_Access;
            Next_Terminal (Tree, Result, Following => True);
            exit when not Has_Element (Result.Ref.Element.Cur);
         end loop;
      end return;
   end First_Terminal;

   --  FIXME: alphabetize these.
   procedure First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      if Node = Invalid_Node_Access then
         return;
      end if;

      Node := First_Terminal (Tree, Node, Parents);
      loop
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;

         Tree.Next_Terminal (Node, Parents);
      end loop;
   end First_Sequential_Terminal;

   function First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := Node;
   begin
      First_Sequential_Terminal (Tree, Result, Parents);
      return Result;
   end First_Sequential_Terminal;

   function First_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Node_Access)
     return Node_Access
   is
      Parents : Node_Stacks.Stack;
   begin
      return Tree.First_Sequential_Terminal (Node, Parents);
   end First_Sequential_Terminal;

   procedure First_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Ref)
   is begin
      Tree.First_Terminal (Ref);
      loop
         exit when Ref = Invalid_Stream_Node_Ref;

         exit when Ref.Node /= Invalid_Node_Access and then
           Tree.Get_Sequential_Index (Ref.Node) /= Invalid_Sequential_Index;

         Tree.Next_Terminal (Ref);
      end loop;
   end First_Sequential_Terminal;

   function First_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Syntax_Trees.Rooted_Ref)
     return Terminal_Ref
   is
      Ref_Parents : Stream_Node_Parents := (Ref, Parents => <>);
   begin
      Tree.First_Sequential_Terminal (Ref_Parents, Following => True);
      return Ref_Parents.Ref;
   end First_Sequential_Terminal;

   procedure First_Sequential_Terminal
     (Tree      : in     Syntax_Trees.Tree;
      Ref       : in out Syntax_Trees.Stream_Node_Parents;
      Following : in     Boolean)
   is begin
      Tree.First_Terminal (Ref, Following);
      loop
         exit when Ref.Ref = Invalid_Stream_Node_Ref;

         exit when Ref.Ref.Node /= Invalid_Node_Access and then
           Tree.Get_Sequential_Index (Ref.Ref.Node) /= Invalid_Sequential_Index;

         exit when not Following and Ref.Ref.Node = Invalid_Node_Access;

         Tree.Next_Terminal (Ref, Following);
      end loop;
   end First_Sequential_Terminal;

   procedure Free_Augmented (Tree : in Syntax_Trees.Tree)
   is begin
      for Node of Tree.Nodes loop
         Free (Node.Augmented);
      end loop;
   end Free_Augmented;

   function Following_Deleted
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
     return Valid_Node_Access_List_Var_Ref
   is begin
      return
        (List  => Node.Following_Deleted'Access,
         Dummy => 1);
   end Following_Deleted;

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Streams.Length = 2 and then Tree.Stream_Length ((Cur => Tree.Streams.Last)) in 2 .. 3;
   end Fully_Parsed;

   procedure Get_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      ID     : in     Token_ID;
      Result : in out Valid_Node_Access_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      if Node.ID = ID then
         Last := Last + 1;
         Result (Last) := Node;
      end if;
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         null;
      when Nonterm =>
         for I of Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            Get_IDs (Tree, I, ID, Result, Last);
         end loop;
      end case;
   end Get_IDs;

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Valid_Node_Access_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Valid_Node_Access_Array (1 .. Count_IDs (Tree, Node, ID)) := (others => Dummy_Node) do
         Get_IDs (Tree, Node, ID, Result, Last);
      end return;
   end Get_IDs;

   function Get_Node
     (Element : in Stream_Index)
     return Valid_Node_Access
   is begin
      return Stream_Element_Lists.Element (Element.Cur).Node;
   end Get_Node;

   function Get_Node
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Valid_Node_Access
   is begin
      return Stream_Element_Lists.Element (Element.Cur).Node;
   end Get_Node;

   function Get_Node_Index (Node : in Node_Access) return Node_Index
   is begin
      return (if Node = Invalid_Node_Access then Invalid_Node_Index else Node.Node_Index);
   end Get_Node_Index;

   function Get_Node_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Node_Index
   is begin
      return (if Node = Invalid_Node_Access then Invalid_Node_Index else Node.Node_Index);
   end Get_Node_Index;

   function Get_Node_Index (Element : in Stream_Index) return Node_Index
   is begin
      return
        (if Stream_Element_Lists.Has_Element (Element.Cur)
         then Stream_Element_Lists.Element (Element.Cur).Node.Node_Index
         else Invalid_Node_Index);
   end Get_Node_Index;

   function Get_Node_Index
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Node_Index
   is begin
      return Get_Node_Index (Element);
   end Get_Node_Index;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Recover_Token
   is begin
      return
        (Virtual      => False,
         Element_Node =>
           (if Ref.Element = Invalid_Stream_Index
            then Invalid_Node_Access
            else Stream_Element_Lists.Element (Ref.Element.Cur).Node),
         Node         => Ref.Node);
   end Get_Recover_Token;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token
   is begin
      --  Used in McKenzie_Recover.Undo_Reduce, so same value as in Tree.Push (Node)
      return
        (Virtual      => False,
         Element_Node => Node,
         Node         => Node);
   end Get_Recover_Token;

   function Get_Sequential_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Base_Sequential_Index
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Node = Invalid_Node_Access
         then Invalid_Sequential_Index
         else Node.Sequential_Index);
   end Get_Sequential_Index;

   procedure Get_Terminals
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Result : in out Valid_Node_Access_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         Last := Last + 1;
         Result (Last) := Node;

      when Nonterm =>
         for C of Node.Children loop
            --  This is called to build an edited source image while editing the tree
            if C /= null then
               Get_Terminals (Tree, C, Result, Last);
            end if;
         end loop;
      end case;
   end Get_Terminals;

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Valid_Node_Access_Array (1 .. SAL.Base_Peek_Type (Count_Terminals (Tree, Node))) :=
        (others => Dummy_Node)
      do
         Get_Terminals (Tree, Node, Result, Last);
      end return;
   end Get_Terminals;

   procedure Get_Terminal_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Result : in out Token_ID_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         Last := Last + 1;
         Result (Integer (Last)) := Node.ID;

      when Nonterm =>
         for I of Node.Children loop
            --  Encountering Deleted_Child here is an error in the user algorithm.
            Get_Terminal_IDs (Tree, I, Result, Last);
         end loop;
      end case;
   end Get_Terminal_IDs;

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Token_ID_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminal_IDs (Tree, Node, Result, Last);
      end return;
   end Get_Terminal_IDs;

   procedure Get_Tree
     (Tree      : in out Syntax_Trees.Tree;
      File_Name : in     String)
   is
      use Ada.Streams.Stream_IO;

      Delims : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ()," & ASCII.LF);

      File   : File_Type;
      Stream : Stream_Access;

      Node_Index_Map : Node_Index_Array_Node_Access.Vector;

      package Node_Index_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists
        (Node_Index);

      type Delayed_Following_Deleted_Type is record
         Prev_Terminal : Node_Access;
         List : Node_Index_Lists.List;
      end record;

      package Delayed_Following_Deleted_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists
        (Delayed_Following_Deleted_Type);

      Delayed_Following_Deleted : Delayed_Following_Deleted_Lists.List;

      function Next_Value return String
      is begin
         return WisiToken.Next_Value (Stream, Delims);
      end Next_Value;

      function Input_Buffer_Region return Buffer_Region
      is begin
         return Result : Buffer_Region do

            Result.First := Base_Buffer_Pos'Value (Next_Value);
            Result.Last  := Base_Buffer_Pos'Value (Next_Value);
         end return;
      end Input_Buffer_Region;

      function Input_Line_Region return WisiToken.Line_Region
      is begin
         return Result : WisiToken.Line_Region do

            Result.First := Line_Number_Type'Value (Next_Value);
            Result.Last  := Line_Number_Type'Value (Next_Value);
         end return;
      end Input_Line_Region;

      function Input_Token return WisiToken.Lexer.Token
      is begin
         return Result : WisiToken.Lexer.Token do

            Result.ID          := Token_ID'Value (Next_Value);
            Result.Byte_Region := Input_Buffer_Region;
            Result.Char_Region := Input_Buffer_Region;
            Result.Line_Region := Input_Line_Region;
         end return;
      end Input_Token;

      function Input_Non_Grammar return WisiToken.Lexer.Token_Arrays.Vector
      is
         Length : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type'Value (Next_Value);
      begin
         return Result : WisiToken.Lexer.Token_Arrays.Vector do
            if Length > 0 then
               Result.Set_First_Last (1, Length);
               for I in 1 .. Length loop
                  Result (I) := Input_Token;
               end loop;
            end if;
         end return;
      end Input_Non_Grammar;

      function Get_Node_Access return Node_Access
      is
         Index : constant Node_Index := Node_Index'Value (Next_Value);
      begin
         if Index = Invalid_Node_Index then
            return Invalid_Node_Access;
         end if;

         if Index in Node_Index_Map.First_Index .. Node_Index_Map.Last_Index then
            return Node_Index_Map (Index);
         else
            raise SAL.Programmer_Error;
         end if;
      end Get_Node_Access;

      function Input_Error_List return Error_List_Access
      is
         Length : constant Ada.Containers.Count_Type := Ada.Containers.Count_Type'Value (Next_Value);
      begin
         if Length = 0 then
            return null;
         else
            return Result : constant Error_List_Access := new Error_Data_Lists.List do
               for I in 1 .. Length loop
                  Result.Append (Error_Data'Class'Input (Stream));
               end loop;
            end return;
         end if;
      end Input_Error_List;

      procedure Input_Following_Deleted (Node : in Valid_Node_Access)
      is
         Length : constant Ada.Containers.Count_Type := Ada.Containers.Count_Type'Value (Next_Value);
      begin
         if Length = 0 then
            return;
         else
            --  Following nodes have not been created yet. Need to read node_index
            --  list, cache it somewhere, finish this after they are read.
            declare
               Item : Delayed_Following_Deleted_Type;
            begin
               Item.Prev_Terminal := Node;
               for I in 1 .. Length loop
                  Item.List.Append (Node_Index'Value (Next_Value));
               end loop;
            end;
         end if;
      end Input_Following_Deleted;

      function Input_Children (Child_Count : in SAL.Base_Peek_Type) return Node_Access_Array
      is begin
         --  Child nodes are always created before parent nodes, so
         --  Get_Node_Access will succeed.
         return Result : Node_Access_Array (1 .. Child_Count) do
            for I in Result'Range loop
               Result (I) := Get_Node_Access;
            end loop;
         end return;
      end Input_Children;

      procedure Input_Node
      --  Read one node from Stream
      is begin
         declare
            Label       : constant Node_Label              := Node_Label'Value (Next_Value);
            Child_Count : constant SAL.Base_Peek_Type      := SAL.Base_Peek_Type'Value (Next_Value);
            ID          : constant Token_ID                := Token_ID'Value (Next_Value);
            Node_Index  : constant Syntax_Trees.Node_Index := Syntax_Trees.Node_Index'Value (Next_Value);
            Error_List  : constant Error_List_Access       := Input_Error_List;
         begin
            case Label is
            when Terminal_Label =>
               declare
                  Non_Grammar      : constant Lexer.Token_Arrays.Vector := Input_Non_Grammar;
                  Sequential_Index : constant Base_Sequential_Index     := Base_Sequential_Index'Value (Next_Value);
               begin
                  case Terminal_Label'(Label) is
                  when Source_Terminal =>
                     pragma Assert
                       (Child_Count = 0 and
                          (Node_Index > 0 or else ID = Tree.Lexer.Descriptor.SOI_ID));
                     declare
                        Byte_Region    : constant Buffer_Region         := Input_Buffer_Region;
                        Char_Region    : constant Buffer_Region         := Input_Buffer_Region;
                        New_Line_Count : constant Base_Line_Number_Type := Base_Line_Number_Type'Value
                          (Next_Value);

                        New_Node : constant Valid_Node_Access := new Node'
                          (Label             => Source_Terminal,
                           Copied_Node       => Invalid_Node_Access,
                           Parent            => Invalid_Node_Access,
                           Augmented         => null,
                           Child_Count       => 0,
                           ID                => ID,
                           Node_Index        => Node_Index,
                           Error_List        => Error_List,
                           Non_Grammar       => Non_Grammar,
                           Sequential_Index  => Sequential_Index,
                           Byte_Region       => Byte_Region,
                           Char_Region       => Char_Region,
                           New_Line_Count    => New_Line_Count,
                           Following_Deleted => Valid_Node_Access_Lists.Empty_List);
                     begin
                        Input_Following_Deleted (New_Node);

                        Tree.Nodes.Append (New_Node);
                        Node_Index_Map.Extend (Node_Index);
                        Node_Index_Map (Node_Index) := New_Node;
                        if New_Node.ID = Tree.Lexer.Descriptor.EOI_ID then
                           Tree.EOI := New_Node;
                        elsif New_Node.ID = Tree.Lexer.Descriptor.SOI_ID then
                           Tree.SOI := New_Node;
                        end if;
                     end;

                  when Virtual_Terminal =>
                     pragma Assert (Child_Count = 0 and Node_Index < 0);
                     declare
                        Insert_Location : constant WisiToken.Insert_Location := WisiToken.Insert_Location'Value
                          (Next_Value);
                        New_Node        : constant Valid_Node_Access         := new Node'
                          (Label            => Virtual_Terminal,
                           Copied_Node      => Invalid_Node_Access,
                           Child_Count      => 0,
                           ID               => ID,
                           Node_Index       => Node_Index,
                           Parent           => Invalid_Node_Access,
                           Augmented        => null,
                           Error_List       => Error_List,
                           Non_Grammar      => Non_Grammar,
                           Sequential_Index => Sequential_Index,
                           Insert_Location  => Insert_Location);
                     begin
                        Tree.Nodes.Append (New_Node);
                        Node_Index_Map.Extend (Node_Index);
                        Node_Index_Map (Node_Index) := New_Node;
                     end;

                  when Virtual_Identifier =>
                     raise SAL.Programmer_Error;

                  end case;
               end;

            when Nonterm =>
               pragma Assert (Node_Index < 0);
               declare
                  Virtual          : constant Boolean           := Boolean'Value (Next_Value);
                  Recover_Conflict : constant Boolean           := Boolean'Value (Next_Value);
                  RHS_Index        : constant Natural           := Natural'Value (Next_Value);
                  Name_Offset      : constant Base_Buffer_Pos   := Base_Buffer_Pos'Value (Next_Value);
                  Name_Length      : constant Base_Buffer_Pos   := Base_Buffer_Pos'Value (Next_Value);
                  Children         : constant Node_Access_Array := Input_Children (Child_Count);
                  New_Node         : constant Valid_Node_Access := new Node'
                    (Label            => Nonterm,
                     Copied_Node      => Invalid_Node_Access,
                     Child_Count      => Child_Count,
                     ID               => ID,
                     Node_Index       => Node_Index,
                     Parent           => Invalid_Node_Access,
                     Augmented        => null,
                     Error_List       => Error_List,
                     Virtual          => Virtual,
                     Recover_Conflict => Recover_Conflict,
                     RHS_Index        => RHS_Index,
                     Name_Offset      => Name_Offset,
                     Name_Length      => Name_Length,
                     Children         => Children);
               begin
                  Tree.Nodes.Append (New_Node);
                  Node_Index_Map.Extend (Node_Index);
                  Node_Index_Map (Node_Index) := New_Node;

                  for Child of Children loop
                     Child.Parent := New_Node;
                  end loop;
               end;
            end case;
         end;
      end Input_Node;

   begin
      Open (File, In_File, File_Name);
      Stream := Ada.Streams.Stream_IO.Stream (File);

      declare
         Node_Count : constant Positive_Node_Index := Positive_Node_Index'Value (Next_Value);
      begin
         for I in 1 .. Node_Count loop
            Input_Node;
         end loop;
      end;

      for Item of Delayed_Following_Deleted loop
         for Index of Item.List loop
            Item.Prev_Terminal.Following_Deleted.Append (Node_Index_Map (Index));
         end loop;
      end loop;

      declare
         Streams_Length : constant Ada.Containers.Count_Type := Ada.Containers.Count_Type'Value (Next_Value);
      begin
         for I in 1 .. Streams_Length loop
            raise SAL.Not_Implemented with "get_tree: streams";
         end loop;
      end;

      Tree.Set_Root (Get_Node_Access);

      Close (File);

      if Tree.Streams.Length > 0 then
         for Stream_Cur in Tree.Streams.Iterate loop
            for Err_Cur in Tree.Stream_Error_Iterate ((Cur => Stream_Cur)) loop
               Error_Data_Lists.Variable_Ref (Err_Cur.SER.Error).Set_Node_Access (Node_Index_Map);
            end loop;
         end loop;
      else
         Tree.Set_Parents;
         for Err in Tree.Error_Iterate loop
            Error_Data_Lists.Variable_Ref (Err.Error).Set_Node_Access (Node_Index_Map);
         end loop;
      end if;
   end Get_Tree;

   function In_Tree
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access)
     return Boolean
   is begin
      return (for some N of Tree.Nodes => N = Node);
   end In_Tree;

   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Child : in Valid_Node_Access)
     return Boolean
   is begin
      for C of Node.Children loop
         if C = Child then
            return True;
         end if;
      end loop;
      return False;
   end Has_Child;

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Children'Length > 0;
   end Has_Children;

   function Has_Error (Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Error_List /= null;
   end Has_Error;

   function Has_Error (Error : in Error_Ref) return Boolean
   is begin
      if Error.Node = Invalid_Node_Access then
         return False;

      elsif Valid_Node_Access_Lists.Has_Element (Error.Deleted) then
         return True;

      else
         return Error.Node.Error_List /= null;
      end if;
   end Has_Error;

   function Has_Error (Error : in Stream_Error_Ref) return Boolean
   is begin
      if Error.Ref.Ref.Node = Invalid_Node_Access then
         return False;

      elsif Valid_Node_Access_Lists.Has_Element (Error.Deleted) then
         return True;

      else
         return Error.Ref.Ref.Node.Error_List /= null;
      end if;
   end Has_Error;

   function Has_Error (Position : in Stream_Error_Cursor) return Boolean
   is begin
      return Has_Error (Position.SER);
   end Has_Error;

   function Has_Error (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Error_List /= null;
   end Has_Error;

   function Has_Error_Class
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Error_Class : in Error_Data'Class)
     return Error_Ref
   is
      use all type Error_Data_Lists.Cursor;
      use all type Ada.Tags.Tag;
   begin
      if Node.Error_List /= null then
         declare
            Result : Error_Ref :=
              (Node    => Node,
               Deleted => Valid_Node_Access_Lists.No_Element,
               Error   => Node.Error_List.First);
         begin
            loop
               if Node.Error_List.Constant_Reference (Result.Error).Element.all'Tag = Error_Class'Tag then
                  return Result;
               end if;
               Error_Data_Lists.Next (Result.Error);
               exit when Result.Error = Error_Data_Lists.No_Element;
            end loop;
         end;
      end if;
      return Invalid_Error_Ref;
   end Has_Error_Class;

   function Has_Errors (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      if Tree.Parents_Set then
         return Tree.First_Error /= Invalid_Error_Ref;
      else
         for Cur in Tree.Streams.Iterate loop
            if Tree.First_Error ((Cur => Cur)) /= Invalid_Stream_Error_Ref then
               return True;
            end if;
         end loop;
         return False;
      end if;
   end Has_Errors;

   function Has_Following_Deleted
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean
   is begin
      return Node.Following_Deleted.Length > 0;
   end Has_Following_Deleted;

   function Has_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Boolean
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      return Parse_Stream.Stack_Top /= Parse_Stream.Elements.Last;
   end Has_Input;

   function Has_Non_Grammar
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Terminal.Non_Grammar.Length > 0;
   end Has_Non_Grammar;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Child.Parent /= Invalid_Node_Access;
   end Has_Parent;

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      return Node.ID;
   end ID;

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   is
      pragma Unreferenced (Tree, Stream);
   begin
      return Stream_Element_Lists.Element (Element.Cur).Node.ID;
   end ID;

   function ID
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return WisiToken.Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      if Ref.Node /= Invalid_Node_Access then
         return Ref.Node.ID;
      elsif Ref.Element /= Invalid_Stream_Index then
         return Stream_Element_Lists.Element (Ref.Element.Cur).Node.ID;
      else
         return Invalid_Token_ID;
      end if;
   end ID;

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Base_Identifier_Index
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Identifier;
   end Identifier;

   function Image
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return String
   is begin
      if Item.Virtual then
         return "(" & Image (Item.ID, Tree.Lexer.Descriptor.all) & ")";
      else
         return "(" & Image (Tree, Item.Element_Node, Node_Numbers => True) &
           (if Item.Element_Node = Item.Node
            then ""
            else ", " & Image (Tree, Item.Node, Terminal_Node_Numbers => True)) & ")";
      end if;
   end Image;

   function Image
     (Tree          : in Syntax_Trees.Tree;
      Stream        : in Parse_Stream;
      Stack         : in Boolean := True;
      Input         : in Boolean := True;
      Shared        : in Boolean := False;
      Children      : in Boolean := False;
      Node_Numbers  : in Boolean := False;
      Non_Grammar   : in Boolean := False;
      Augmented     : in Boolean := False;
      Line_Numbers  : in Boolean := False;
      State_Numbers : in Boolean := True)
     return String
   is
      --  stack ^ stack_top input / shared
      use Ada.Strings.Unbounded;
      use Stream_Element_Lists;
      Result     : Unbounded_String := +"(" & Trimmed_Image (Stream.Label) & ", ";
      Element    : Cursor           :=
        (if Stack or Stream.Stack_Top = No_Element
         then Stream.Elements.First
         else Next (Stream.Stack_Top));
      Need_Comma           : Boolean      := False;
      Current_Stream_Label : Stream_Label := Stream.Label;
   begin
      loop
         if not Has_Element (Element) then
            if Shared and Current_Stream_Label /= Shared_Stream_Label then
               if not Has_Element (Stream.Shared_Link) then
                  exit;
               else
                  Current_Stream_Label := Shared_Stream_Label;
                  Element              := Stream.Shared_Link;

                  Result := @ & "/";
               end if;
            else
               exit;
            end if;
         end if;

         if Need_Comma then
            Result := @ & (if Children then "," & ASCII.LF else ", ");
         else
            Need_Comma := True;
         end if;

         Result := @ &
           (if Stream.Stack_Top = Element then "^(" else "(") &
           (if State_Numbers
            then Trimmed_Image (Stream_Element_Lists.Element (Element).State)
            else "--") & ", " &
           (if Children
            then Tree.Subtree_Image
              (Stream_Element_Lists.Element (Element).Node, Node_Numbers, Non_Grammar, Augmented, Line_Numbers)
            else Tree.Image
              (Stream_Element_Lists.Element (Element).Node,
               Children              => False,
               RHS_Index             => False,
               Node_Numbers          => Node_Numbers,
               Terminal_Node_Numbers => True,
               Non_Grammar           => Non_Grammar,
               Augmented             => Augmented,
               Line_Numbers          => Line_Numbers))
           & ")";

         if not Input then
            exit when Element = Stream.Stack_Top;
         end if;

         Element := Next (Element);
      end loop;
      Result := @ & ")";
      return -Result;
   end Image;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Children     : in Boolean     := False;
      Non_Grammar  : in Boolean     := False;
      Augmented    : in Boolean     := False;
      Line_Numbers : in Boolean     := False;
      Root         : in Node_Access := Invalid_Node_Access)
     return String
   is begin
      if Root /= Invalid_Node_Access then
         --  Assuming children = true in this case.
         return Subtree_Image (Tree, Root, Non_Grammar, Augmented, Line_Numbers);

      elsif Tree.Streams.Length = 0 then
         if Tree.Root = Invalid_Node_Access then
            return "invalid_tree: no streams, Tree.Root not set";
         else
            --  Assuming children = true in this case.
            return Subtree_Image
              (Tree, Tree.Root,
               Non_Grammar  => Non_Grammar,
               Augmented    => Augmented,
               Line_Numbers => Line_Numbers);
         end if;
      else
         declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String;
            Need_New_Line : Boolean := False;
         begin
            for Stream of Tree.Streams loop
               if Need_New_Line then
                  Result := @ & ASCII.LF;
               else
                  Need_New_Line := True;
               end if;
               Result := @ & Image
                 (Tree, Stream, Children,
                  Non_Grammar  => Non_Grammar,
                  Augmented    => Augmented,
                  Line_Numbers => Line_Numbers);
            end loop;
            return -Result;
         end;
      end if;
   end Image;

   function Image
     (Tree          : in Syntax_Trees.Tree;
      Stream        : in Stream_ID;
      Stack         : in Boolean := True;
      Input         : in Boolean := True;
      Shared        : in Boolean := False;
      Children      : in Boolean := False;
      Node_Numbers  : in Boolean := True;
      Non_Grammar   : in Boolean := False;
      Augmented     : in Boolean := False;
      Line_Numbers  : in Boolean := False;
      State_Numbers : in Boolean := True)
     return String
   is begin
      return Image
        (Tree, Tree.Streams (Stream.Cur), Stack, Input, Shared,
         Children, Node_Numbers, Non_Grammar, Augmented, Line_Numbers, State_Numbers);
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      State                 : in Boolean := False;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Line_Numbers          : in Boolean := False;
      Non_Grammar           : in Boolean := False;
      Augmented             : in Boolean := False;
      Expecting             : in Boolean := False)
     return String
   is begin
      if Element.Cur = Stream_Element_Lists.No_Element then
         return "<null>";
      else
         declare
            El : Stream_Element renames Stream_Element_Lists.Element (Element.Cur);
         begin
            return
              (if State
               then "(" & Trimmed_Image (El.State) & ", "
               else "") &
              Image
                (Tree, El.Node, Children,
                 RHS_Index, Node_Numbers, Terminal_Node_Numbers,
                 Line_Numbers => Line_Numbers,
                 Non_Grammar  => Non_Grammar,
                 Augmented    => Augmented,
                 Expecting    => Expecting) &
              (if State then ")" else "");
         end;
      end if;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Node                  : in Node_Access;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Line_Numbers          : in Boolean := False;
      Non_Grammar           : in Boolean := False;
      Augmented             : in Boolean := False;
      Expecting             : in Boolean := False;
      Safe_Only             : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
   begin
      if Node = null then
         return "<null>";
      else
         declare
            Result : Unbounded_String;
            Node_Byte_Region : constant Buffer_Region :=
              (if Safe_Only
               then Null_Buffer_Region
               else Tree.Byte_Region (Node, Trailing_Non_Grammar => False));
         begin
            if Node.Label in Terminal_Label and then Node.Sequential_Index /= Invalid_Sequential_Index then
               Append (Result, Trimmed_Image (Node.Sequential_Index) & ";");
            end if;

            if Node_Numbers then
               Append (Result, Trimmed_Image (Node.Node_Index) & ":");

            elsif Terminal_Node_Numbers then
               Append
                 (Result,
                  (case Node.Label is
                   when Source_Terminal    => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Terminal   => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Identifier => Trimmed_Image (Node.Identifier) & ":",
                   when Nonterm            => ""));
            end if;

            Append (Result, "(" & Image (Node.ID, Tree.Lexer.Descriptor.all));
            Append (Result, (if RHS_Index and Node.Label = Nonterm then "_" & Trimmed_Image (Node.RHS_Index) else ""));

            if Node_Byte_Region /= Null_Buffer_Region then
               Append (Result, ", " & Image (Node_Byte_Region));
            end if;

            if (Line_Numbers and Tree.Editable and not Safe_Only) and then
              Tree.Line_Region (Node, Trailing_Non_Grammar => True) /=
              Null_Line_Region
            then
               Append (Result, ", " & Image (Tree.Line_Region (Node, Trailing_Non_Grammar => True)));
            end if;

            if not Safe_Only and Children and Node.Label = Nonterm then
               Result := @ & " <= " & Image
                 (Tree, Node.Children, RHS_Index, Node_Numbers, Terminal_Node_Numbers, Non_Grammar, Augmented);
            end if;

            if (Non_Grammar and Node.Label in Terminal_Label) and then Node.Non_Grammar.Length > 0 then
               Result := @ & "(";
               for Token of Node.Non_Grammar loop
                  Result := @ & "(";
                  Result := @ & Image (Token.ID, Tree.Lexer.Descriptor.all) & ", ";
                  Result := @ & Image (Token.Byte_Region) & ", ";
                  Result := @ & Image (Token.Line_Region);
                  Result := @ & ")";
               end loop;
               Result := @ & ")";
            end if;

            if Node.Augmented /= null and Augmented then
               Result := @ & Image_Augmented (Node.Augmented.all);
            end if;

            if (Node_Numbers and Node.Label = Nonterm) and then Node.Recover_Conflict then
               Append (Result, " recover_conflict");
            end if;

            if Node.Error_List /= null then
               if Expecting then
                  for Err of Node.Error_List.all loop
                     Append (Result, ASCII.LF & "   ERROR: " & Err.Image (Tree, Node));
                  end loop;
               else
                  for Err of Node.Error_List.all loop
                     Append (Result, ", " & Err.Class_Image & " ERROR");
                  end loop;
               end if;
            end if;

            if Node.Label = Source_Terminal and then Node.Following_Deleted.Length > 0 then
               Append
                 (Result,
                  (if Children then ASCII.LF & "  " else "") & " deleted: " &
                    Tree.Image (Node.Following_Deleted));
            end if;

            Append (Result, ")");

            return -Result;
         end;
      end if;
   exception
   when E : others =>
      --  Tolerate corrupt tree, for debugging.
      if Debug_Mode then
         Tree.Lexer.Trace.Put_Line
           ("corrupt tree; " & Ada.Exceptions.Exception_Name (E) & ":" &
              Ada.Exceptions.Exception_Message (E));
         Tree.Lexer.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end if;
      return Node.Node_Index'Image & ": corrupt tree; " & Ada.Exceptions.Exception_Name (E) & ":" &
        Ada.Exceptions.Exception_Message (E);
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Nodes                 : in Node_Access_Array;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Line_Numbers          : in Boolean := False;
      Non_Grammar           : in Boolean := False;
      Augmented             : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           (if Nodes (I) = null then " - "
            else Tree.Image
              (Nodes (I),
               RHS_Index             => RHS_Index,
               Node_Numbers          => Node_Numbers,
               Terminal_Node_Numbers => Terminal_Node_Numbers,
               Line_Numbers          => Line_Numbers,
               Non_Grammar           => Non_Grammar,
               Augmented             => Augmented));
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Ref                   : in Stream_Node_Ref;
      First_Terminal        : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Line_Numbers          : in Boolean := False;
      Non_Grammar           : in Boolean := False;
      Augmented             : in Boolean := False;
      Expecting             : in Boolean := False)
     return String
   is
      use Stream_Element_Lists;
   begin
      if Ref.Element.Cur /= No_Element then
         declare
            Element_Node : constant Valid_Node_Access := Element (Ref.Element.Cur).Node;
         begin
            return "(" & Trimmed_Image (Tree.Streams (Ref.Stream.Cur).Label) & ", " &
              Image
                (Tree, Ref.Element,
                 Node_Numbers           => Node_Numbers,
                 Terminal_Node_Numbers  => Terminal_Node_Numbers,
                 Line_Numbers           => Line_Numbers,
                 Non_Grammar            => Non_Grammar,
                 Augmented              => Augmented,
                 Expecting              => Expecting) &
              (if Ref.Node = Invalid_Node_Access or Element_Node.Label in Terminal_Label
               then ""
               elsif Element_Node.Label = Nonterm and Element_Node = Ref.Node and First_Terminal
               then ", " & Image
                 (Tree,
                  Tree.First_Terminal (Ref.Node),
                  Node_Numbers          => Node_Numbers,
                  Terminal_Node_Numbers => Terminal_Node_Numbers,
                  Line_Numbers          => Line_Numbers,
                  Non_Grammar           => Non_Grammar,
                  Augmented             => Augmented,
                  Expecting             => Expecting)

               else ", " & Image
                 (Tree,
                  Ref.Node,
                  Node_Numbers          => Node_Numbers,
                  Terminal_Node_Numbers => True,
                  Line_Numbers          => Line_Numbers,
                  Non_Grammar           => Non_Grammar,
                  Augmented             => Augmented)) & ")";
         end;
      elsif Ref.Node /= Invalid_Node_Access then
         return "(" & Image
           (Tree, Ref.Node,
            Terminal_Node_Numbers => True,
            Line_Numbers          => Line_Numbers,
            Non_Grammar           => Non_Grammar) & ")";
      else
         return "()";
      end if;
   end Image;

   function Input_Has_Matching_Error
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Data   : in Error_Data'Class)
     return Boolean
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Error_Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last
         then (Tree.Shared_Stream,
               (Cur => Parse_Stream.Shared_Link),
               Element (Parse_Stream.Shared_Link).Node)
         else (Stream,
               (Cur => Next (Parse_Stream.Stack_Top)),
               Element (Next (Parse_Stream.Stack_Top)).Node));
   begin
      Tree.First_Terminal (Error_Ref, Following => False);

      if Error_Ref.Ref.Node = Invalid_Node_Access then
         --  Empty nonterm
         return False;
      end if;

      if Error_Ref.Ref.Node.Error_List = null then
         return False;
      else
         return (for some Err of Error_Ref.Ref.Node.Error_List.all => Dispatch_Equal (Err, Data));
      end if;
   end Input_Has_Matching_Error;

   function Insert_After
     (User_Data           : in out User_Data_Type;
      Tree                : in     Syntax_Trees.Tree'Class;
      Insert_Token        : in     Valid_Node_Access;
      Insert_Before_Token : in     Valid_Node_Access;
      Comment_Present     : in     Boolean;
      Blank_Line_Present  : in     Boolean)
     return Insert_Location
   is
      pragma Unreferenced (User_Data, Tree, Insert_Token, Insert_Before_Token, Comment_Present, Blank_Line_Present);
   begin
      return Before_Next;
   end Insert_After;

   procedure Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Lexer.Token;
      Before   : in     Stream_Index;
      Errors   : in     Error_Data_Lists.List)
   is
      New_Node : constant Valid_Node_Access := Add_Source_Terminal_1
        (Tree, Terminal,
         In_Shared_Stream => Stream = Tree.Shared_Stream,
         Errors => Errors);
   begin
      Insert_Stream_Element (Tree, Stream, New_Node, Before => Before.Cur);
   end Insert_Source_Terminal;

   function Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Lexer.Token;
      Before   : in     Stream_Index;
      Errors   : in     Error_Data_Lists.List)
     return Single_Terminal_Ref
   is
      New_Node : constant Valid_Node_Access := Add_Source_Terminal_1
        (Tree, Terminal,
         In_Shared_Stream => Stream = Tree.Shared_Stream,
         Errors => Errors);
   begin
      return Insert_Stream_Element (Tree, Stream, New_Node, Before => Before.Cur);
   end Insert_Source_Terminal;

   procedure Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Node,
            State => Unknown_State),
         Before   =>
           (if Before /= No_Element
            then Before
            else
              (if Parse_Stream.Stack_Top = No_Element
               then No_Element
               else Next (Parse_Stream.Stack_Top))));
   end Insert_Stream_Element;

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Rooted_Ref
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Cursor := Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Node,
            State => Unknown_State),
         Before   =>
           (if Before /= No_Element
            then Before
            else
              (if Parse_Stream.Stack_Top = No_Element
               then No_Element
               else Next (Parse_Stream.Stack_Top))));
   begin
      return (Stream, (Cur => New_Element), Node);
   end Insert_Stream_Element;

   function Insert_Virtual_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID)
     return Single_Terminal_Ref
   is
      New_Node : constant Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         others      => <>);
   begin
      Tree.Nodes.Append (New_Node);
      return Insert_Stream_Element (Tree, Stream, New_Node);
   end Insert_Virtual_Terminal;

   function Is_Descendant_Of
     (Tree       : in Syntax_Trees.Tree;
      Root       : in Valid_Node_Access;
      Descendant : in Valid_Node_Access)
     return Boolean
   is
      Node : Node_Access := Descendant;
   begin
      loop
         exit when Node = Invalid_Node_Access;
         if Node = Root then
            return True;
         end if;

         Node := Tree.Parent (Node);
      end loop;
      return False;
   end Is_Descendant_Of;

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Streams.Length = 0 and Tree.Root = Invalid_Node_Access;
   end Is_Empty;

   function Is_Empty_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Boolean
   is begin
      return
        (case Item.Virtual is
         when True => Is_Nonterminal (Item.ID, Tree.Lexer.Descriptor.all) and Item.First_Terminal = Invalid_Node_Access,
         when False => Item.Node /= Invalid_Node_Access and then Tree.Is_Empty_Nonterm (Item.Node));
   end Is_Empty_Nonterm;

   function Is_Empty_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean
   is begin
      return Node.Label = Nonterm and then Tree.First_Terminal (Node) = Invalid_Node_Access;
   end Is_Empty_Nonterm;

   function Is_Empty_Or_Virtual_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean
   is begin
      return Node.Label = Nonterm and then
        (Tree.First_Terminal (Node) = Invalid_Node_Access or else -- no terminals
           Length (Tree.Byte_Region (Node, Trailing_Non_Grammar => False)) = 0 -- all terminals are virtual
        );
   end Is_Empty_Or_Virtual_Nonterm;

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Label = Nonterm;
   end Is_Nonterm;

   function Is_Optimized_List
     (Productions : in Production_Info_Trees.Vector;
      ID          : in Token_ID)
     return Boolean
   is begin
      if Productions.Is_Empty then
         return False;
      else
         return Productions (ID).Optimized_List;
      end if;
   end Is_Optimized_List;

   function Is_Source_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Label = Source_Terminal;
   end Is_Source_Terminal;

   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Tree.Label (Node) in Terminal_Label;
   end Is_Terminal;

   function Is_Valid (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Parse_Stream_Lists.Has_Element (Stream.Cur);
   end Is_Valid;

   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Label = Virtual_Terminal;
   end Is_Virtual_Terminal;

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Label = Virtual_Identifier;
   end Is_Virtual_Identifier;

   function Label (Node : in Valid_Node_Access) return Node_Label
   is begin
      return Node.Label;
   end Label;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Label;
   end Label;

   function Label (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Node_Label
   is
      pragma Unreferenced (Tree);
   begin
      return Stream_Element_Lists.Element (Element.Cur).Node.Label;
   end Label;

   function Last_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      --  We always use a Parents stack, to limit Prev_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access := Last_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Non_Grammar.Length > 0;
         Prev_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end Last_Non_Grammar;

   function Last_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is begin
      return (Cur => Parse_Stream_Lists.Last (Tree.Streams));
   end Last_Parse_Stream;

   function Last_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := Tree.Last_Terminal (Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Sequential_Index /= Invalid_Sequential_Index;
         Tree.Prev_Terminal (Result, Parents);
      end loop;
      return Result;
   end Last_Sequential_Terminal;

   function Last_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Node_Access)
     return Node_Access
   is
      Parents : Node_Stacks.Stack;
   begin
      return Tree.Last_Sequential_Terminal (Node, Parents);
   end Last_Sequential_Terminal;

   procedure Last_Sequential_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Syntax_Trees.Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Preceding    : in     Boolean)
   is begin
      Tree.Last_Terminal (Ref, Parse_Stream, Preceding);
      loop
         exit when Ref.Ref = Invalid_Stream_Node_Ref;

         exit when Ref.Ref.Node /= Invalid_Node_Access and then
           Tree.Get_Sequential_Index (Ref.Ref.Node) /= Invalid_Sequential_Index;

         Tree.Prev_Terminal (Ref, Parse_Stream, Preceding);
         exit when not Preceding and Ref.Ref.Node = Invalid_Node_Access;
      end loop;
   end Last_Sequential_Terminal;

   function Last_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      --  We always use a Parents stack, to limit Prev_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access := Last_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal => Result.Non_Grammar.Length > 0,
                  when Virtual_Identifier => Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         Prev_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end Last_Source_Terminal;

   procedure Last_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean)
   is begin
      Ref.Ref.Node := Last_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      loop
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Ref.Ref.Node.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal => Ref.Ref.Node.Non_Grammar.Length > 0,
                  when Virtual_Identifier => Ref.Ref.Node.Non_Grammar.Length > 0)
            else Ref.Ref.Node.Label = Source_Terminal);

         Prev_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      end loop;
   end Last_Source_Terminal;

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access
   is begin
      return
        (if Item.Virtual
         then Item.Last_Terminal
         else Last_Terminal (Tree, Item.Element_Node));
   end Last_Terminal;

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is begin
      case Node.Label is
      when Terminal_Label =>
         return Node;
      when Nonterm =>
         for C of reverse Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            declare
               Term : constant Node_Access := Last_Terminal (Tree, C);
            begin
               if Term /= Invalid_Node_Access then
                  return Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Access;
      end case;
   end Last_Terminal;

   function Last_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
   begin
      case Node.Label is
      when Terminal_Label =>
         return Node;

      when Nonterm =>
         for C of reverse Node.Children loop
            --  We tolerate null C here because this function is called while
            --  printing a tree for debug.
            if C /= Invalid_Node_Access then
               Parents.Push (Node);
               declare
                  Last_Term : constant Node_Access := Last_Terminal (Tree, C, Parents);
               begin
                  if Last_Term /= Invalid_Node_Access then
                     return Last_Term;
                  else
                     Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to Last_Terminal.
                  end if;
               end;
            end if;
         end loop;
         --  All children are empty
         return Invalid_Node_Access;
      end case;
   end Last_Terminal;

   procedure Last_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Preceding    : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      if Ref.Ref.Node = Invalid_Node_Access then
         --  Ref is an empty nonterm.
         null;
      else
         Ref.Ref.Node := Last_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
      end if;

      if Preceding then
         loop
            exit when Ref.Ref.Node /= Invalid_Node_Access;
            Prev_Terminal (Tree, Ref, Parse_Stream, Preceding);
            exit when not Has_Element (Ref.Ref.Element.Cur);
         end loop;
      elsif Ref.Ref.Node = Invalid_Node_Access then
         Ref.Parents.Clear;
      end if;
   end Last_Terminal;

   procedure Left_Breakdown
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Ref;
      User_Data : in     Syntax_Trees.User_Data_Access_Constant)
   is
      --  [Wagner Graham 1998] doesn't modify the tree structure for
      --  Left_Breakdown; it just moves the Current_Token pointer around.
      --  That means the rest of the parser must understand that.
      --
      --  Here we actually decompose the tree, as in [Lahav 2008]. Empty
      --  nonterms are handled by caller.
      --
      --  Any errors on stream elements that are deleted along the way are
      --  moved to their First_Terminal, which is always the same node; the
      --  node that is the last promoted to the parse stream.
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Stream.Cur);

      Cur         : Cursor                       := Ref.Element.Cur;
      To_Delete   : Cursor                       := Cur;
      Node        : Valid_Node_Access            := Stream_Element_Lists.Element (Cur).Node;
      Next_I      : Positive_Index_Type;
      First_Child : constant Positive_Index_Type := 1; -- preserve symmetry with Right_Breakdown
      New_Errors  : Error_Data_Lists.List;

   begin
      Ref.Element.Cur := No_Element; --  Allow deleting via To_Delete.

      loop
         Next_I := Positive_Index_Type'Last;

         for I in reverse 2 .. Node.Child_Count loop
            if Node.Children (I).Child_Count > 0 or Node.Children (I).Label in Terminal_Label then
               Next_I := I;
            end if;

            Cur := Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => Node.Children (I),
                  State => Unknown_State),
               Before   => Cur);

            Node.Children (I).Parent := Invalid_Node_Access;

            --  We don't set node.children (I) to invalid here, because we need it
            --  if Next_I /= First_Child.
         end loop;

         Node.Children (First_Child).Parent := Invalid_Node_Access;

         if Node.Children (First_Child).Child_Count > 0 or Node.Children (First_Child).Label in Terminal_Label then
            Next_I := First_Child;
         else
            --  Node.Children (First_Child) is an empty nonterm; it has not
            --  been added to stream.
            if Next_I = Positive_Index_Type'Last then
               --  Node is an empty nonterm; move to first sibling below.
               null;
            else
               --  First non_empty is in Node.Children (Next_I); delete leading empty
               --  nonterms that were added to the stream.
               for I in First_Child + 1 .. Next_I - 1 loop
                  declare
                     To_Delete_2 : Cursor := Cur;
                  begin
                     Next (Cur);
                     --  We do not set errors on empty nonterms.
                     pragma Assert (Stream_Element_Lists.Element (To_Delete_2).Node.Error_List = null);
                     Parse_Stream.Elements.Delete (To_Delete_2);
                  end;
               end loop;
               pragma Assert (Element (Cur).Node = Node.Children (Next_I));

               --  Delete the nonterm that we were breaking down, and record the one
               --  we are now breaking down for deletion.
               declare
                  Node : constant Valid_Node_Access := Stream_Element_Lists.Element (To_Delete).Node;
               begin
                  if Node.Error_List /= null then
                     for Err of Tree.Error_List (Node) loop
                        New_Errors.Append (To_Message (Err, Tree, Node));
                     end loop;
                  end if;
               end;
               Parse_Stream.Elements.Delete (To_Delete);
               To_Delete := Cur;
            end if;
         end if;

         declare
            Temp : constant Node_Access := Node;
         begin
            if Next_I = Positive_Index_Type'Last then
               --  Node is an empty nonterm; move to first sibling. Possibly similar
               --  to test_incremental.adb Recover_04.
               raise SAL.Not_Implemented with "FIXME: Syntax_Trees.Left_Breakdown move to next sibling.";
            else
               Node := Node.Children (Next_I);
            end if;

            --  Now we can clear the children of Temp (was Node).
            if Tree.Parents_Set then
               Temp.Children := (others => Invalid_Node_Access);
            end if;
         end;

         if Node.Label in Terminal_Label then
            if To_Delete /= Cur and Next_I /= First_Child then
               Ref.Element.Cur := Cur;

            else
               Ref.Element.Cur := Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Node,
                     State => Unknown_State),
                  Before   => Cur);

               Cur := No_Element; --  allow delete via To_Delete
            end if;

            Ref.Node := Node;

            declare
               Node : constant Valid_Node_Access := Stream_Element_Lists.Element (To_Delete).Node;
            begin
               if Node.Error_List /= null then
                  for Err of Tree.Error_List (Node) loop
                     New_Errors.Append (To_Message (Err, Tree, Node));
                  end loop;
               end if;
            end;
            Parse_Stream.Elements.Delete (To_Delete);
            exit;
         end if;
      end loop;

      if New_Errors.Length > 0 then
         Ref.Node := Add_Errors (Tree, Ref.Node, New_Errors, User_Data);
         Replace_Node (Ref.Element, Ref.Node);
      end if;
   end Left_Breakdown;

   function Lexable (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Streams.Length = 1 and
        (Tree.Shared_Stream.Cur /= Parse_Stream_Lists.No_Element and then
           Tree.Streams (Tree.Shared_Stream.Cur).Elements.Length = 1);
   end Lexable;

   function Line_At_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Byte_Pos : in Buffer_Pos)
     return Base_Line_Number_Type
   is
      function Line_At_Byte_Pos
        (Node       : in Valid_Node_Access;
         Start_Line : in WisiToken.Line_Number_Type)
        return Base_Line_Number_Type
      is
         Prev_Term : constant Valid_Node_Access :=
           (if Node = Tree.Root
            then Tree.SOI
            elsif Node = Tree.SOI
            then Tree.SOI
            else Tree.Prev_Source_Terminal (Node, Trailing_Non_Grammar => True));
      begin
         --  Byte_Pos can be in whitespace or non_grammar, so we construct the
         --  region to check from two successive terminals. We need line number
         --  information, so we find the prev/next terminals with new_lines in
         --  the token or non_grammar.
         case Node.Label is
         when Terminal_Label =>
            declare
               Prev_New_Line : constant New_Line_Ref := Tree.Prev_New_Line (Node, Start_Line);
               Check_Region : constant Buffer_Region :=
                 (First =>
                    (if Node = Tree.SOI
                     then Tree.Byte_Region (Tree.SOI, Trailing_Non_Grammar => True).First
                     else Tree.Byte_Region (Prev_Term, Trailing_Non_Grammar => True).Last + 1),
                  Last  =>
                    (if Node = Tree.EOI
                     then Node.Non_Grammar (1).Byte_Region.First
                     else Tree.Byte_Region (Node, Trailing_Non_Grammar => True).Last));

               function Check_Non_Grammar return Base_Line_Number_Type
               --  Return Invalid_Line_Number if Byte_Pos not in Node.Non_Grammar
               is begin
                  if Node.Non_Grammar.Length > 0 and then
                    Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last
                  then
                     for Token of Node.Non_Grammar loop
                        if Byte_Pos <= Token.Byte_Region.First then
                           return Token.Line_Region.First;

                        elsif Byte_Pos <= Token.Byte_Region.Last then
                           return Tree.Lexer.Line_At_Byte_Pos (Token, Byte_Pos);

                        end if;
                     end loop;
                  end if;
                  return Invalid_Line_Number;
               end Check_Non_Grammar;

            begin
               pragma Assert (Prev_New_Line.Pos /= Invalid_Buffer_Pos); --  SOI if nothing else

               if not Contains (Check_Region, Byte_Pos) then
                  return Invalid_Line_Number;

               else
                  case Terminal_Label'(Node.Label) is
                  when Virtual_Identifier | Virtual_Terminal =>
                     --  ada_mode-partial_parse.adb
                     declare
                        Temp : constant Base_Line_Number_Type := Check_Non_Grammar;
                     begin
                        if Temp = Invalid_Line_Number then
                           return Prev_New_Line.Line;
                        else
                           return Temp;
                        end if;
                     end;

                  when Source_Terminal =>
                     if Node = Tree.EOI then
                        return Node.Non_Grammar (1).Line_Region.First;

                     elsif Byte_Pos < Node.Byte_Region.First then
                        --  In whitespace before token
                        return Prev_New_Line.Line;

                     elsif Byte_Pos <= Node.Byte_Region.Last then
                        return Tree.Lexer.Line_At_Byte_Pos
                          (Node.Byte_Region, Byte_Pos,
                           First_Line => Prev_New_Line.Line);

                     else
                        declare
                           Temp : constant Base_Line_Number_Type := Check_Non_Grammar;
                        begin
                           if Temp = Invalid_Line_Number then
                              raise SAL.Programmer_Error;
                           else
                              return Temp;
                           end if;
                        end;
                     end if;
                  end case;
               end if;
            end;

         when Nonterm =>
            declare
               First_Term : constant Node_Access  := Tree.First_Source_Terminal
                 (Node, Trailing_Non_Grammar => True, Following => False);
               Last_Term  : constant Node_Access  := Tree.Last_Source_Terminal (Node, Trailing_Non_Grammar  => True);
            begin
               if First_Term = Invalid_Node_Access then
                  --  Empty or all virtual
                  return Invalid_Line_Number;

               elsif not Contains
                 ((First =>
                     (if Node = Tree.Root
                      then Tree.Byte_Region (Tree.SOI, Trailing_Non_Grammar => True).First
                      else Tree.Byte_Region (Prev_Term, Trailing_Non_Grammar => True).Last + 1),
                   Last  =>
                    (if Last_Term = Tree.EOI
                     then Last_Term.Non_Grammar (1).Byte_Region.First
                     else Tree.Byte_Region (Last_Term, Trailing_Non_Grammar => True).Last)),
                  Byte_Pos)
               then
                  return Invalid_Line_Number;

               else
                  for Child of Node.Children loop
                     declare
                        Temp : constant Base_Line_Number_Type := Line_At_Byte_Pos
                          (Child, Start_Line => Tree.Prev_New_Line (Child, Start_Line).Line);
                     begin
                        if Temp = Invalid_Line_Number then
                           --  Check next child
                           null;
                        else
                           return Temp;
                        end if;
                     end;
                  end loop;
                  raise SAL.Programmer_Error; --  Contains said we'd find it.
               end if;
            end;
         end case;
      end Line_At_Byte_Pos;
   begin
      return Line_At_Byte_Pos (Tree.Root, Start_Line => Tree.SOI.Non_Grammar (1).Line_Region.First);
   end Line_At_Byte_Pos;

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Buffer_Pos
   is
      Node           : Node_Access := Tree.Root;
      Begin_Char_Pos : Buffer_Pos  := Invalid_Buffer_Pos;
   begin
      if Line = Line_Number_Type'First then
         return Buffer_Pos'First;
      end if;

      Node := Find_New_Line (Tree, Line, Node, Begin_Char_Pos);
      return Begin_Char_Pos;
   end Line_Begin_Char_Pos;

   function Line_Begin_Char_Pos
     (Tree   : in Syntax_Trees.Tree;
      Line   : in Line_Number_Type;
      Stream : in Stream_ID)
     return Buffer_Pos
   is
      Begin_Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
   begin
      if Line = Line_Number_Type'First then
         return Buffer_Pos'First;
      end if;

      declare
         Ref : Stream_Node_Parents;
      begin
         Ref.Ref := Tree.Stream_First (Stream, Skip_SOI => False);
         Find_New_Line (Tree, Ref, Stream, Line, Begin_Char_Pos);
         if Ref.Ref.Node = Invalid_Node_Access then
            return Invalid_Buffer_Pos;
         else
            return Begin_Char_Pos;
         end if;
      end;
   end Line_Begin_Char_Pos;

   function Line_Begin_Token
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   is begin
      declare
         Node : constant Node_Access := Tree.First_Non_Grammar (Root (Tree));
      begin
         if Node = Invalid_Node_Access then
            --  Tree has no tokens with a Line_Region. Note that during LR parse, EOI
            --  is not in the tree, only in the parse stream.
            return Invalid_Node_Access;
         end if;

         if Line = Tree.Line_Region (Node, Trailing_Non_Grammar => True).First then
            return Node;
         elsif Line < Tree.Line_Region (Node, Trailing_Non_Grammar => True).First then
            return Invalid_Node_Access;
         end if;
      end;

      declare
         Begin_Char_Pos : Buffer_Pos;
         Node           : Node_Access := Find_New_Line (Tree, Line, Root (Tree), Begin_Char_Pos);
      begin
         if Node = Invalid_Node_Access then
            --  Line is after EOI.
            return Invalid_Node_Access;

         elsif Node.ID = Tree.Lexer.Descriptor.EOI_ID then
            --  Find_New_Line allows both Line, Line - 1.
            if Node.Non_Grammar (Node.Non_Grammar.First_Index).Line_Region.First = Line then
               return Node;
            else
               return Invalid_Node_Access;
            end if;

         else
            --  Node now contains the non-grammar that ends Line - 1
            if Empty_Line (Tree, Node.Non_Grammar, Line) then
               return Invalid_Node_Access;
            else
               Next_Terminal (Tree, Node);
               return Node;
            end if;
         end if;
      end;
   end Line_Begin_Token;

   function Line_Begin_Token
     (Tree                      : in Syntax_Trees.Tree;
      Line                      : in Line_Number_Type;
      Stream                    : in Stream_ID;
      Following_Source_Terminal : in Boolean)
     return Node_Access
   is
      Ref             : Stream_Node_Parents;
      Begin_Char_Pos  : Buffer_Pos;

      EOI_Line : constant Line_Number_Type := Tree.EOI.Non_Grammar (Tree.EOI.Non_Grammar.First).Line_Region.First;
   begin
      Ref.Ref := Stream_First (Tree, Stream, Skip_SOI => True);

      if Line = Line_Number_Type'First then
         if Line = Tree.Line_Region (Ref, Stream).First then
            return Tree.First_Terminal (Ref.Ref.Node);
         else
            if Following_Source_Terminal then
               Next_Source_Terminal (Tree, Ref, Trailing_Non_Grammar => False);
               return Ref.Ref.Node;
            else
               return Invalid_Node_Access;
            end if;
         end if;

      elsif Line = EOI_Line + 1 then
         return Tree.EOI;

      elsif Line > EOI_Line + 1 then
         return Invalid_Node_Access;
      end if;

      Find_New_Line (Tree, Ref, Stream, Line, Begin_Char_Pos);

      if Ref.Ref = Invalid_Stream_Node_Ref then
         return Invalid_Node_Access;
      else
         --  Ref now contains the non-grammar that ends Line - 1, or EOI.

         if Ref.Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID then
            --  test_incremental.adb Edit_String_10
            return Invalid_Node_Access;

         elsif Empty_Line (Tree, Ref.Ref.Node.Non_Grammar, Line) then
            if Following_Source_Terminal then
               Next_Source_Terminal (Tree, Ref, Trailing_Non_Grammar => False);
               return Ref.Ref.Node;
            else
               return Invalid_Node_Access;
            end if;
         else
            Next_Terminal (Tree, Ref, Following => True);
            return Ref.Ref.Node;
         end if;
      end if;
   end Line_Begin_Token;

   procedure  Line_Region_Internal_1
     (Tree                    : in     Syntax_Trees.Tree;
      Node                    : in     Node_Access;
      Prev_Non_Grammar        : in     Valid_Node_Access;
      Next_Non_Grammar        : in     Valid_Node_Access;
      Trailing_Non_Grammar    : in     Boolean;
      First_Non_Grammar_Token :    out WisiToken.Lexer.Token;
      Last_Non_Grammar_Token  :    out WisiToken.Lexer.Token)
   is
      --  Since all non_grammar have line_region, we don't have to look for
      --  a new_line, just any non_grammar.
      --
      --  We always have to find a previous and next non_grammar, to allow
      --  for multi-line tokens.
      --
      --  The last few tokens in a nonterm may have no non_grammar; then we
      --  have to find the following Non_Grammar.

      Last_Non_Grammar : constant Syntax_Trees.Node_Access :=
        (if Node = Invalid_Node_Access
         then Invalid_Node_Access
         else Tree.Last_Non_Grammar (Node));

      Last_Terminal : constant Syntax_Trees.Node_Access :=
        (if Node = Invalid_Node_Access
         then Invalid_Node_Access
         else Tree.Last_Terminal (Node));

      Actual_Last_Non_Grammar : constant Syntax_Trees.Valid_Node_Access :=
        (if Last_Non_Grammar = Invalid_Node_Access
         then Next_Non_Grammar
         elsif Last_Non_Grammar = Last_Terminal
         then Last_Non_Grammar
         else Next_Non_Grammar);
   begin
      First_Non_Grammar_Token :=
        (if (Node = Tree.Root and Prev_Non_Grammar.ID = Tree.Lexer.Descriptor.SOI_ID)
           --  We are finding the line_region of wisi_accept in an Editable
           --  tree; we want to include the leading non_grammar in SOI.
           or Node.ID = Tree.Lexer.Descriptor.SOI_ID
           --  We are finding the line_region of SOI.
         then
            Prev_Non_Grammar.Non_Grammar (Prev_Non_Grammar.Non_Grammar.First_Index)
         else
            --  We are finding the line_region of a leading non_terminal; we don't
            --  want to include the leading non_grammar in SOI.
            Prev_Non_Grammar.Non_Grammar (Prev_Non_Grammar.Non_Grammar.Last_Index));

      Last_Non_Grammar_Token := Actual_Last_Non_Grammar.Non_Grammar
        (if Trailing_Non_Grammar and Actual_Last_Non_Grammar = Last_Non_Grammar
         then Actual_Last_Non_Grammar.Non_Grammar.Last_Index
         else Actual_Last_Non_Grammar.Non_Grammar.First_Index);
   end Line_Region_Internal_1;

   function Line_Region_Internal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Prev_Non_Grammar     : in Valid_Node_Access;
      Next_Non_Grammar     : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return WisiToken.Line_Region
   is
      First_Non_Grammar_Token : WisiToken.Lexer.Token;
      Last_Non_Grammar_Token  : WisiToken.Lexer.Token;
   begin
      Line_Region_Internal_1
        (Tree, Node, Prev_Non_Grammar, Next_Non_Grammar, Trailing_Non_Grammar,
         First_Non_Grammar_Token, Last_Non_Grammar_Token);

      return
        (First => First_Non_Grammar_Token.Line_Region.Last,
         Last => Last_Non_Grammar_Token.Line_Region.First);
   end Line_Region_Internal;

   function Byte_Region_Of_Line_Region_Internal
     (Tree                : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Prev_Non_Grammar     : in Valid_Node_Access;
      Next_Non_Grammar     : in Valid_Node_Access;
      Trailing_Non_Grammar : in     Boolean)
     return WisiToken.Buffer_Region
   is
      First_Non_Grammar_Token : WisiToken.Lexer.Token;
      Last_Non_Grammar_Token  : WisiToken.Lexer.Token;
   begin
      Line_Region_Internal_1
        (Tree, Node, Prev_Non_Grammar, Next_Non_Grammar, Trailing_Non_Grammar,
         First_Non_Grammar_Token, Last_Non_Grammar_Token);

      return
        (First => First_Non_Grammar_Token.Byte_Region.Last,
         Last => Last_Non_Grammar_Token.Byte_Region.First);
   end Byte_Region_Of_Line_Region_Internal;

   function Line_Region (Tree : in Syntax_Trees.Tree) return WisiToken.Line_Region
   is begin
      return
        (First => Tree.SOI.Non_Grammar (1).Line_Region.First,
         Last  => Tree.EOI.Non_Grammar (1).Line_Region.Last);
   end Line_Region;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return WisiToken.Line_Region
   is
      Prev_Non_Grammar     : constant Node_Access := Tree.Prev_Non_Grammar (Node);
      Next_Non_Grammar     : constant Node_Access := Tree.Next_Non_Grammar (Node);
   begin
      if Prev_Non_Grammar = Invalid_Node_Access or Next_Non_Grammar = Invalid_Node_Access then
         --  Tolerate this because used in error messages.
         return Null_Line_Region;
      else
         return Line_Region_Internal
           (Tree, Node,
            Prev_Non_Grammar     => Tree.Prev_Non_Grammar (Node),
            Next_Non_Grammar     => Tree.Next_Non_Grammar (Node),
            Trailing_Non_Grammar => Trailing_Non_Grammar);
      end if;
   end Line_Region;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean)
     return WisiToken.Line_Region
   is begin
      if Tree.Parents_Set then
         declare
            Prev_Non_Grammar : Stream_Node_Ref := Ref;
            Next_Non_Grammar : Stream_Node_Ref := Ref;
         begin
            Tree.Prev_Non_Grammar (Prev_Non_Grammar);
            Tree.Next_Non_Grammar (Next_Non_Grammar);
            return Line_Region_Internal
              (Tree, Ref.Node, Prev_Non_Grammar.Node, Next_Non_Grammar.Node, Trailing_Non_Grammar);
         end;

      else
         return Line_Region (Tree, To_Stream_Node_Parents (Tree, Ref), Ref.Stream, Trailing_Non_Grammar);
      end if;
   end Line_Region;

   function Byte_Region_Of_Line_Region
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return WisiToken.Buffer_Region
   is
      Prev_Non_Grammar : Stream_Node_Ref := Ref;
      Next_Non_Grammar : Stream_Node_Ref := Ref;
   begin
      Tree.Prev_Non_Grammar (Prev_Non_Grammar);
      Tree.Next_Non_Grammar (Next_Non_Grammar);
      return Byte_Region_Of_Line_Region_Internal
        (Tree, Ref.Node, Prev_Non_Grammar.Node, Next_Non_Grammar.Node, Trailing_Non_Grammar => True);
   end Byte_Region_Of_Line_Region;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Parents;
      Parse_Stream         : in Stream_ID;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   is
      Prev_Non_Grammar : Stream_Node_Parents := Ref;
      Next_Non_Grammar : Stream_Node_Parents := Ref;
   begin
      Tree.Prev_Non_Grammar (Prev_Non_Grammar, Parse_Stream);
      Tree.Next_Non_Grammar (Next_Non_Grammar);
      return Line_Region_Internal
        (Tree, Ref.Ref.Node, Prev_Non_Grammar.Ref.Node, Next_Non_Grammar.Ref.Node, Trailing_Non_Grammar);
   end Line_Region;

   function Line_Region
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Real_Recover_Token)
     return WisiToken.Line_Region
   is
      function Find_Element return Stream_Index
      is
         use Stream_Element_Lists;
         Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
         Cur : Cursor := Parse_Stream.Stack_Top;
      begin
         loop
            exit when not Has_Element (Cur);
            if Stream_Element_Lists.Element (Cur).Node = Ref.Element_Node then
               return (Cur => Cur);
            end if;
            Next (Cur);
         end loop;

         --  Not found in stream input; search stack.
         Cur := Parse_Stream.Stack_Top;
         loop
            Previous (Cur);
            exit when not Has_Element (Cur);
            if Stream_Element_Lists.Element (Cur).Node = Ref.Element_Node then
               return (Cur => Cur);
            end if;
         end loop;
         return Invalid_Stream_Index;
      end Find_Element;

      Element : constant Stream_Index := Find_Element;
   begin
      if Element = Invalid_Stream_Index then
         return Null_Line_Region;
      else
         declare
            Ref_Parents : Stream_Node_Parents :=
              (Ref     => (Stream, Element, Get_Node (Element)),
               Parents => <>);
         begin
            Tree.First_Terminal (Ref_Parents, Following => True);
            return Line_Region (Tree, Ref_Parents, Stream, Trailing_Non_Grammar => True);
         end;
      end if;
   end Line_Region;

   function Make_Rooted (Item : in Recover_Token) return Recover_Token
   is begin
      if Item.Virtual then
         return Item;
      elsif Item.Element_Node = Item.Node then
         return Item;
      else
         return
           (Virtual => False,
            Element_Node => Item.Element_Node,
            Node => Item.Element_Node);
      end if;
   end Make_Rooted;

   type Augmented_In_Tree is new Base_Augmented with record I : Integer; end record;
   Aug_In_Tree : constant Augmented_In_Tree := (Base_Augmented with 1);

   procedure Mark_In_Tree
     (Tree                : in     Syntax_Trees.Tree;
      Node                : in     Valid_Node_Access;
      Data                : in out User_Data_Type'Class;
      Node_Error_Reported : in out Boolean)
   is
      pragma Unreferenced (Data, Node_Error_Reported);
   begin
      if Node.Augmented /= null then
         raise SAL.Programmer_Error with
           (if Node.Augmented.all in Augmented_In_Tree
            then "Mark_In_Tree called twice on node " & Tree.Image (Node, Node_Numbers => True)
            else "Mark_In_Tree called with Augmented already set");
      end if;
      Node.Augmented := new Augmented_In_Tree'(Aug_In_Tree);

      if Node.Label = Source_Terminal then
         for N of Node.Following_Deleted loop
            N.Augmented := new Augmented_In_Tree'(Aug_In_Tree);
         end loop;
      end if;
   end Mark_In_Tree;

   procedure Move_Element
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Ref       : in out Stream_Node_Parents;
      New_Node  : in     Valid_Node_Access;
      User_Data : in     User_Data_Access_Constant)
   --  Move Ref to Stream, replacing Ref.Node with New_Node,
   --  copying all ancestors. Update Ref to point to new stream element
   --  with copied nodes.
   is
      --  We don't use Move_Shared_To_Input, because that doesn't deep copy the
      --  node.
      Orig_Element_Node : constant Valid_Node_Access := Get_Node (Ref.Ref.Element);

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Ref.Ref.Stream /= Stream then
         declare
            New_Element : constant Stream_Element_Lists.Cursor :=
              Parse_Stream.Elements.Insert
                (Element  =>
                   (Node  =>
                      (if Orig_Element_Node = Ref.Ref.Node
                       then New_Node
                       else Copy_Node
                         (Tree, Orig_Element_Node,
                          Parent                 => Invalid_Node_Access,
                          User_Data              => User_Data,
                          Copy_Children          => False,
                          Copy_Following_Deleted => True)),

                    State => Unknown_State),
                 Before   => Stream_Element_Lists.Next (Parse_Stream.Stack_Top));
         begin
            if Ref.Ref.Stream = Tree.Shared_Stream and then
              Ref.Ref.Element.Cur = Parse_Stream.Shared_Link
            then
               Stream_Element_Lists.Next (Parse_Stream.Shared_Link);
            end if;

            --  Don't set Ref.Ref.Node yet; needed by Copy_Ancestors below.
            Ref.Ref.Stream      := Stream;
            Ref.Ref.Element.Cur := New_Element;

            if Ref.Parents.Depth > 0 then
               Ref.Parents.Set
                 (Index   => Ref.Parents.Depth,
                  Depth   => Ref.Parents.Depth,
                  Element => Get_Node (Ref.Ref.Element));
            end if;
         end;

      elsif Orig_Element_Node = Ref.Ref.Node then
         Replace_Node (Ref.Ref.Element, New_Node);
      end if;

      if Orig_Element_Node = Ref.Ref.Node then
         Ref.Ref.Node := New_Node;
      else
         --  Edit child links in ancestors, update Ref.Parents to match.
         Copy_Ancestors (Tree, Ref, New_Node, User_Data);
      end if;
   end Move_Element;

   procedure Move_Shared_To_Input
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Before       : constant Cursor := Next (Parse_Stream.Stack_Top);
   begin
      Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Element (Parse_Stream.Shared_Link).Node,
            State => Unknown_State),
         Before => Before);
      Next (Parse_Stream.Shared_Link);
   end Move_Shared_To_Input;

   procedure Move_Shared_To_Input
     (Tree   : in out Syntax_Trees.Tree;
      First  : in     Stream_Node_Ref;
      Last   : in     Stream_Node_Ref;
      Stream : in     Stream_ID)
   is
      use Stream_Element_Lists;
      Temp         : Stream_Node_Ref := First;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Before       : constant Cursor := Next (Parse_Stream.Stack_Top);
   begin
      loop
         Parse_Stream.Elements.Insert
           (Element  =>
              (Node  => Element (Temp.Element.Cur).Node,
               State => Unknown_State),
            Before => Before);

         exit when Temp.Element = Last.Element;
         Tree.Stream_Next (Temp, Rooted => True);
      end loop;

      Tree.Stream_Next (Temp, Rooted => True);
      Parse_Stream.Shared_Link := Temp.Element.Cur;
   end Move_Shared_To_Input;

   function Name (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region
   is begin
      if Item.Virtual then
         if Item.Name = Null_Buffer_Region then
            if Item.First_Terminal = Invalid_Node_Access  or else
              Tree.Byte_Region (Item.First_Terminal, Trailing_Non_Grammar => False) = Null_Buffer_Region
            then
               return Null_Buffer_Region;
            else
               if Item.First_Terminal = Invalid_Node_Access  or else
                 Tree.Byte_Region (Item.Last_Terminal, Trailing_Non_Grammar => False) = Null_Buffer_Region
               then
                  return Null_Buffer_Region;
               else
                  return
                    (Tree.Byte_Region (Item.First_Terminal, Trailing_Non_Grammar => False).First,
                     Tree.Byte_Region (Item.Last_Terminal, Trailing_Non_Grammar => False).Last);
               end if;
            end if;
         else
            return Item.Name;
         end if;
      else
         return Tree.Name (Item.Element_Node);
      end if;
   end Name;

   function Name (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Buffer_Region
   is begin
      case Node.Label is
      when Nonterm =>
         if Node.Name_Length = 0 then
            return Tree.Byte_Region (Node, Trailing_Non_Grammar => False);
         else
            declare
               First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
               Byte_First     : constant Buffer_Pos  := Tree.Byte_Region
                 (First_Terminal, Trailing_Non_Grammar => False).First;
            begin
               return
                 (Byte_First + Node.Name_Offset,
                  Byte_First + Node.Name_Offset + Node.Name_Length - 1);
            end;
         end if;

      when Source_Terminal =>
         return Node.Byte_Region;

      when Virtual_Terminal_Label =>
         return Null_Buffer_Region;
      end case;
   end Name;

   function Name (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Buffer_Region
   is begin
      --  We use the Element node because the nonterminal has the most valid Name.
      return Tree.Name (Stream_Element_Lists.Element (Ref.Element.Cur).Node);
   end Name;

   overriding function Next
     (Object   : Error_Iterator;
      Position : Error_Ref)
     return Error_Ref
   is begin
      return Result : Error_Ref := Position do
         Object.Tree.Next_Error (Result);
      end return;
   end Next;

   overriding function Next
     (Object   : Stream_Error_Iterator;
      Position : Stream_Error_Cursor)
     return Stream_Error_Cursor
   is begin
      return Result : Stream_Error_Cursor := Position do
         Object.Tree.Next_Error (Result.SER);
      end return;
   end Next;

   procedure Next_Error (Tree : in Syntax_Trees.Tree; Error : in out Error_Ref)
   is
      pragma Unreferenced (Tree);
      use Valid_Node_Access_Lists;
      use Error_Data_Lists;
   begin
      if Has_Element (Error.Error) then
         Next (Error.Error);
      end if;
      if Has_Element (Error.Error) then
         return;
      end if;
      if Has_Element (Error.Deleted) then
         Next (Error.Deleted);
         if not Has_Element (Error.Deleted) then
            Next_Node (Error.Node);
         end if;
         First_Error (Error);
      else
         if Error.Node.Error_List = null then
            if Error.Node.Label = Source_Terminal then
               Error.Deleted := Error.Node.Following_Deleted.First;
            else
               Next_Node (Error.Node);
               if Error.Node = Invalid_Node_Access then
                  --  No more errors.
                  return;
               end if;
            end if;

            First_Error (Error);
         else
            if Has_Element (Error.Error) then
               Next (Error.Error);
            end if;
            if not Has_Element (Error.Error) then
               if Error.Node.Label = Source_Terminal then
                  Error.Deleted := Error.Node.Following_Deleted.First;
                  if not Has_Element (Error.Deleted) then
                     Next_Node (Error.Node);
                  end if;
               else
                  Next_Node (Error.Node);
               end if;

               if Error.Node = Invalid_Node_Access then
                  --  No more errors.
                  return;
               end if;
               First_Error (Error);
            end if;
         end if;
      end if;
   end Next_Error;

   procedure Next_Error (Tree : in Syntax_Trees.Tree; Error : in out Stream_Error_Ref)
   is
      use Valid_Node_Access_Lists;
      use Error_Data_Lists;
   begin
      if Has_Element (Error.Error) then
         Next (Error.Error);
      end if;
      if Has_Element (Error.Error) then
         return;
      end if;
      if Has_Element (Error.Deleted) then
         Next (Error.Deleted);
         if not Has_Element (Error.Deleted) then
            Next_Node (Tree, Error.Ref);
         end if;
         First_Error (Tree, Error);
      else
         if Error.Ref.Ref.Node.Error_List = null then
            if Error.Ref.Ref.Node.Label = Source_Terminal then
               Error.Deleted := Error.Ref.Ref.Node.Following_Deleted.First;
            else
               Next_Node (Tree, Error.Ref);
               if Error.Ref.Ref.Node = Invalid_Node_Access then
                  --  No more errors.
                  return;
               end if;
            end if;

            First_Error (Tree, Error);
         else
            if Has_Element (Error.Error) then
               Next (Error.Error);
            end if;
            if not Has_Element (Error.Error) then
               if Error.Ref.Ref.Node.Label = Source_Terminal then
                  Error.Deleted := Error.Ref.Ref.Node.Following_Deleted.First;
                  if not Has_Element (Error.Deleted) then
                     Next_Node (Tree, Error.Ref);
                  end if;
               else
                  Next_Node (Tree, Error.Ref);
               end if;

               if Error.Ref.Ref.Node = Invalid_Node_Access then
                  --  No more errors.
                  return;
               end if;
               First_Error (Tree, Error);
            end if;
         end if;
      end if;
   end Next_Error;

   procedure Next_New_Line
     (Tree               : in     Syntax_Trees.Tree;
      Start_Ref          : in     Terminal_Ref;
      After_Non_Grammar  : in     Positive_Index_Type;
      Result_Ref         :    out Terminal_Ref;
      Result_Non_Grammar :    out Positive_Index_Type)
   is
      Index : Lexer.Token_Arrays.Extended_Index := After_Non_Grammar;
   begin
      Result_Ref := Start_Ref;

      loop
         Next_Non_Grammar :
         loop
            if Index /= Lexer.Token_Arrays.No_Index and
              Index < Result_Ref.Node.Non_Grammar.Last_Index
            then
               Index := @ + 1;
            else
               Next_Terminal (Tree, Result_Ref);
               if Result_Ref.Node.Non_Grammar.Length > 0 then
                  Index := Result_Ref.Node.Non_Grammar.First_Index;
                  exit Next_Non_Grammar;
               else
                  Index := Lexer.Token_Arrays.No_Index;
               end if;
            end if;
         end loop Next_Non_Grammar;

         exit when Result_Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID or
           Contains_New_Line (Result_Ref.Node.Non_Grammar (Index).Line_Region);
      end loop;
      Result_Non_Grammar := Index;
   end Next_New_Line;

   procedure Next_Node (Node : in out Node_Access)
   is
      procedure Next_Sibling
      is begin
         loop
            if Node.Parent = Invalid_Node_Access then
               Node := Invalid_Node_Access;
               return;
            else
               declare
                  Child_Index : constant Positive_Index_Type := Syntax_Trees.Child_Index (Node.Parent.all, Node);
               begin
                  if Child_Index = Node.Parent.Child_Count then
                     Node := Node.Parent;
                  else
                     Node := Node.Parent.Children (Child_Index + 1);
                     exit;
                  end if;
               end;
            end if;
         end loop;
      end Next_Sibling;

   begin
      case Node.Label is
      when Terminal_Label =>
         Next_Sibling;

      when Nonterm =>
         if Node.Child_Count > 0 then
            Node := Node.Children (1);
         else
            Next_Sibling;
         end if;
      end case;
   end Next_Node;

   procedure Next_Node (Tree : in Syntax_Trees.Tree; Node : in out Stream_Node_Parents)
   is
      procedure Next_Sibling
      is begin
         loop
            if Node.Parents.Depth = 0 then
               if Node.Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID then
                  Node.Ref := Invalid_Stream_Node_Ref;
                  return;
               else
                  Stream_Next (Tree, Node.Ref, Rooted => True);
                  return;
               end if;
            else
               declare
                  Child_Index : constant Positive_Index_Type := Syntax_Trees.Child_Index
                    (Node.Parents.Peek.all, Node.Ref.Node);
               begin
                  if Child_Index = Node.Parents.Peek.Child_Count then
                     Node.Ref.Node := Node.Parents.Pop;
                  else
                     Node.Ref.Node := Node.Parents.Peek.Children (Child_Index + 1);
                     exit;
                  end if;
               end;
            end if;
         end loop;
      end Next_Sibling;

   begin
      case Node.Ref.Node.Label is
      when Terminal_Label =>
         Next_Sibling;

      when Nonterm =>
         if Node.Ref.Node.Child_Count > 0 then
            Node.Parents.Push (Node.Ref.Node);
            Node.Ref.Node := Node.Ref.Node.Children (1);
         else
            Next_Sibling;
         end if;
      end case;
   end Next_Node;

   function Next_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Valid_Node_Access
   is
      Result : Node_Access := Node;
   begin
      if Node.ID = Tree.Lexer.Descriptor.EOI_ID then
         return Node;

      elsif Node = Tree.Root then
         return Tree.EOI;
      end if;

      loop
         Result := Next_Terminal (Tree, Result);
         exit when Result.Non_Grammar.Length > 0;
      end loop;
      return Result;
   end Next_Non_Grammar;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Ref)
   is begin
      if Ref.Node /= Invalid_Node_Access and then Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID then
         return;
      end if;
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Node = Invalid_Node_Access;
         exit when Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Next_Non_Grammar;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Parents)
   is begin
      if Ref.Ref.Node /= Invalid_Node_Access and then Ref.Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID then
         return;
      end if;
      loop
         Next_Terminal (Tree, Ref, Following => True);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Next_Non_Grammar;

   procedure Next_Nonterm (Tree : in Syntax_Trees.Tree; Ref : in out Stream_Node_Ref)
   is
      procedure Next_Sibling
      is begin
         loop
            if Ref.Node.Parent = Invalid_Node_Access then
               loop
                  Stream_Next (Tree, Ref, Rooted => True);
                  if Ref = Invalid_Stream_Node_Ref then
                     return;

                  elsif Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID then
                     Ref := Invalid_Stream_Node_Ref;
                     return;
                  elsif Ref.Node.Label = Nonterm then
                     return;
                  else
                     null;
                  end if;
               end loop;
            else
               declare
                  Child_Index : constant Positive_Index_Type := Syntax_Trees.Child_Index
                    (Ref.Node.Parent.all, Ref.Node);
               begin
                  if Child_Index = Ref.Node.Parent.Child_Count then
                     Ref.Node := Ref.Node.Parent;
                  else
                     for I in Child_Index + 1 .. Ref.Node.Parent.Child_Count loop
                        if Ref.Node.Parent.Children (I).ID = Tree.Lexer.Descriptor.EOI_ID then
                           Ref := Invalid_Stream_Node_Ref;
                           return;
                        elsif Ref.Node.Parent.Children (I).Label = Nonterm then
                           Ref.Node := Ref.Node.Parent.Children (I);
                           return;
                        end if;
                     end loop;
                     Ref.Node := Ref.Node.Parent;
                  end if;
               end;
            end if;
         end loop;
      end Next_Sibling;

   begin
      case Ref.Node.Label is
      when Terminal_Label =>
         Next_Sibling;

      when Nonterm =>
         if Ref.Node.Child_Count > 0 then
            for N of Ref.Node.Children loop
               if N.ID = Tree.Lexer.Descriptor.EOI_ID then
                  Ref := Invalid_Stream_Node_Ref;
                  return;
               elsif N.Label = Nonterm then
                  Ref.Node := N;
                  return;
               end if;
            end loop;
         end if;
         Next_Sibling;
      end case;
   end Next_Nonterm;

   procedure Next_Parse_Stream (Tree : in Syntax_Trees.Tree; Stream : in out Stream_ID)
   is begin
      Parse_Stream_Lists.Next (Stream.Cur);
   end Next_Parse_Stream;

   procedure Next_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      loop
         Next_Terminal (Tree, Node, Parents);
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Next_Sequential_Terminal;

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Ref)
   is begin
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Node = Invalid_Node_Access;
         exit when Ref.Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Next_Sequential_Terminal;

   procedure Next_Sequential_Terminal
     (Tree      : in     Syntax_Trees.Tree;
      Ref       : in out Syntax_Trees.Stream_Node_Parents;
      Following : in     Boolean)
   is begin
      loop
         Next_Terminal (Tree, Ref, Following);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Next_Sequential_Terminal;

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      Result : Node_Access := Next_Terminal (Tree, Node);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         Next_Terminal (Tree, Result);
      end loop;
      return Result;
   end Next_Source_Terminal;

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean)
     return Stream_Node_Ref
   is begin
      return Result : Stream_Node_Ref := Next_Terminal (Tree, Ref) do
         loop
            exit when Result = Invalid_Stream_Node_Ref;
            exit when
              (if Trailing_Non_Grammar
               then (case Terminal_Label'(Result.Node.Label) is
                     when Source_Terminal => True,
                     when Virtual_Terminal | Virtual_Identifier =>
                        Result.Node.Non_Grammar.Length > 0)
               else Result.Node.Label = Source_Terminal);

            Next_Terminal (Tree, Result);
         end loop;
      end return;
   end Next_Source_Terminal;

   procedure Next_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Ref;
      Trailing_Non_Grammar : in     Boolean)
   is begin
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref = Invalid_Stream_Node_Ref;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Ref.Node.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Ref.Node.Non_Grammar.Length > 0)
            else Ref.Node.Label = Source_Terminal);
      end loop;
   end Next_Source_Terminal;

   procedure Next_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Trailing_Non_Grammar : in     Boolean)
   is begin
      loop
         Next_Terminal (Tree, Ref, Following => True);
         exit when Ref.Ref = Invalid_Stream_Node_Ref;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Ref.Ref.Node.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Ref.Ref.Node.Non_Grammar.Length > 0)
            else Ref.Ref.Node.Label = Source_Terminal);
      end loop;
   end Next_Source_Terminal;

   function Next_Stream_ID_Trimmed_Image (Tree : in Syntax_Trees.Tree) return String
   is begin
      return Trimmed_Image (Tree.Next_Stream_Label);
   end Next_Stream_ID_Trimmed_Image;

   procedure Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access)
   is begin
      Node := Next_Terminal (Tree, Node);
   end Next_Terminal;

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      pragma Unreferenced (Tree);

      function First_Child (Node : in Valid_Node_Access) return Node_Access
      is
      begin
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
            for J of Node.Children loop
               --  We tolerate deleted children here for edited trees.
               if J /= Invalid_Node_Access then
                  declare
                     Result : constant Node_Access := First_Child (J);
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     end if;
                  end;
               end if;
            end loop;
            --  All Children are empty
            return Invalid_Node_Access;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Access; Parent : in Node_Access) return Node_Access
      is begin
         --  Parent is parent of Child; return node immediately after Child.
         if Parent = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            case Parent.Label is
            when Source_Terminal =>
               --  Child is in Parent.Following_Deleted.
               return Next_Child (Parent, Parent.Parent);

            when Nonterm =>
               --  Normal tree node
               for I in Parent.Children'Range loop
                  if Parent.Children (I) = Child then
                     --  Use first non-empty next from I + 1.
                     for J in I + 1 .. Parent.Children'Last loop
                        --  We tolerate deleted children here for edited trees.
                        if Parent.Children (J) /= Invalid_Node_Access then
                           declare
                              Result : constant Node_Access := First_Child (Parent.Children (J));
                           begin
                              if Result /= Invalid_Node_Access then
                                 return Result;
                              end if;
                           end;
                        end if;
                     end loop;
                     --  All next Children are empty
                     return Next_Child (Parent, Parent.Parent);
                  end if;
               end loop;
               raise SAL.Programmer_Error; -- Child not found in Node.Children
            when others =>
               raise SAL.Programmer_Error;
            end case;
         end if;
      end Next_Child;
   begin
      return Next_Child (Node, Node.Parent);
   end Next_Terminal;

   procedure Next_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is
      pragma Unreferenced (Tree);

      function First_Child (Node : in Valid_Node_Access) return Node_Access
      is
      begin
         case Node.Label is
         when Terminal_Label =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
            Parents.Push (Node);
            for J of Node.Children loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               declare
                  Result : constant Node_Access := First_Child (J);
               begin
                  if Result /= Invalid_Node_Access then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            Parents.Pop;
            return Invalid_Node_Access;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Access; Parent : in Valid_Node_Access) return Node_Access
      is
         Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
      begin
         --  Parent is Parent of Child; return node immediately after Child.
         pragma Assert (Parent.Label = Nonterm);
         for I in Parent.Children'Range loop
            --  Encountering a deleted child here is an error in the user
            --  algorithm.
            if Parent.Children (I) = Child then
               --  Use first non-empty from I + 1.
               for J in I + 1 .. Parent.Children'Last loop
                  Parents.Push (Parent);
                  declare
                     Result : constant Node_Access := First_Child (Parent.Children (J));
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     else
                        Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to First_Child.
                     end if;
                  end;
               end loop;
               --  All next Children are empty (or there are none); move to
               --  next cousin.
               if Parents.Is_Empty then
                  return Invalid_Node_Access;
               else
                  return Next_Child (Parent, Parents.Pop);
               end if;
            end if;
         end loop;
         raise SAL.Programmer_Error; -- Child not found in Node.Children
      end Next_Child;
   begin
      if Parents.Is_Empty then
         Node := Invalid_Node_Access;
      else
         Node := Next_Child (Node, Parents.Pop);
      end if;
   end Next_Terminal;

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Element (Ref.Element.Cur).Node.Label in Terminal_Label or else
           Ref.Node = Invalid_Node_Access
           --  A previous Stream_Next arrived at an empty nonterm, or
           --  Next_Terminal reached the end of an element node.
         then
            Stream_Next (Tree, Ref, Rooted => False);
            exit when Ref.Element = Invalid_Stream_Index;

         else
            Ref.Node := Next_Terminal (Tree, Ref.Node);
         end if;

         exit when Ref.Node /= Invalid_Node_Access;
      end loop;
   end Next_Terminal;

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   is begin
      return Result : Terminal_Ref := Ref do
         Next_Terminal (Tree, Result);
      end return;
   end Next_Terminal;

   procedure Next_Terminal
     (Tree      : in     Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      Following : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Element (Ref.Ref.Element.Cur).Node.Label in Terminal_Label or else
           --  Can only be true on the first loop

           Ref.Ref.Node = Invalid_Node_Access
           --  The previous loop reached the end of an element node.

         then
            if Following then
               Stream_Next (Tree, Ref, Rooted => False);
            else
               Ref.Ref.Node := Invalid_Node_Access;
               Ref.Parents.Clear;
               exit;
            end if;

            exit when Ref.Ref.Element = Invalid_Stream_Index; -- end of stream

            exit when Ref.Ref.Node /= Invalid_Node_Access and then Ref.Ref.Node.Label in Terminal_Label;

         else
            Next_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
            exit when Ref.Ref.Node /= Invalid_Node_Access;
         end if;

      end loop;
   end Next_Terminal;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID
   is begin
      return Result : constant Stream_ID :=
        (Cur             => Tree.Streams.Append
           ((Label       => Tree.Next_Stream_Label,
             Stack_Top   => Invalid_Stream_Index.Cur,
             Shared_Link => Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True).Element.Cur,
             Elements    => <>)))
      do
         Tree.Next_Stream_Label := @ + 1;
      end return;
   end New_Stream;

   function New_Stream
     (Tree       : in out Syntax_Trees.Tree;
      Old_Stream : in     Stream_ID)
     return Stream_ID
   is begin
      if Old_Stream = Invalid_Stream_ID then
         return New_Stream (Tree);
      else
         declare
            Old_Parse_Stream : Parse_Stream renames Tree.Streams (Old_Stream.Cur);
            Old_Stack_Top    : constant Stream_Element_Lists.Cursor := Old_Parse_Stream.Stack_Top;

            Result_Cur : constant Parse_Stream_Lists.Cursor := Tree.Streams.Append
              ((Label       => Tree.Next_Stream_Label,
                Stack_Top   => Invalid_Stream_Index.Cur,
                Shared_Link => Old_Parse_Stream.Shared_Link,
                Elements    => <>));

            New_Stream : Parse_Stream renames Tree.Streams (Result_Cur);

            New_Cur : Stream_Element_Lists.Cursor;
            Old_Cur : Stream_Element_Lists.Cursor := Old_Parse_Stream.Elements.First;
            use Stream_Element_Lists;
         begin
            loop
               exit when not Has_Element (Old_Cur);
               declare
                  Old_Element : constant Stream_Element := Element (Old_Cur);
                  New_Node    : constant Node_Access    := Old_Element.Node;
                  --  We do not deep copy any nodes for the new stream; they are all
                  --  shared with other streams.
               begin
                  New_Cur := New_Stream.Elements.Append
                    ((Node  => New_Node,
                      State => Old_Element.State));
               end;

               if Old_Cur = Old_Stack_Top then
                  New_Stream.Stack_Top := New_Cur;
               end if;
               Next (Old_Cur);
            end loop;

            Tree.Next_Stream_Label := @ + 1;

            return (Cur => Result_Cur);
         end;
      end if;
   end New_Stream;

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result
   is
     --  Within one batch parsed subtree, positive and negative
     --  Node_Indices are separately unique. Positive Node_Index first, abs
     --  value for wisitoken_grammar_editing.translate_EBNF_to_BNF.
     (if Left.Node_Index > 0 and Right.Node_Index <= 0 then SAL.Less
      elsif Left.Node_Index <= 0 and Right.Node_Index > 0 then SAL.Greater
      elsif abs Left.Node_Index > abs Right.Node_Index then SAL.Greater
      elsif abs Left.Node_Index < abs Right.Node_Index then SAL.Less
      else SAL.Equal);

   function Node_ID
     (Tree   : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      return Item.Node.ID;
   end Node_ID;

   function Non_Grammar_Var
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in     Valid_Node_Access)
     return Token_Array_Var_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return
        (Element =>
           (case Terminal.Label is
            when Terminal_Label => Terminal.Non_Grammar'Access,
            when others         => raise SAL.Programmer_Error),
         Dummy => 0);
   end Non_Grammar_Var;

   function Non_Grammar_Const (Terminal : in Valid_Node_Access) return Token_Array_Const_Ref
   is begin
      return
        (Element =>
           (case Terminal.Label is
            when Terminal_Label => Terminal.Non_Grammar'Access,
            when others         => raise SAL.Programmer_Error),
         Dummy => 0);
   end Non_Grammar_Const;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Token_Array_Const_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return Non_Grammar_Const (Terminal);
   end Non_Grammar_Const;

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access
   is
      pragma Unreferenced (Tree);

      Result : Node_Access := Node;
      N      : Natural    := 0;
   begin
      loop
         Result := Result.Parent;
         N := N + 1;
         exit when N = Count or Result = Invalid_Node_Access;
      end loop;
      return Result;
   end Parent;

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Parents_Set;
   end Parents_Set;

   function Parents_Valid (Ref : in Stream_Node_Parents) return Boolean
   is begin
      return
        (Ref.Ref.Element = Invalid_Stream_Index and Ref.Ref.Node = Invalid_Node_Access) or else
        ((Stream_Element_Lists.Element (Ref.Ref.Element.Cur).Node = Ref.Ref.Node or
            Ref.Ref.Node = Invalid_Node_Access) and Ref.Parents.Is_Empty) or else
        (Ref.Parents.Depth > 0 and then
           (for all Item of Ref.Parents => Item /= Invalid_Node_Access and then Item.Label = Nonterm) and then
           (Ref.Parents.Peek (Ref.Parents.Depth) = Stream_Element_Lists.Element (Ref.Ref.Element.Cur).Node and
              --  we don't check the intervening parent items.
              (for some Child of Ref.Parents.Peek.Children => Child = Ref.Ref.Node)));
   end Parents_Valid;

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Streams.Length = 1;
   end Parseable;

   function Peek
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Count  : in SAL.Peek_Type := 1)
     return Stream_Index
   is
      use Stream_Element_Lists;

      Result : Cursor := Tree.Streams (Stream.Cur).Stack_Top;
   begin
      for I in 1 .. Count - 1 loop
         Result := Previous (@);
      end loop;
      return (Cur => Result);
   end Peek;

   function Pop (Parse_Stream : in out Syntax_Trees.Parse_Stream) return Valid_Node_Access
   is
      use Stream_Element_Lists;
      Temp : Cursor := Parse_Stream.Stack_Top;
   begin
      return Result : constant Valid_Node_Access := Element (Parse_Stream.Stack_Top).Node do
         Previous (Parse_Stream.Stack_Top);
         Parse_Stream.Elements.Delete (Temp);
         --  This does not change Parse_Stream.Shared_Link
      end return;
   end Pop;

   function Pop
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID)
     return Valid_Node_Access
   is begin
      return Pop (Tree.Streams (Stream.Cur));
   end Pop;

   function Prev_New_Line
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      Start_Line : in Base_Line_Number_Type := Invalid_Line_Number)
     return New_Line_Ref
   is
      Prev              : Node_Access           := Tree.Prev_Terminal (Node);
      Actual_Start_Line : Line_Number_Type;

      function SOI_New_Line_Ref return New_Line_Ref
      is begin
         return
           (Node              => Tree.SOI,
            Non_Grammar_Index => 1,
            First             => False,
            Pos               => Tree.SOI.Non_Grammar (1).Byte_Region.First,
            Line              => Tree.SOI.Non_Grammar (1).Line_Region.First);
      end SOI_New_Line_Ref;

      function Prev_New_Line_Ref (Index : in SAL.Peek_Type) return New_Line_Ref
      is begin
         return
           (Node              => Prev,
            Non_Grammar_Index => Index,
            First             => False,
            Pos               => Tree.Lexer.Contains_New_Line (Prev.Non_Grammar (Index).Byte_Region, First => False),
            Line              => Prev.Non_Grammar (Index).Line_Region.Last);
      end Prev_New_Line_Ref;

   begin
      if Start_Line = Invalid_Line_Number then
         declare
            New_Line_Count : Base_Line_Number_Type := 0;
         begin
            if Prev = Invalid_Node_Access then
               return SOI_New_Line_Ref;
            end if;

            loop
               exit when Prev.Non_Grammar.Length > 0;
               New_Line_Count := @ + Prev.New_Line_Count;
               Prev_Terminal (Tree, Prev);
            end loop;

            Actual_Start_Line := Prev.Non_Grammar (Prev.Non_Grammar.Last_Index).Line_Region.Last + New_Line_Count;
            Prev              := Tree.Prev_Terminal (Node);
         end;
      else
         Actual_Start_Line := Start_Line;
      end if;

      loop
         if Prev = Invalid_Node_Access then
            return SOI_New_Line_Ref;

         elsif Prev = Tree.SOI then
            for I in reverse Prev.Non_Grammar.First_Index + 1 .. Prev.Non_Grammar.Last_Index loop
               if New_Line_Count (Prev.Non_Grammar (I).Line_Region) > 0 then
                  return Prev_New_Line_Ref (I);
               end if;
            end loop;
            return SOI_New_Line_Ref;

         else
            case Terminal_Label'(Prev.Label) is
            when Virtual_Identifier | Virtual_Terminal =>
               for I in reverse Prev.Non_Grammar.First_Index .. Prev.Non_Grammar.Last_Index loop
                  --  ada_mode-interactive_01.adb
                  if Prev.Non_Grammar (I).ID = Tree.Lexer.Descriptor.New_Line_ID then
                     return Prev_New_Line_Ref (I);
                  end if;
               end loop;
               Prev_Terminal (Tree, Prev);

            when Source_Terminal =>
               if Prev.Non_Grammar.Length > 0 then
                  for I in reverse Prev.Non_Grammar.First_Index .. Prev.Non_Grammar.Last_Index loop
                     if New_Line_Count (Prev.Non_Grammar (I).Line_Region) > 0 then
                        return Prev_New_Line_Ref (I);
                     end if;
                  end loop;
                  Prev_Terminal (Tree, Prev);

               else
                  declare
                     Result : New_Line_Ref;
                  begin
                     Result.Pos := Tree.Lexer.Contains_New_Line (Prev.ID, Prev.Byte_Region, First => False);
                     if Result.Pos /= Invalid_Buffer_Pos then
                        Result.Node  := Prev;
                        Result.First := True;
                        Result.Line  := Actual_Start_Line;
                        return Result;

                     else
                        Prev_Terminal (Tree, Prev);
                     end if;
                  end;
               end if;
            end case;
         end if;
      end loop;
   end Prev_New_Line;

   function Prev_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      Result : Node_Access := Node;
   begin
      if Node = Tree.Root then
         return Tree.SOI;

      elsif Node.ID = Tree.Lexer.Descriptor.SOI_ID then
         return Node;
      end if;

      loop
         Result := Prev_Terminal (Tree, Result);
         exit when Result = Invalid_Node_Access;
         exit when Result.Non_Grammar.Length > 0;
      end loop;
      return Result;
   end Prev_Non_Grammar;

   procedure Prev_Non_Grammar
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is begin
      if Ref.Node /= Invalid_Node_Access and then Ref.Node.ID = Tree.Lexer.Descriptor.SOI_ID then
         return;
      end if;

      loop
         Prev_Terminal (Tree, Ref);
         exit when Ref.Node = Invalid_Node_Access;
         exit when Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Prev_Non_Grammar;

   procedure Prev_Non_Grammar
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   is begin
      if Ref.Ref.Node /= Invalid_Node_Access and then Ref.Ref.Node.ID = Tree.Lexer.Descriptor.SOI_ID then
         return;
      end if;
      loop
         Prev_Terminal (Tree, Ref, Parse_Stream, Preceding => True);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Prev_Non_Grammar;

   procedure Prev_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      loop
         Prev_Terminal (Tree, Node, Parents);
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Prev_Sequential_Terminal;

   procedure Prev_Sequential_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Syntax_Trees.Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Preceding    : in     Boolean)
   is begin
      loop
         Prev_Terminal (Tree, Ref, Parse_Stream, Preceding);
         exit when not Preceding and Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref = Invalid_Stream_Node_Ref;
         exit when Ref.Ref.Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Prev_Sequential_Terminal;

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is begin
      return Result : Node_Access := Node do
         loop
            Result := Prev_Terminal (Tree, Result);
            exit when Result = Invalid_Node_Access;
            exit when
              (if Trailing_Non_Grammar
               then (case Terminal_Label'(Result.Label) is
                     when Source_Terminal => True,
                     when Virtual_Terminal | Virtual_Identifier =>
                        Result.Non_Grammar.Length > 0)
               else Result.Label = Source_Terminal);
         end loop;
      end return;
   end Prev_Source_Terminal;

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean)
     return Stream_Node_Ref
   is begin
      return Result : Stream_Node_Ref := Ref do
         loop
            Prev_Terminal (Tree, Result);
            exit when Result = Invalid_Stream_Node_Ref;
            exit when
              (if Trailing_Non_Grammar
               then (case Terminal_Label'(Result.Node.Label) is
                     when Source_Terminal => True,
                     when Virtual_Terminal | Virtual_Identifier =>
                        Result.Node.Non_Grammar.Length > 0)
               else Result.Node.Label = Source_Terminal);
         end loop;
      end return;
   end Prev_Source_Terminal;

   procedure Prev_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Parse_Stream         : in     Stream_ID;
      Trailing_Non_Grammar : in     Boolean)
   is begin
      loop
         Prev_Terminal (Tree, Ref, Parse_Stream, Preceding => True);
         exit when Ref.Ref = Invalid_Stream_Node_Ref;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Ref.Ref.Node.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Ref.Ref.Node.Non_Grammar.Length > 0)
            else Ref.Ref.Node.Label = Source_Terminal);
      end loop;
   end Prev_Source_Terminal;

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      pragma Unreferenced (Tree);

      function Last_Child (Node : in Valid_Node_Access) return Node_Access
      is begin
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
            for J of reverse Node.Children loop
               --  We tolerate deleted children here for edited trees.
               if J /= Invalid_Node_Access then
                  declare
                     Result : constant Node_Access := Last_Child (J);
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     end if;
                  end;
               end if;
            end loop;
            --  All Children are empty
            return Invalid_Node_Access;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Access; Parent : in Node_Access) return Node_Access
      is begin
         --  Parent is Parent of Child; return terminal node immediately previous to Child.
         if Parent = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            case Parent.Label is
            when Source_Terminal =>
               --  Child is in Parent.Following_Deleted
               return Parent;

            when Nonterm =>
               --  Normal tree entry
               for I in reverse Parent.Children'Range loop
                  if Parent.Children (I) = Child then
                     --  Use first non-empty from I - 1.
                     for J in reverse Parent.Children'First .. I - 1 loop
                        --  We tolerate deleted children here for edited trees.
                        if Parent.Children (J) /= Invalid_Node_Access then
                           declare
                              Result : constant Node_Access := Last_Child (Parent.Children (J));
                           begin
                              if Result /= Invalid_Node_Access then
                                 return Result;
                              end if;
                           end;
                        end if;
                     end loop;
                     --  All previous Children are empty
                     return Prev_Child (Parent, Parent.Parent);
                  end if;
               end loop;
               raise SAL.Programmer_Error; -- Child not found in Parent.Children
            when others =>
               raise SAL.Programmer_Error;
            end case;
         end if;
      end Prev_Child;
   begin
      return Prev_Child (Node, Node.Parent);
   end Prev_Terminal;

   procedure Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access)
   is begin
      Node := Prev_Terminal (Tree, Node);
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is
      pragma Unreferenced (Tree);

      function Last_Child (Node : in Valid_Node_Access) return Node_Access
      is begin
         case Node.Label is
         when Terminal_Label =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
            Parents.Push (Node);
            for J of reverse Node.Children loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               declare
                  Result : constant Node_Access := Last_Child (J);
               begin
                  if Result /= Invalid_Node_Access then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            Parents.Pop;
            return Invalid_Node_Access;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Access; Parent : in Valid_Node_Access) return Node_Access
      is
         Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
      begin
         --  Parent is parent of Child; return node immediately previous to Child.
         pragma Assert (Parent.Label = Nonterm);
         for I in reverse Parent.Children'Range loop
            --  Encountering a deleted child here is an error in the user
            --  algorithm.
            if Parent.Children (I) = Child then
               --  Use first non-empty from I - 1.
               for J in reverse Parent.Children'First .. I - 1 loop
                  Parents.Push (Parent);
                  declare
                     Result : constant Node_Access := Last_Child (Parent.Children (J));
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     else
                        Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to Last_Child.
                     end if;
                  end;
               end loop;

               --  All previous Parent.Children are empty (or there are none); move to
               --  prev cousin.
               if Parents.Is_Empty then
                  return Invalid_Node_Access;
               else
                  return Prev_Child (Parent, Parents.Pop);
               end if;
            end if;
         end loop;
         raise SAL.Programmer_Error; -- Child not found in Parent.Children
      end Prev_Child;
   begin
      if Parents.Is_Empty then
         Node := Invalid_Node_Access;

      else
         Node := Prev_Child (Node, Parents.Pop);
      end if;
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Element (Ref.Element.Cur).Node.Label in Terminal_Label or else
           Ref.Node = Invalid_Node_Access
           --  A previous Prev_Element arrived at an empty nonterm, or
           --  Prev_Terminal reached the beginning of an element node.
         then
            Stream_Prev (Tree, Ref, Rooted => False);
            exit when Ref.Element = Invalid_Stream_Index;

         else
            Ref.Node := Prev_Terminal (Tree, Ref.Node);
         end if;
         exit when Ref.Node /= Invalid_Node_Access;
      end loop;
   end Prev_Terminal;

   function Prev_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   is begin
      return Result : Terminal_Ref := Ref do
         Prev_Terminal (Tree, Result);
      end return;
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID;
      Preceding    : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements
         if Element (Ref.Ref.Element.Cur).Node.Label in Terminal_Label or else
           Ref.Ref.Node = Invalid_Node_Access
           --  Ref is at a terminal element or an empty nonterm, or was at a
           --  first terminal; move to previous stream element.
         then
            if not Preceding then
               Ref.Ref.Node := Invalid_Node_Access;
               Ref.Parents.Clear;
               exit;
            end if;
            if Parse_Stream /= Invalid_Stream_ID and Ref.Ref.Stream = Tree.Shared_Stream then
               declare
                  P_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Parse_Stream.Cur);
               begin
                  if Ref.Ref.Element.Cur = P_Stream.Shared_Link then
                     Ref :=
                       (Ref     =>
                          (Parse_Stream,
                           (Cur => P_Stream.Elements.Last),
                           Element (P_Stream.Elements.Last).Node),
                        Parents => <>);
                     Tree.Last_Terminal (Ref, Parse_Stream, Preceding => False);
                  else
                     Stream_Prev (Tree, Ref, Rooted => False);
                  end if;
               end;
            else
               Stream_Prev (Tree, Ref, Rooted => False);
            end if;

            exit when Ref.Ref.Element = Invalid_Stream_Index;
         else
            Prev_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
         end if;
         exit when Ref.Ref.Node /= Invalid_Node_Access;
      end loop;
   end Prev_Terminal;

   procedure Print_Ref_Counts (Tree : in Syntax_Trees.Tree)
   is begin
      for Stream of Tree.Streams loop
         Tree.Lexer.Trace.Put (Trimmed_Image (Stream.Label) & ":");
         declare
            use Stream_Element_Lists;
            Cur : Cursor := Stream.Elements.First;
         begin
            loop
               exit when Cur = No_Element;
               Tree.Lexer.Trace.Put (Integer'Image (Ref_Count (Cur) - 1));
               Next (Cur);
            end loop;
         end;
         Tree.Lexer.Trace.New_Line;
      end loop;
   end Print_Ref_Counts;

   procedure Print_Streams
     (Tree        : in     Syntax_Trees.Tree;
      Children    : in     Boolean := False;
      Non_Grammar : in     Boolean := False)
   is begin
      for Stream of Tree.Streams loop
         Tree.Lexer.Trace.Put_Line
           (Tree.Image
              (Stream, Shared => True, Children => Children, Node_Numbers => True, Non_Grammar => Non_Grammar));
         Tree.Lexer.Trace.New_Line;
      end loop;
   end Print_Streams;

   procedure Print_Tree
     (Tree         : in Syntax_Trees.Tree;
      Root         : in Node_Access := Invalid_Node_Access;
      Line_Numbers : in Boolean     := False;
      Non_Grammar  : in Boolean     := False)
   is
      procedure Print_Node (Node : in Valid_Node_Access; Level : in Integer)
      is begin
         for I in 1 .. Level loop
            Tree.Lexer.Trace.Put ("| ", Prefix => False);
         end loop;
         Tree.Lexer.Trace.Put (Image (Tree, Node, Children => False, RHS_Index => True, Node_Numbers => True,
                           Line_Numbers => Line_Numbers, Non_Grammar => Non_Grammar),
                    Prefix => False);

         if Node.Augmented /= null then
            Tree.Lexer.Trace.Put (Image_Augmented (Node.Augmented.all), Prefix => False);
         end if;

         Tree.Lexer.Trace.New_Line;
         if Node.Label = Nonterm then
            for Child of Node.Children loop
               if Child = null then
                  Tree.Lexer.Trace.Put ("    : ", Prefix => True);
                  for I in 1 .. Level + 1 loop
                     Tree.Lexer.Trace.Put ("| ", Prefix => False);
                  end loop;
                  Tree.Lexer.Trace.Put ("<null>", Prefix => False);
                  Tree.Lexer.Trace.New_Line;
               else
                  Print_Node (Child, Level + 1);
               end if;
            end loop;
         end if;
      end Print_Node;

      Print_Root : constant Node_Access := (if Root = Invalid_Node_Access then Syntax_Trees.Root (Tree) else Root);

      Print_SOI_EOI : constant Boolean := Root = Invalid_Node_Access and Print_Root /= Tree.Root and
        Tree.Streams.Length > 0;
   begin
      if Print_Root = Invalid_Node_Access then
         Tree.Lexer.Trace.Put_Line ("<empty tree>");
      else
         if Print_SOI_EOI then
            declare
               --  Get SOI, EOI from same stream as Print_Root
               Stream : Parse_Stream renames Tree.Streams (Tree.Streams.Last);
            begin
               Print_Node (Stream_Element_Lists.Element (Stream.Elements.First).Node, 0);
            end;
         end if;
         Print_Node (Print_Root, 0);
         if Print_SOI_EOI then
            declare
               --  Get SOI, EOI from same stream as Print_Root
               Stream : Parse_Stream renames Tree.Streams (Tree.Streams.Last);
            begin
               Print_Node (Stream_Element_Lists.Element (Stream.Elements.Last).Node, 0);
            end;
         end if;
      end if;
   end Print_Tree;

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Access;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Access)
        return Boolean)
     return Boolean
   is
   begin
      if Visit_Parent = Before then
         if not Process_Node (Tree, Node) then
            return False;
         end if;
      end if;

      if Node.Label = Nonterm then
         for Child of Node.Children loop
            if Child /= null then
               if not Process_Tree (Tree, Child, Visit_Parent, Process_Node) then
                  return False;
               end if;
            end if;
         end loop;
      end if;

      if Visit_Parent = After then
         return Process_Node (Tree, Node);
      else
         return True;
      end if;
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Node         : in     Valid_Node_Access;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access))
   is begin
      if Node.Label = Nonterm then
         for Child of Node.Children loop
            if Child /= null then
               Process_Tree (Tree, Child, Process_Node);
            end if;
         end loop;
      end if;

      Process_Node (Tree, Node);
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access);
      Root         : in     Node_Access := Invalid_Node_Access)
   is begin
      Tree.Traversing := True;
      Process_Tree (Tree, (if Root = Invalid_Node_Access then Syntax_Trees.Root (Tree) else Root), Process_Node);
      Tree.Traversing := False;
   exception
   when others =>
      Tree.Traversing := False;
      raise;
   end Process_Tree;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Production_ID
   is
      pragma Unreferenced (Tree);
   begin
      return (Node.ID, Node.RHS_Index);
   end Production_ID;

   function Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Stream_ID    : in     Syntax_Trees.Stream_ID;
      Node         : in     Valid_Node_Access;
      State        : in     State_Index)
     return Rooted_Ref
   is
      use Stream_Element_Lists;
      New_Element : constant Cursor := Parse_Stream.Elements.Insert
        (Element  => (Node, State),
         Before   => Next (Parse_Stream.Stack_Top));
   begin
      Parse_Stream.Stack_Top := New_Element;
      --  caller must change Parse_Stream.Shared_Link if needed.
      return (Stream_ID, (Cur => New_Element), Node);
   end Push;

   procedure Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Stream_ID    : in     Syntax_Trees.Stream_ID;
      Node         : in     Valid_Node_Access;
      State        : in     State_Index)
   is
      Junk : Stream_Node_Ref := Push (Parse_Stream, Stream_ID, Node, State);
      pragma Unreferenced (Junk);
   begin
      null;
   end Push;

   procedure Push
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     State_Index)
   is begin
      Push (Tree.Streams (Stream.Cur), Stream, Node, State);
   end Push;

   procedure Push_Back
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Parse_Stream.Stack_Top := Stream_Element_Lists.Previous (Parse_Stream.Stack_Top);
      --  This does not change Parse_Stream.Shared_Link
   end Push_Back;

   procedure Put_Tree
     (Tree      : in Syntax_Trees.Tree;
      File_Name : in String)
   is
      --  The format uses parens and spaces; no commas. One node per line,
      --  then one stream per line.
      use Ada.Streams.Stream_IO;
      File   : File_Type;
      Stream : Stream_Access;

      --  SAL.SAL.Gen_Unbounded_Definite_Vectors uses SAL.Peek_Type
      --  internally to index the vector; that has a range 1 ..
      --  Integer'Last, smaller than Node_Index'Range. So we must use a
      --  smaller type, and hope the actual tree does not have that many
      --  nodes.
      subtype Half_Node_Index is Node_Index range Node_Index'First / 2 .. Node_Index'Last / 2;
      package Node_Index_Sets is new SAL.Gen_Unbounded_Definite_Vectors
        (Half_Node_Index, Boolean, Default_Element => False);
      Node_Index_Seen : Node_Index_Sets.Vector;

      procedure Seen (I : in Node_Index)
      is begin
         if Node_Index_Seen.Is_Empty then
            Node_Index_Seen.Set_First_Last (I, I);
         elsif I < Node_Index_Seen.First_Index then
            Node_Index_Seen.Set_First_Last (I, Node_Index_Seen.Last_Index);

         elsif I > Node_Index_Seen.Last_Index then
            Node_Index_Seen.Set_First_Last (Node_Index_Seen.First_Index, I);

         else
            if Node_Index_Seen (I) then
               raise SAL.Programmer_Error with "duplicate node_index; use Copy_Tree first";
            end if;
         end if;
         Node_Index_Seen (I) := True;
      end Seen;

      procedure Put_Error_List (Error_List : in Error_List_Access)
      is begin
         if Error_List = null then
            String'Write (Stream, " 0()");
         else
            String'Write (Stream, Error_List.Length'Image & '(');
            for Error of Error_List.all loop
               Error_Data'Class'Output (Stream, Error);
            end loop;
            Character'Write (Stream, ')');
         end if;
      end Put_Error_List;

      function Buffer_Region_Image (Region : in Buffer_Region) return String
      is begin
         return "(" & Trimmed_Image (Region.First) & Region.Last'Image & ")";
      end Buffer_Region_Image;

      function Line_Region_Image (Region : in WisiToken.Line_Region) return String
      is begin
         return "(" & Trimmed_Image (Region.First) & Region.Last'Image & ")";
      end Line_Region_Image;

      function Token_Image (Token : in WisiToken.Lexer.Token) return String
      is begin
         return "(" & Trimmed_Image (Token.ID) &
           Buffer_Region_Image (Token.Byte_Region) &
           Buffer_Region_Image (Token.Char_Region) &
           Line_Region_Image (Token.Line_Region) & ")";
      end Token_Image;

      function Token_Array_Image is new WisiToken.Lexer.Token_Arrays.Gen_Image (Token_Image);

      procedure Put_Node (Node : in Valid_Node_Access)
      is begin
         String'Write
           (Stream,
            "(" & Node.Label'Image &
              Node.Child_Count'Image &
              Node.ID'Image & " " &
              Node.Node_Index'Image);

         --  We don't output Node.Parent; redundant with node.Children.

         if Node.Augmented /= null then
            raise SAL.Not_Implemented with "put_tree augmented";
         end if;

         Put_Error_List (Node.Error_List);

         case Node.Label is
         when Terminal_Label =>

            String'Write (Stream, Node.Non_Grammar.Length'Image & Token_Array_Image (Node.Non_Grammar));
            String'Write (Stream, Node.Sequential_Index'Image);

            case Terminal_Label'(Node.Label) is
            when Source_Terminal =>
               String'Write
                 (Stream,
                  Buffer_Region_Image (Node.Byte_Region) & Buffer_Region_Image (Node.Char_Region) &
                    Node.New_Line_Count'Image);
               String'Write (Stream, Node.Following_Deleted.Length'Image & "(");
               for Del of Node.Following_Deleted loop
                  String'Write (Stream, Del.Node_Index'Image);
               end loop;
               String'Write (Stream, ")");

            when Virtual_Terminal =>
               String'Write (Stream, " " & Node.Insert_Location'Image);

            when Virtual_Identifier =>
               raise SAL.Not_Implemented with "put_tree virtual_identifier";
            end case;

         when Nonterm =>
            String'Write
              (Stream, " " & Node.Virtual'Image & " " & Node.Recover_Conflict'Image & Node.RHS_Index'Image &
                 Node.Name_Offset'Image & Node.Name_Length'Image);
            String'Write (Stream, "(");
            for Child of Node.Children loop
               if Child = Invalid_Node_Access then
                  String'Write (Stream, Invalid_Node_Index'Image);
               else
                  String'Write (Stream, " " & Child.Node_Index'Image);
               end if;
            end loop;
            String'Write (Stream, ")");
         end case;
         String'Write (Stream, ")" & ASCII.LF);
      end Put_Node;

   begin
      Create (File, Out_File, File_Name);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      String'Write (Stream, Tree.Nodes.Last_Index'Image & ASCII.LF);
      for Node of Tree.Nodes loop
         Seen (Node.Node_Index);
         Put_Node (Node);
      end loop;

      String'Write (Stream, Tree.Streams.Length'Image & ASCII.LF);
      for Stream of Tree.Streams loop
         raise SAL.Not_Implemented with "put_tree stream";
      end loop;
      if Tree.Root /= Invalid_Node_Access then
         String'Write (Stream, Tree.Root.Node_Index'Image);
      end if;
      Close (File);
   end Put_Tree;

   function Recover_Conflict (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Recover_Conflict;
   end Recover_Conflict;

   function Reduce
     (Tree             : in out Syntax_Trees.Tree;
      Stream           : in     Stream_ID;
      Production       : in     WisiToken.Production_ID;
      Child_Count      : in     Ada.Containers.Count_Type;
      State            : in     State_Index;
      Recover_Conflict : in     Boolean)
     return Rooted_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      function Pop_Children return Valid_Node_Access_Array
      is begin
         return Result : Valid_Node_Access_Array (1 .. SAL.Base_Peek_Type (Child_Count)) := (others => Dummy_Node) do
            --  IMPROVEME: use iterated_component_association to avoid bogus init. Waiting on compiler support.
            for I in reverse Result'Range loop
               Result (I) := Pop (Parse_Stream);
            end loop;
         end return;
      end Pop_Children;

      New_Node : constant Node_Access := Tree.Add_Nonterm_1
        (Production, Pop_Children, Clear_Parents => False, Recover_Conflict => Recover_Conflict);
   begin
      return Push (Parse_Stream, Stream, New_Node, State);
   end Reduce;

   procedure Replace_Child
     (Tree                 : in out Syntax_Trees.Tree;
      Parent               : in     Valid_Node_Access;
      Child_Index          : in     SAL.Peek_Type;
      Old_Child            : in     Node_Access;
      New_Child            : in     Node_Access;
      Old_Child_New_Parent : in     Node_Access := Invalid_Node_Access)
   is
      pragma Unreferenced (Tree);
   begin
      Parent.Children (Child_Index) := New_Child;

      if Old_Child /= null then
         Old_Child.Parent := Old_Child_New_Parent;
      end if;

      New_Child.Parent := Parent;
   end Replace_Child;

   procedure Replace_Node (Element : in Stream_Index; New_Node : in Valid_Node_Access)
   is
      Orig : Stream_Element := Stream_Element_Lists.Element (Element.Cur);
   begin
      Orig.Node := New_Node;
      Stream_Element_Lists.Replace_Element (Element.Cur, Orig);
   end Replace_Node;

   procedure Replace_State (Cur : in Stream_Element_Lists.Cursor; New_State : in State_Index)
   is
      Orig : Stream_Element := Stream_Element_Lists.Element (Cur);
   begin
      Orig.State := New_State;
      Stream_Element_Lists.Replace_Element (Cur, Orig);
   end Replace_State;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   is
      pragma Unreferenced (Tree);
   begin
      return Node.RHS_Index;
   end RHS_Index;

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      if Tree.Root = Invalid_Node_Access then
         if Tree.Streams.Length = 0 then
            return Invalid_Node_Access;
         else
            declare
               use Stream_Element_Lists;
               Stream : Parse_Stream renames Tree.Streams (Tree.Streams.Last);
               --  parse stream from Parse or shared_stream from Edit_Tree with no changes

               Cur : Cursor := Stream.Elements.First; -- SOI
            begin
               if Has_Element (Cur) then
                  Cur := Next (Cur); -- wisitoken_accept
                  if Has_Element (Cur) then
                     return Element (Cur).Node;
                  end if;
               end if;
               return Invalid_Node_Access;
            end;
         end if;
      else
         return Tree.Root;
      end if;
   end Root;

   function Rooted (Ref : in Stream_Node_Ref) return Boolean
   is begin
      return Stream_Element_Lists.Has_Element (Ref.Element.Cur) and then
        Stream_Element_Lists.Element (Ref.Element.Cur).Node = Ref.Node;
   end Rooted;

   procedure Set_Augmented
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Value : in Augmented_Class_Access)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Augmented := Value;
   end Set_Augmented;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in out Valid_Node_Access;
      Children : in     Node_Access_Array)
   is begin
      --  See Design note in spec about Parents, Parent_Set.

      --  Clear current Children.Parent first, in case some are also in new
      --  children.
      for C of Parent.Children loop
         if C /= null then
            C.Parent := Invalid_Node_Access;
         end if;
      end loop;

      if Parent.Children'Length = Children'Length then
         --  reuse current node
         Parent.Virtual  := False;
         Parent.Children := Children;

      else
         --  reallocate node with new child_count
         declare
            Realloc_Parent : constant Node_Access := new Node'
              (Label       => Nonterm,
               Copied_Node => Invalid_Node_Access,
               Child_Count => Children'Last,
               ID          => Parent.ID,
               Node_Index  => -(Tree.Nodes.Last_Index + 1),
               Parent      => Parent.Parent,
               Augmented   => Parent.Augmented,
               Error_List  =>
                 (if Parent.Error_List = null
                  then null
                  else new Error_Data_Lists.List'(Parent.Error_List.all)),
               Virtual          => False,
               Recover_Conflict => Parent.Recover_Conflict,
               RHS_Index        => Parent.RHS_Index,
               Name_Offset      => Parent.Name_Offset,
               Name_Length      => Parent.Name_Length,
               Children         => Children);
         begin
            Tree.Nodes.Append (Realloc_Parent);

            if Parent.Parent /= null then
               Parent.Parent.Children (Child_Index (Parent.Parent.all, Parent)) := Realloc_Parent;
            end if;

            Parent := Realloc_Parent;
         end;
      end if;

      for Child of Children loop
         if Child.Parent /= Invalid_Node_Access then
            declare
               Other_Parent : constant Node_Access := Child.Parent;
               Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                 (Other_Parent.all, Child);
            begin
               Other_Parent.Children (Child_Index) := null;
            end;
         end if;

         Child.Parent := Parent;
      end loop;
   end Set_Children;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in out Valid_Node_Access;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Node_Access_Array)
   is
   begin
      Node.ID        := New_ID.LHS;
      Node.RHS_Index := New_ID.RHS;

      Set_Children (Tree, Node, Children);
   end Set_Children;

   procedure Set_Insert_Location
     (Tree            : in Syntax_Trees.Tree;
      Node            : in Valid_Node_Access;
      Insert_Location : in WisiToken.Insert_Location)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Insert_Location := Insert_Location;
   end Set_Insert_Location;

   procedure Set_Name
     (Tree : in     Syntax_Trees.Tree;
      Item : in out Recover_Token;
      Name : in     Buffer_Region)
   is begin
      if Item.Virtual then
         Item.Name := Name;
      else
         Tree.Set_Name (Item.Element_Node, Name);
      end if;
   end Set_Name;

   procedure Set_Name
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Region : in Buffer_Region)
   is
      First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
      Byte_First     : constant Buffer_Pos  := Tree.Byte_Region (First_Terminal, Trailing_Non_Grammar => False).First;
   begin
      if Region = Null_Buffer_Region or else -- Not a valid name
        (First_Terminal.Label = Virtual_Terminal and not Tree.Parents_Set) --  Can't trust Byte_First
      then
         null;
      else
         Node.Name_Offset := Region.First - Byte_First;
         Node.Name_Length := Region.Last - Region.First + 1;
      end if;
   end Set_Name;

   procedure Set_Node_Index
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      Node_Index : in Syntax_Trees.Node_Index)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Node_Index := Node_Index;
   end Set_Node_Index;

   procedure Set_Parents
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID := Invalid_Stream_ID)
   is
      procedure Set_Parents
        (Tree   : in out Syntax_Trees.Tree;
         Node   : in     Valid_Node_Access;
         Parent : in     Node_Access)
      is
      begin
         Node.Parent := Parent;
         case Node.Label is
         when Source_Terminal =>
            for N of Node.Following_Deleted loop
               N.Parent := Node;
            end loop;

         when Virtual_Terminal | Virtual_Identifier =>
            null;

         when Nonterm =>
            for C of Node.Children loop
               if C = null then
                  --  This can only happen if someone calls Set_Parents after parents
                  --  are already set and the tree is edited.
                  raise SAL.Programmer_Error with "encountered deleted child";
               end if;
               Set_Parents (Tree, C, Node);
            end loop;
         end case;
      end Set_Parents;
   begin
      --  IMPROVEME incremental: only need to handle fully parsed tree, no
      --  streams. Use incremental algorithm; if find a set parent link,
      --  assume subtree under that node has parent links set? requires all
      --  "node.parent = null" to do all ancestors as well.
      if Stream = Invalid_Stream_ID then
         if Tree.Streams.Length = 0 then
            if Tree.Root = Invalid_Node_Access then
               raise SAL.Parameter_Error with "invalid_tree: no streams, Tree.Root not set";
            else
               Set_Parents (Tree, Tree.Root, Invalid_Node_Access);
            end if;
         else
            declare
               use Stream_Element_Lists;
               Cur : Cursor := Tree.Streams (Tree.Shared_Stream.Cur).Elements.First;
            begin
               loop
                  exit when Cur = No_Element;
                  Set_Parents (Tree, Element (Cur).Node, Invalid_Node_Access);
                  Next (Cur);
               end loop;
            end;
         end if;
      else
         declare
            use Stream_Element_Lists;
            Cur : Cursor := Tree.Streams (Stream.Cur).Elements.First;
         begin
            loop
               exit when Cur = No_Element;
               Set_Parents (Tree, Element (Cur).Node, Invalid_Node_Access);
               Next (Cur);
            end loop;
         end;
      end if;
      Tree.Parents_Set := True;
   end Set_Parents;

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access)
   is begin
      if New_Root.Children (1).ID = Tree.Lexer.Descriptor.SOI_ID and
        New_Root.Children (New_Root.Children'Last).ID = Tree.Lexer.Descriptor.EOI_ID
      then
         Tree.Root := New_Root;
      else
         declare
            function Create_New_Children return Node_Access_Array
            is
               Last : Positive_Index_Type := New_Root.Children'Last;
               New_Children : Node_Access_Array (1 .. New_Root.Children'Last + 2);
            begin
               if New_Root.Children (1) /= Tree.SOI then
                  New_Children (1) := Tree.SOI;
                  Last := 1 + New_Root.Children'Length;
                  New_Children (2 .. Last) := New_Root.Children;
               end if;

               if New_Root.Children (New_Root.Children'Last) /= Tree.EOI then
                  Last := @ + 1;
                  New_Children (Last) := Tree.EOI;
               end if;
               return New_Children (1 .. Last);
            end Create_New_Children;

            New_Children : constant Node_Access_Array := Create_New_Children;
         begin
            Tree.Root := new Node'
              (Label       => Nonterm,
               Copied_Node => Invalid_Node_Access,
               Child_Count => New_Children'Last,
               ID          => New_Root.ID,
               Node_Index  => New_Root.Node_Index,
               Parent      => null,
               Augmented   => New_Root.Augmented,
               Error_List  =>
                 (if New_Root.Error_List = null
                  then null
                  else new Error_Data_Lists.List'(New_Root.Error_List.all)),
               Virtual          => New_Root.Virtual,
               Recover_Conflict => New_Root.Recover_Conflict,
               RHS_Index        => New_Root.RHS_Index,
               Name_Offset      => New_Root.Name_Offset,
               Name_Length      => New_Root.Name_Length,
               Children         => New_Children);

            for Child of New_Children loop
               Child.Parent := Tree.Root;
            end loop;

            Tree.Nodes.Append (Tree.Root);
         end;
      end if;
   end Set_Root;

   procedure Set_Sequential_Index
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Index : in Base_Sequential_Index)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Sequential_Index := Index;
   end Set_Sequential_Index;

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is begin
      return Tree.Shared_Stream;
   end Shared_Stream;

   function Shared_Token
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Stream_Element_Lists.Has_Element (Parse_Stream.Shared_Link) then
         return
           (Stream  => Tree.Shared_Stream,
            Element => (Cur => Parse_Stream.Shared_Link),
            Node    => Stream_Element_Lists.Element (Parse_Stream.Shared_Link).Node);
      else
         --  Shared_Link was EOI, then EOI was copied to parse stream to add an
         --  error. test_incremental.adb Preserve_parse_Errors_1.
         return Invalid_Stream_Node_Ref;
      end if;
   end Shared_Token;

   procedure Shift
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Parse_Stream.Elements.Last then
         --  Current input token is Stream.Shared_Link.
         Push (Parse_Stream, Stream, Stream_Element_Lists.Element (Parse_Stream.Shared_Link).Node, State);
         Stream_Element_Lists.Next (Parse_Stream.Shared_Link);
      else
         --  Current input token is Stream input.
         Stream_Element_Lists.Next (Parse_Stream.Stack_Top);
         Replace_State (Parse_Stream.Stack_Top, State);
      end if;
   end Shift;

   procedure Shift
     (Tree             : in     Syntax_Trees.Tree;
      Node             : in     Valid_Node_Access;
      Shift_Bytes      : in     Base_Buffer_Pos;
      Shift_Chars      : in     Base_Buffer_Pos;
      Shift_Lines      : in     Base_Line_Number_Type;
      Last_Stable_Byte : in     Base_Buffer_Pos;
      Non_Grammar_Next : in out Lexer.Token_Arrays.Extended_Index)
   is begin
      case Terminal_Label'(Node.Label) is
      when Source_Terminal =>
         pragma Assert (if Node.ID = Tree.Lexer.Descriptor.SOI_ID then Shift_Bytes = 0 and Shift_Chars = 0);
         if Node.Byte_Region /= Null_Buffer_Region then
            Node.Byte_Region := @ + Shift_Bytes;
         end if;
         if Node.Char_Region /= Null_Buffer_Region then
            Node.Char_Region := @ + Shift_Chars;
         end if;
      when Virtual_Terminal | Virtual_Identifier =>
         null;
      end case;

      for I in Node.Non_Grammar.First_Index .. Node.Non_Grammar.Last_Index loop
         declare
            Token : Lexer.Token renames Node.Non_Grammar (I);
         begin
            if Token.ID = Tree.Lexer.Descriptor.SOI_ID then
               null;

            elsif Token.Byte_Region.Last < Last_Stable_Byte then
               Token.Byte_Region := @ + Shift_Bytes;
               Token.Char_Region := @ + Shift_Chars;
               Token.Line_Region := @ + Shift_Lines;

            else
               Non_Grammar_Next := I;
               exit;
            end if;
         end;
      end loop;

      if Node.Augmented /= null then
         Shift (Node.Augmented.all, Shift_Bytes, Shift_Chars, Shift_Lines, Last_Stable_Byte);
      end if;
   end Shift;

   function Single_Terminal (Ref : in Stream_Node_Ref) return Boolean
   is begin
      return Stream_Element_Lists.Element (Ref.Element.Cur).Node = Ref.Node and Ref.Node.Label in Terminal_Label;
   end Single_Terminal;

   function SOI (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      return Tree.SOI;
   end SOI;

   function Stack_Depth (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Element : Cursor             := Parse_Stream.Stack_Top;
      Result  : SAL.Base_Peek_Type := 0;
   begin
      loop
         exit when not Has_Element (Element);
         Result := @ + 1;
         Element := Previous (Element);
      end loop;
      return Result;
   end Stack_Depth;

   function Stack_Top
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   is begin
      return (Cur => Tree.Streams (Stream.Cur).Stack_Top);
   end Stack_Top;

   procedure Start_Edit (Tree : in out Syntax_Trees.Tree)
   is
      use Stream_Element_Lists;
   begin
      Tree.Shared_Stream :=
        (Cur             => Tree.Streams.Append
           ((Label       => Shared_Stream_Label,
             Stack_Top   => Invalid_Stream_Index.Cur,
             Shared_Link => Invalid_Stream_Index.Cur,
             Elements    => <>)));

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.SOI,
          State => Unknown_State));

      Tree.SOI.Parent := Invalid_Node_Access;

      --  Delete SOI, EOI from root children (added in Clear_Parse_Streams)
      declare
         New_Children : Node_Access_Array (1 .. Tree.Root.Child_Count - 2);
      begin
         New_Children := Tree.Root.Children (2 .. Tree.Root.Children'Last - 1);

         Tree.Root := new Node'
           (Label       => Nonterm,
            Copied_Node => Invalid_Node_Access,
            Child_Count => Tree.Root.Child_Count - 2,
            ID          => Tree.Root.ID,
            Node_Index  => Tree.Root.Node_Index,
            Parent      => null,
            Augmented   => Tree.Root.Augmented,
            Error_List  =>
              (if Tree.Root.Error_List = null
               then null
               else new Error_Data_Lists.List'(Tree.Root.Error_List.all)),
            Virtual          => Tree.Root.Virtual,
            Recover_Conflict => Tree.Root.Recover_Conflict,
            RHS_Index        => Tree.Root.RHS_Index,
            Name_Offset      => Tree.Root.Name_Offset,
            Name_Length      => Tree.Root.Name_Length,
            Children         => New_Children);

         for Child of New_Children loop
            Child.Parent := Tree.Root;
         end loop;

         Tree.Nodes.Append (Tree.Root);
      end;

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.Root,
          State => Unknown_State));

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.EOI,
          State => Unknown_State));

      Tree.EOI.Parent := Invalid_Node_Access;

      Tree.Root := Invalid_Node_Access;
   end Start_Edit;

   procedure Start_Lex
     (Tree           : in out Syntax_Trees.Tree)
   is
      Begin_Byte_Pos : Buffer_Pos;
      Begin_Char_Pos : Buffer_Pos;
      Begin_Line     : Line_Number_Type;
   begin
      Tree.Lexer.Begin_Pos (Begin_Byte_Pos, Begin_Char_Pos, Begin_Line);
      declare
         Token : constant Lexer.Token :=
           (ID          => Tree.Lexer.Descriptor.SOI_ID,
            Byte_Region => (Begin_Byte_Pos, Begin_Byte_Pos),
            Char_Region => (Begin_Char_Pos, Begin_Char_Pos),
            Line_Region => (Begin_Line, Begin_Line));
         --  Tree.*_Region uses SOI.*_Region.First as first * in text,
         --  .Last as first * in next token.

      begin
         if Tree.SOI = null then
            Tree.SOI := new Node'
              (Label       => Source_Terminal,
               Child_Count => 0,
               others => <>);
            Tree.Nodes.Append (Tree.SOI);
         end if;

         Tree.SOI.all :=
           (Label             => Source_Terminal,
            Copied_Node       => Invalid_Node_Access,
            Child_Count       => 0,
            ID                => Tree.Lexer.Descriptor.SOI_ID,
            Node_Index        => 0,
            Parent            => Invalid_Node_Access,
            Augmented         => null,
            Error_List        => null,
            Non_Grammar       => Lexer.Token_Arrays.To_Vector (Token),
            Sequential_Index  => Invalid_Sequential_Index,
            Byte_Region       => Token.Byte_Region,
            Char_Region       => Token.Char_Region,
            New_Line_Count    => New_Line_Count (Token.Line_Region),
            Following_Deleted => Valid_Node_Access_Lists.Empty_List);

         Tree.Shared_Stream :=
           (Cur             => Tree.Streams.Append
              ((Label       => Shared_Stream_Label,
                Stack_Top   => Stream_Element_Lists.No_Element,
                Shared_Link => Stream_Element_Lists.No_Element,
                Elements    => <>)));

         Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
           ((Node => Tree.SOI,
             State => Unknown_State));
      end;
   end Start_Lex;

   procedure Start_Parse
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   is
      Junk : Terminal_Ref := Append_Stream_Element (Tree, Stream, Tree.SOI, State);
      pragma Unreferenced (Junk);
   begin
      Tree.Parents_Set := False;
   end Start_Parse;

   function State
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Unknown_State_Index
   is
      pragma Unreferenced (Tree, Stream);
   begin
      return Stream_Element_Lists.Element (Element.Cur).State;
   end State;

   function State (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return State_Index
   is begin
      return Stream_Element_Lists.Element (Tree.Streams (Stream.Cur).Stack_Top).State;
   end State;

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural
   is begin
      return Natural (Tree.Streams.Length);
   end Stream_Count;

   procedure Stream_Delete
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in out Stream_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Element.Cur then
         Parse_Stream.Stack_Top := Stream_Element_Lists.No_Element;
      end if;

      Parse_Stream.Elements.Delete (Element.Cur);
   end Stream_Delete;

   function Stream_Error_Iterate
     (Tree   : aliased in Syntax_Trees.Tree;
      Stream :         in Stream_ID)
     return Stream_Error_Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Stream_Error_Iterator'(Tree => Tree'Access, Stream => Stream.Cur);
   end Stream_Error_Iterate;

   function Stream_First
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean)
     return Stream_Index
   is begin
      return Result : Stream_Index := (Cur => Tree.Streams (Stream.Cur).Elements.First) do
         if Skip_SOI then
            Stream_Element_Lists.Next (Result.Cur);
         end if;
      end return;
   end Stream_First;

   function Stream_First
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean)
     return Rooted_Ref
   is
      Cur : constant Stream_Element_Lists.Cursor := Tree.Streams (Stream.Cur).Elements.First;
   begin
      return Result : Rooted_Ref := (Stream, (Cur => Cur), Stream_Element_Lists.Element (Cur).Node) do
         if Skip_SOI then
            Tree.Stream_Next (Result, Rooted => True);
         end if;
      end return;
   end Stream_First;

   function Stream_Input_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is
      use Stream_Element_Lists;
      use SAL;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Element : Cursor         := Next (Parse_Stream.Stack_Top);
      Result  : Base_Peek_Type := 0;
   begin
      loop
         exit when not Has_Element (Element);
         Result := @ + 1;
         Element := Next (Element);
      end loop;
      return Result;
   end Stream_Input_Length;

   procedure Stream_Insert
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Node.Parent := Invalid_Node_Access;
      Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Node,
            State => Unknown_State),
         Before   => Before.Cur);
   end Stream_Insert;

   function Stream_Insert
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Index)
     return Stream_Node_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Node.Parent := Invalid_Node_Access;
      return Result : constant Stream_Node_Ref :=
        (Stream         => Stream,
         Node           => Node,
         Element        =>
           (Cur         => Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => Node,
                  State => Unknown_State),
               Before   => Before.Cur)));
   end Stream_Insert;

   function Stream_Last
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_EOI : in Boolean)
     return Stream_Index
   is begin
      return Result : Stream_Index := (Cur => Tree.Streams (Stream.Cur).Elements.Last) do
         if Skip_EOI then
            Stream_Element_Lists.Previous (Result.Cur);
         end if;
      end return;
   end Stream_Last;

   function Stream_Last
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_EOI : in Boolean)
     return Rooted_Ref
   is begin
      return To_Rooted_Ref (Tree, Stream, Stream_Last (Tree, Stream, Skip_EOI));
   end Stream_Last;

   function Stream_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is begin
      return SAL.Base_Peek_Type (Tree.Streams (Stream.Cur).Elements.Length);
   end Stream_Length;

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is begin
      return
        (if Element = Invalid_Stream_Index
         then (Cur => Tree.Streams (Stream.Cur).Elements.First)
         else (Cur => Stream_Element_Lists.Next (Element.Cur)));
   end Stream_Next;

   function Stream_Next
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   is
      Result : Stream_Node_Ref := Ref;
   begin
      Stream_Next (Tree, Result, Rooted => True);
      return Result;
   end Stream_Next;

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Element := (Cur => Next (Ref.Element.Cur));

      if not Has_Element (Ref.Element.Cur) then
         if Ref.Stream = Tree.Shared_Stream then
            Ref.Stream := Invalid_Stream_ID;

         elsif not Has_Element (Tree.Streams (Ref.Stream.Cur).Shared_Link) then
            Ref.Stream := Invalid_Stream_ID;

         else
            Ref.Element.Cur := Tree.Streams (Ref.Stream.Cur).Shared_Link;
            Ref.Stream      := Tree.Shared_Stream;
         end if;
      end if;

      Ref.Node :=
        (if Has_Element (Ref.Element.Cur)
         then (if Rooted
               then Element (Ref.Element.Cur).Node
               else First_Terminal (Tree, Element (Ref.Element.Cur).Node))
         else Invalid_Node_Access);
   end Stream_Next;

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Parents.Clear;
      Stream_Next (Tree, Ref.Ref, Rooted => True);

      if Ref.Ref.Element.Cur /= No_Element and not Rooted then
         Ref.Ref.Node := First_Terminal
           (Tree, Stream_Element_Lists.Element (Ref.Ref.Element.Cur).Node, Ref.Parents);
      end if;
   end Stream_Next;

   function Stream_Prev
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is
      pragma Unreferenced (Tree, Stream);
   begin
      return (Cur => Stream_Element_Lists.Previous (Element.Cur));
   end Stream_Prev;

   function Stream_Prev
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   is
      Result : Stream_Node_Ref := Ref;
   begin
      Stream_Prev (Tree, Result);
      return Result;
   end Stream_Prev;

   procedure Stream_Prev
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean := True)
   is
      use Stream_Element_Lists;
   begin
      Ref.Element := (Cur => Previous (Ref.Element.Cur));

      if not Has_Element (Ref.Element.Cur) then
         Ref.Stream := Invalid_Stream_ID;
      end if;

      Ref.Node :=
        (if Has_Element (Ref.Element.Cur)
         then (if Rooted
               then Element (Ref.Element.Cur).Node
               else Tree.Last_Terminal (Element (Ref.Element.Cur).Node))
         else Invalid_Node_Access);
   end Stream_Prev;

   procedure Stream_Prev
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Parents.Clear;
      Stream_Prev (Tree, Ref.Ref, Rooted => True);

      if Ref.Ref.Element.Cur /= No_Element and not Rooted then
         Ref.Ref.Node := Last_Terminal
           (Tree, Stream_Element_Lists.Element (Ref.Ref.Element.Cur).Node, Ref.Parents);
      end if;
   end Stream_Prev;

   function Subtree_Image
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Node_Access;
      Node_Numbers : in Boolean := True;
      Non_Grammar  : in Boolean := False;
      Augmented    : in Boolean := False;
      Line_Numbers : in Boolean := False;
      Level        : in Integer := 0)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"" & ASCII.LF;
   begin
      for I in 1 .. Level loop
         Result := @ & "| ";
      end loop;
      Result := @ &
        (if Node = Invalid_Node_Access
         then "<null>"
         else Image
           (Tree, Node,
            Children              => False,
            Node_Numbers          => Node_Numbers,
            RHS_Index             => True,
            Terminal_Node_Numbers => True,
            Non_Grammar           => Non_Grammar,
            Line_Numbers          => Line_Numbers,
            Augmented             => Augmented));

      if Node /= Invalid_Node_Access and then Node.Label = Nonterm then
         for Child of Node.Children loop
            Result := @ & Subtree_Image
              (Tree, Child, Node_Numbers, Non_Grammar, Augmented, Line_Numbers, Level + 1);
         end loop;
      end if;

      return -Result;
   end Subtree_Image;

   function Subtree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access
   is
      pragma Unreferenced (Tree);
      N : Valid_Node_Access := Node;
   begin
      loop
         exit when N.Parent = Invalid_Node_Access;
         N := N.Parent;
      end loop;
      return N;
   end Subtree_Root;

   function To_Node_Access (Item : in Valid_Node_Access_Array) return Node_Access_Array
   is begin
      return (for I in Item'Range => Item (I));
   end To_Node_Access;

   function To_Real_Recover_Token (Item : in Stream_Node_Ref) return Real_Recover_Token
   is begin
      return
        (Virtual      => False,
         Element_Node => Stream_Element_Lists.Element (Item.Element.Cur).Node,
         Node         => Item.Node);
   end To_Real_Recover_Token;

   function To_Rooted_Ref
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Rooted_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return (Stream, Element, Stream_Element_Lists.Element (Element.Cur).Node);
   end To_Rooted_Ref;

   function To_Stream_Node_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Node   : in Valid_Node_Access)
     return Stream_Node_Ref
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Root : constant Valid_Node_Access := Tree.Subtree_Root (Node);
      Cur  : Cursor                     := Parse_Stream.Elements.First;
   begin
      loop
         exit when Element (Cur).Node = Root;
         Next (Cur);
      end loop;

      return
        (Stream  => Stream,
         Element => (Cur => Cur),
         Node    => Node);
   end To_Stream_Node_Ref;

   function To_Stream_Node_Parents (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Stream_Node_Parents
   is begin
      return Result : Stream_Node_Parents := (Ref, Parents => <>) do
         if Ref = Invalid_Stream_Node_Ref or else Rooted (Ref) then
            null;

         elsif Tree.Parents_Set then
            declare
               Inverted_Parents : Node_Stacks.Stack;
               Node             : Valid_Node_Access          := Ref.Node.Parent;
               Root_Node        : constant Valid_Node_Access :=
                 Stream_Element_Lists.Element (Ref.Element.Cur).Node;
            begin
               loop
                  Inverted_Parents.Push (Node);
                  exit when Node = Root_Node;
                  Node := Node.Parent;
               end loop;
               Result.Parents := Inverted_Parents.Invert;
            end;
         else
            declare
               use Stream_Element_Lists;
            begin
               Result.Ref.Node := Tree.First_Terminal (Element (Ref.Element.Cur).Node, Result.Parents);
               loop
                  exit when Result.Ref.Node = Ref.Node;
                  Tree.Next_Terminal (Result, Following => True);
               end loop;
            end;
         end if;
      end return;
   end To_Stream_Node_Parents;

   function To_Valid_Node_Access (Item : in Node_Access_Array) return Valid_Node_Access_Array
   is begin
      return (for I in Item'Range => Item (I));
   end To_Valid_Node_Access;

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Traversing;
   end Traversing;

   function Trimmed_Image (Tree : in Syntax_Trees.Tree; Item : in Stream_ID) return String
   is begin
      return Trimmed_Image (Tree.Streams (Item.Cur).Label);
   end Trimmed_Image;

   function Trimmed_Image (Item : in Stream_Index) return String
   is begin
      return
        (if Item = Invalid_Stream_Index
         then "-"
         else Trimmed_Image (Stream_Element_Lists.Element (Item.Cur).Node.Node_Index));
   end Trimmed_Image;

   function Trimmed_Image (Node : in Node_Access) return String
   is begin
      return
        (if Node = Invalid_Node_Access
         then "-"
         else Trimmed_Image (Node.Node_Index));
   end Trimmed_Image;

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String
   is begin
      return Node_Index'(Tree.Nodes.Last_Index)'Image;
   end Tree_Size_Image;

   procedure Update_Error
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Error_Ref : in     Stream_Error_Ref;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access_Constant)
   is
      use Error_Data_Lists;
      Error_Node : constant Valid_Node_Access := Syntax_Trees.Error_Node (Error_Ref);

      New_Error_List : constant Error_List_Access := new List'(Error_Node.Error_List.all);
      Cur            : Cursor                     := New_Error_List.First;
      Moved_Ref      : Stream_Node_Parents        := Error_Ref.Ref;
   begin
      loop
         exit when Dispatch_Equal (Data, New_Error_List (Cur));
         Next (Cur);
      end loop;

      New_Error_List (Cur) := Data;

      if Error_Ref.Deleted /= Valid_Node_Access_Lists.No_Element then
         declare
            Old_Following_Deleted : Valid_Node_Access_Lists.List renames Error_Ref.Ref.Ref.Node.Following_Deleted;

            pragma Assert (Error_Node.Following_Deleted.Length = 0);

            New_Error_Node : constant Valid_Node_Access := Copy_Node
              (Tree, Error_Node,
               Parent                 => Invalid_Node_Access,
               Copy_Children          => False,
               Copy_Following_Deleted => False,
               User_Data              => User_Data,
               New_Error_List         => New_Error_List,
               Set_Error_List         => True,
               Set_Copied_Node        => True);

         begin
            Move_Element
              (Tree, Stream, Moved_Ref,
               New_Node => Copy_Node
                 (Tree, Error_Ref.Ref.Ref.Node,
                  Parent =>
                    (if Tree.Parents_Set
                     then Error_Ref.Ref.Ref.Node.Parent
                     else Invalid_Node_Access),
                  Copy_Children          => False,
                  Copy_Following_Deleted => False,
                  User_Data              => User_Data),
               User_Data                 => User_Data);

            for Cur in Old_Following_Deleted.Iterate loop
               Moved_Ref.Ref.Node.Following_Deleted.Append
                 ((if Cur = Error_Ref.Deleted
                   then New_Error_Node
                   else Copy_Node
                     (Tree, Old_Following_Deleted (Cur),
                      Parent                 => Invalid_Node_Access,
                      User_Data              => User_Data,
                      Copy_Children          => False,
                      Copy_Following_Deleted => False,
                      Set_Copied_Node        => True)));
            end loop;

            New_Error_List (Cur).Adjust_Copy;
            New_Error_Node.Copied_Node := Invalid_Node_Access;
            for Cur in Old_Following_Deleted.Iterate loop
               Old_Following_Deleted (Cur).Copied_Node := Invalid_Node_Access;
            end loop;
         end;

      else
         Move_Element
           (Tree, Stream, Moved_Ref,
            New_Node                  => Copy_Node
              (Tree, Error_Ref.Ref.Ref.Node,
               Parent                 => Invalid_Node_Access,
               Copy_Children          => False,
               Copy_Following_Deleted => True,
               New_Error_List         => New_Error_List,
               Set_Error_List         => True,
               User_Data              => User_Data),
            User_Data                 => User_Data);
      end if;
   end Update_Error;

   function Valid_Root (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Root /= Invalid_Node_Access or Tree.Stream_Count > 0;
   end Valid_Root;

   function Valid_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is begin
      return
        Tree.Contains (Ref.Stream, Ref.Element) and then
        (if Ref.Node = Invalid_Node_Access
         then Ref /= Invalid_Stream_Node_Ref
         else
           (not Tree.Parents_Set or else
              Tree.Is_Descendant_Of (Root => Tree.Get_Node (Ref.Stream, Ref.Element), Descendant => Ref.Node)));
   end Valid_Stream_Node;

   procedure Validate_Tree
     (Tree              : in out Syntax_Trees.Tree;
      User_Data         : in out User_Data_Type'Class;
      Error_Reported    : in out Node_Sets.Set;
      Node_Index_Order  : in     Boolean;
      Byte_Region_Order : in     Boolean                    := True;
      Root              : in     Node_Access                := Invalid_Node_Access;
      Validate_Node     : in     Syntax_Trees.Validate_Node := null)
   is

      Real_Root : Node_Access;

      Last_Source_Terminal_Pos : Base_Buffer_Pos := Buffer_Pos'First;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access)
      is
         Node_Image_Output : Boolean := False;

         procedure Put_Error (Msg : in String)
         is begin
            --  Tell the caller that some error happened, even if not
            --  Node_Index_Order.
            Error_Reported.Insert (Node);

            if not Node_Image_Output then
               Tree.Lexer.Trace.Put_Line
                 (Tree.Error_Message
                    (Node,
                     Image (Tree, Node,
                            Children     => False,
                            Node_Numbers => True)));

               Node_Image_Output := True;
            end if;

            Tree.Lexer.Trace.Put_Line (Tree.Error_Message (Node, "... invalid_tree: " & Msg));
         end Put_Error;

      begin
         --  Node_Index checked in Nonterm below; no check needed for
         --  Source_Terminal since that is set by lexer. Node_Index on Virtual
         --  terminals not checked.

         if Node = Real_Root then
            if Node.Parent /= null then
               Put_Error ("root parent set expecting null");
            end if;
         elsif Node.Parent = null then
            Put_Error ("parent null expecting set");
            --  Node in Parent.Children checked below.
         end if;

         --  Augmented handled by Validate_Node below.

         if Node.Error_List /= null then
            for Err of Node.Error_List.all loop
               declare
                  Node_Error_Reported : Boolean :=
                    (if Node_Index_Order
                     then Error_Reported.Contains (Node)
                     else False);
               begin
                  Validate_Error (Err, Tree, Node, Node_Error_Reported);
                  if Node_Error_Reported then
                     Error_Reported.Insert (Node);
                     Node_Image_Output := True;
                  end if;
               end;
            end loop;
         end if;

         case Node.Label is
         when Terminal_Label =>
            if Node.Sequential_Index /= Invalid_Sequential_Index then
               Put_Error ("invalid Sequential_Index:" & Node.Sequential_Index'Image);
            end if;
            case Terminal_Label'(Node.Label) is
            when Source_Terminal =>
               if Byte_Region_Order and then Node.Byte_Region.First < Last_Source_Terminal_Pos then
                  Put_Error ("byte_region out of order");
               end if;
               if Node.Non_Grammar.Length > 0 then
                  Last_Source_Terminal_Pos := Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last;
               else
                  Last_Source_Terminal_Pos := Node.Byte_Region.Last;
               end if;

               for Deleted of Node.Following_Deleted loop
                  if Deleted.Parent /= Node then
                     Put_Error ("deleted.parent wrong");
                  end if;
                  if not Tree.In_Tree (Deleted) then
                     Put_Error ("deleted not in Tree.Nodes");
                  end if;
                  if Deleted.Error_List /= null then
                     for Err of Deleted.Error_List.all loop
                        declare
                           Node_Error_Reported : Boolean :=
                             (if Node_Index_Order
                              then Error_Reported.Contains (Deleted)
                              else False);
                        begin
                           Validate_Error (Err, Tree, Deleted, Node_Error_Reported);
                           if Node_Error_Reported then
                              Error_Reported.Insert (Deleted);
                           end if;
                        end;
                     end loop;
                  end if;
                  if Deleted.Following_Deleted.Length > 0 then
                     Put_Error ("deleted has following_deleted");
                  end if;
               end loop;

            when Virtual_Terminal | Virtual_Identifier =>
               if Node.Non_Grammar.Length > 0 then
                  Last_Source_Terminal_Pos := Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last;
               end if;
            end case;

         when Nonterm =>
            for I in Node.Children'Range loop
               if Node.Children (I) = null then
                  Put_Error ("child" & I'Image & " deleted");

               else
                  if Node_Index_Order and then
                    abs Node.Children (I).Node_Index >= abs Node.Node_Index
                  then
                     Put_Error
                       ("child.node_index" & Node_Index'Image (abs Node.Children (I).Node_Index) &
                          " >= parent.node_index" & Node_Index'Image (abs Node.Node_Index));
                  end if;
                  declare
                     Child_Parent : constant Node_Access := Node.Children (I).Parent;
                  begin
                     if Child_Parent /= Node then
                        Put_Error
                          ((if Child_Parent = Invalid_Node_Access
                            then "child.parent invalid"
                            else "child.parent incorrect"));
                     end if;
                  end;
               end if;
            end loop;
         end case;

         if Validate_Node /= null then
            declare
               Node_Error_Reported : Boolean :=
                 (if Node_Index_Order
                  then Error_Reported.Contains (Node)
                  else False);
            begin
               Validate_Node (Tree, Node, User_Data, Node_Error_Reported);
               if Node_Error_Reported then
                  Error_Reported.Insert (Node);
                  Node_Image_Output := True;
               end if;
            end;
         end if;
      end Process_Node;

   begin
      if Root /= Invalid_Node_Access then
         Real_Root := Root;
         Process_Tree (Tree, Root, Process_Node'Access);
      else
         if Tree.Streams.Length = 0 then
            if Tree.Root = Invalid_Node_Access then
               Tree.Lexer.Trace.Put_Line
                 (Error_Message
                    (Tree.Lexer.File_Name, 1, 1, "... invalid_tree: Tree.Root not set"));
            else
               Real_Root := Tree.Root;
               Process_Tree (Tree, Tree.Root, Process_Node'Access);

               if Validate_Node = Mark_In_Tree'Access then
                  for Node of Tree.Nodes loop
                     if Node.Augmented = null then
                        Error_Reported.Insert (Node);

                        --  Node is not in tree, so can't use Tree.Error_Message
                        Tree.Lexer.Trace.Put_Line
                          (Image
                             (Tree, Node,
                              Children     => False,
                              Node_Numbers => True,
                              Safe_Only    => True));
                        Tree.Lexer.Trace.Put_Line
                          ("... invalid_tree: node in Tree.Nodes but not in tree (has parent, should not)");
                     end if;
                  end loop;
               end if;
            end if;
         else
            for Stream of Tree.Streams loop
               declare
                  use Stream_Element_Lists;
                  Cur : Cursor := Stream.Elements.First;
               begin
                  loop
                     exit when Cur = No_Element;
                     Real_Root := Element (Cur).Node;
                     Process_Tree (Tree, Real_Root, Process_Node'Access);
                     Next (Cur);
                  end loop;
               end;
            end loop;
         end if;
      end if;
   end Validate_Tree;

   procedure Sequential_Index_Cleared (Tree : in Syntax_Trees.Tree)
   is
      Stream : Stream_ID := Tree.Shared_Stream;
   begin
      loop
         declare
            Terminal : Stream_Node_Parents := Tree.To_Stream_Node_Parents
              (Tree.To_Rooted_Ref (Stream, Tree.Stream_First (Stream, Skip_SOI => False)));
         begin
            loop
               if Terminal.Ref.Node.Sequential_Index /= Invalid_Sequential_Index then
                  raise SAL.Programmer_Error with "sequential_index not cleared: " & Tree.Image (Terminal.Ref);
               end if;
               Tree.Next_Terminal (Terminal, Following => True);
               exit when Terminal.Ref = Invalid_Stream_Node_Ref;
            end loop;
         end;
         Tree.Next_Parse_Stream (Stream);
         exit when Stream = Invalid_Stream_ID;
      end loop;
   end Sequential_Index_Cleared;

end WisiToken.Syntax_Trees;
