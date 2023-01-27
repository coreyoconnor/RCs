--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
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

with Ada.Finalization;
with Ada.Streams;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Image_Aux;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Image;
with SAL.Gen_Indefinite_Doubly_Linked_Lists.Gen_Image_Aux;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   type Recover_Op_Label is (Fast_Forward, Undo_Reduce, Push_Back, Insert, Delete);
   subtype Insert_Delete_Op_Label is Recover_Op_Label range Insert .. Delete;
   --  Fast_Forward is a placeholder to mark a fast_forward parse; that
   --  resets what operations are allowed to be done on a config.
   --
   --  Undo_Reduce is the inverse of Reduce.
   --
   --  Push_Back pops the top stack item, and moves the input stream
   --  pointer back to the first shared_terminal contained by that item.
   --
   --  Insert inserts a new token in the token input stream, before the
   --  given point in Terminals.
   --
   --  Delete deletes one item from the token input stream, at the given
   --  point.

   --  WORKAROUND: GNAT Community 2020 with -gnat2020 S'Image outputs
   --  integer when S is a subtype. Fixed in Community 2021.
   function Image (Item : in Recover_Op_Label) return String
   is (case Item is
       when Fast_Forward => "FAST_FORWARD",
       when Undo_Reduce  => "UNDO_REDUCE",
       when Push_Back    => "PUSH_BACK",
       when Insert       => "INSERT",
       when Delete       => "DELETE");

   type Recover_Op (Op : Recover_Op_Label := Fast_Forward) is record
      --  Stores recover operations during error recovery. We store enough
      --  information to perform the operation on the main parser stack and
      --  input stream when the config is the result of a successful
      --  recover.

      case Op is
      when Fast_Forward =>
         FF_First_Index : Syntax_Trees.Sequential_Index;
         --  First token in fast forward region

         FF_Next_Index : Syntax_Trees.Sequential_Index;
         --  Config current_token after the operation is done; next after the
         --  last token in the fast forward region.
         --
         --  If FF_First_Index = FF_Next_Index, no tokens were actually parsed
         --  for the fast_forward; it is just a marker to allow error recovery
         --  to reset op order restrictions.

      when Undo_Reduce =>
         Nonterm : Token_ID;
         --  The nonterminal popped off the stack.

         Token_Count : SAL.Base_Peek_Type;
         --  The number of tokens pushed on the stack.

         UR_Token_Index : Syntax_Trees.Base_Sequential_Index;
         --  First terminal in the undo_reduce token; Invalid_Sequential_Index if
         --  empty. Used to check that successive Undo_Reduce are valid.

      when Push_Back =>
         PB_ID : Token_ID;
         --  The nonterm ID popped off the stack.

         PB_Token_Index : Syntax_Trees.Base_Sequential_Index;
         --  First terminal in the pushed_back token; Invalid_Sequential_Index if
         --  empty. Used to check that successive Push_Backs are valid.

      when Insert =>
         Ins_ID : Token_ID;
         --  The token ID inserted.

         Ins_Before : Syntax_Trees.Sequential_Index;
         --  Ins_ID is inserted before Ins_Before.

      when Delete =>
         Del_ID : Token_ID;
         --  The token ID deleted; a terminal token.

         Del_Token_Index : Syntax_Trees.Sequential_Index;
         --  Token at Del_Token_Index is deleted.

      end case;
   end record;
   subtype Insert_Delete_Op is Recover_Op with Dynamic_Predicate => (Insert_Delete_Op.Op in Insert_Delete_Op_Label);
   subtype Insert_Op is Recover_Op with Dynamic_Predicate => (Insert_Op.Op = Insert);

   function Token_Index (Op : in Insert_Delete_Op) return Syntax_Trees.Sequential_Index
   is (case Insert_Delete_Op_Label'(Op.Op) is
       when Insert => Op.Ins_Before,
       when Delete => Op.Del_Token_Index);

   function ID (Op : in Insert_Delete_Op) return WisiToken.Token_ID
   is (case Insert_Delete_Op_Label'(Op.Op) is
       when Insert => Op.Ins_ID,
       when Delete => Op.Del_ID);

   function Equal (Left : in Recover_Op; Right : in Insert_Op) return Boolean;

   package Recover_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Recover_Op, Default_Element =>
        (Fast_Forward, Syntax_Trees.Sequential_Index'Last, Syntax_Trees.Sequential_Index'First), Capacity => 80);
   --  Using a fixed size vector significantly speeds up
   --  McKenzie_Recover. The capacity is determined by the maximum number
   --  of repair operations, which is limited by the cost_limit McKenzie
   --  parameter plus an arbitrary number from the language-specific
   --  repairs; in practice, a capacity of 80 is enough so far. If a
   --  config does hit that limit, it is abandoned; some other config is
   --  likely to be cheaper.

   package Recover_Op_Array_Refs is new Recover_Op_Arrays.Gen_Refs;

   function Recover_Op_Image (Item : in Recover_Op; Descriptor : in WisiToken.Descriptor) return String
   is ("(" & Image (Item.Op) & ", " &
         (case Item.Op is
          when Fast_Forward => Syntax_Trees.Trimmed_Image (Item.FF_First_Index) & ", " &
            Syntax_Trees.Trimmed_Image (Item.FF_Next_Index),
          when Undo_Reduce  => Image (Item.Nonterm, Descriptor) & "," &
            Item.Token_Count'Image & ", " & Syntax_Trees.Trimmed_Image (Item.UR_Token_Index),
          when Push_Back    => Image (Item.PB_ID, Descriptor) & ", " & Syntax_Trees.Trimmed_Image (Item.PB_Token_Index),
          when Insert       => Image (Item.Ins_ID, Descriptor) & ", " & Syntax_Trees.Trimmed_Image (Item.Ins_Before),
          when Delete       => Image (Item.Del_ID, Descriptor) & ", " &
            Syntax_Trees.Trimmed_Image (Item.Del_Token_Index))
         & ")");

   function Image (Item : in Recover_Op; Descriptor : in WisiToken.Descriptor) return String
     renames Recover_Op_Image;

   function Recover_Op_Array_Image is new Recover_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Image (Item : in Recover_Op_Arrays.Vector; Descriptor : in WisiToken.Descriptor) return String
     renames Recover_Op_Array_Image;

   function None (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean;
   --  True if Ops contains no Op.

   function None_Since_FF (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean;
   --  True if Ops contains no Op after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward).

   type Recover_Op_Nodes (Op : Insert_Delete_Op_Label := Insert) is record
      --  Stores recover operation data used by the main parser to implement
      --  insert, delete; and by the client editor to implement auto
      --  corrections.
      --
      --  We can't compute buffer positions until after
      --  User_Data.Insert_Tokens runs, after parse completes; it can move
      --  non_grammar around, which affects buffer positions.

      Input_Node_Index : Syntax_Trees.Node_Index := Syntax_Trees.Invalid_Node_Index;
      --  Used by Get_Tree to store the node_index for Ins_Node or Del_Node
      --  read from the input file; converted to Node_Access by Set_Node_Access.

      case Op is
      when Insert =>
         Ins_ID : Token_ID := Invalid_Token_ID;
         --  The token ID inserted.

         Ins_Before : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'First;
         --  Ins_ID is inserted before Ins_Before in the Shared_Stream.

         Ins_Node : Syntax_Trees.Node_Access := Syntax_Trees.Invalid_Node_Access;
         --  The parse stream node holding the inserted token.

      when Delete =>
         Del_ID : Token_ID := Invalid_Token_ID;
         --  The token ID deleted; a terminal token.

         Del_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'First;
         --  Token at Del_Index is deleted; used by parser to skip the token.

         Del_Node : Syntax_Trees.Node_Access := Syntax_Trees.Invalid_Node_Access;
         --  Del_Node is deleted; used by post-parse actions to adjust for the
         --  deleted token. Del_Node.Parent is the previous non-deleted terminal.
      end case;
   end record;

   subtype Delete_Op_Nodes is Recover_Op_Nodes (Delete);

   package Recover_Op_Nodes_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Index_Type, Recover_Op_Nodes, Default_Element => (others => <>));

   function Image (Item : in Recover_Op_Nodes; Tree : in Syntax_Trees.Tree'Class) return String;

   function Image is new Recover_Op_Nodes_Arrays.Gen_Image_Aux
     (Aux_Data            => Syntax_Trees.Tree'Class,
      Index_Trimmed_Image => Trimmed_Image,
      Element_Image       => Image);

   function To_Recover_Op_Nodes (Item : in Recover_Op_Arrays.Vector) return Recover_Op_Nodes_Arrays.Vector;

   type Recover_Test_Info is record
      --  Used by test_mckenzie_recover.adb
      Ops           : Recover_Op_Arrays.Vector;
      Cost          : Natural;
      Enqueue_Count : Natural;
      Check_Count   : Natural;
   end record;
   type Recover_Test_Info_Access is access Recover_Test_Info;
   --  No "Free" declared for Recover_Test_Info_Access; it is only used
   --  in test_mckenzie_recover.adb, so we don't care about recovering
   --  the memory. This allows us to copy these access values freely.

   type Lexer_Error is new Syntax_Trees.Error_Data with record
      Error : WisiToken.Lexer.Error;
   end record;

   overriding procedure Adjust_Copy (Data : in out Lexer_Error) is null;
   overriding function Dispatch_Equal (Left : in Lexer_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean;
   overriding function To_Message
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Lexer_Error) return String is ("lexer");

   overriding
   procedure Validate_Error
     (Data                : in     Lexer_Error;
      Tree                : in     Syntax_Trees.Tree'Class;
      Error_Node          : in     Syntax_Trees.Valid_Node_Access;
      Node_Error_Reported : in out Boolean)
   is null;

   function Input_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Lexer_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Lexer_Error);
   for Lexer_Error'Input use Input_Lexer_Error;
   for Lexer_Error'Output use Output_Lexer_Error;

   overriding
   procedure Set_Node_Access
     (Data           : in out Lexer_Error;
      Node_Index_Map : in     Syntax_Trees.Node_Index_Array_Node_Access.Vector)
   is null;

   type Parse_Error
     (First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is new Syntax_Trees.Error_Data with record
      Expecting   : Token_ID_Set (First_Terminal .. Last_Terminal);
      Recover_Ops : aliased Recover_Op_Nodes_Arrays.Vector;

      Recover_Test : aliased Recover_Test_Info_Access; -- only set when running test_mckenzie_recover.adb.
   end record;

   overriding procedure Adjust_Copy (Data : in out Parse_Error);
   overriding function Dispatch_Equal (Left : in Parse_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean;
   overriding function To_Message
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Parse_Error) return String is ("parser");

   overriding
   procedure Validate_Error
     (Data                : in     Parse_Error;
      Tree                : in     Syntax_Trees.Tree'Class;
      Error_Node          : in     Syntax_Trees.Valid_Node_Access;
      Node_Error_Reported : in out Boolean);

   function Input_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Parse_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'

   procedure Output_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Parse_Error);
   for Parse_Error'Input use Input_Parse_Error;
   for Parse_Error'Output use Output_Parse_Error;

   overriding
   procedure Set_Node_Access
     (Data           : in out Parse_Error;
      Node_Index_Map : in     Syntax_Trees.Node_Index_Array_Node_Access.Vector);

   type In_Parse_Action_Error is new Syntax_Trees.Error_Data with record
      Status      : WisiToken.Syntax_Trees.In_Parse_Actions.Status;
      Recover_Ops : aliased Recover_Op_Nodes_Arrays.Vector;

      Recover_Test : aliased Recover_Test_Info_Access; -- only set when running test_mckenzie_recover.adb.
   end record;

   overriding procedure Adjust_Copy (Data : in out In_Parse_Action_Error);
   overriding function Dispatch_Equal
     (Left  : in In_Parse_Action_Error;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean;
   overriding function To_Message
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in In_Parse_Action_Error) return String is ("in_parse_action");

   overriding
   procedure Validate_Error
     (Data                : in     In_Parse_Action_Error;
      Tree                : in     Syntax_Trees.Tree'Class;
      Error_Node          : in     Syntax_Trees.Valid_Node_Access;
      Node_Error_Reported : in out Boolean);

   function Input_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return In_Parse_Action_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in In_Parse_Action_Error);
   for In_Parse_Action_Error'Input use Input_In_Parse_Action_Error;
   for In_Parse_Action_Error'Output use Output_In_Parse_Action_Error;

   overriding
   procedure Set_Node_Access
     (Data           : in out In_Parse_Action_Error;
      Node_Index_Map : in     Syntax_Trees.Node_Index_Array_Node_Access.Vector);

   type Error_Message is new Syntax_Trees.Error_Data with record
      Msg         : Ada.Strings.Unbounded.Unbounded_String;
      Recover_Ops : aliased Recover_Op_Nodes_Arrays.Vector;

      Recover_Test : aliased Recover_Test_Info_Access; -- only set when running test_mckenzie_recover.adb.
   end record;

   overriding procedure Adjust_Copy (Data : in out Error_Message);
   overriding function Dispatch_Equal
     (Left  : in Error_Message;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean;
   overriding function To_Message
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Error_Message) return String is ("message");

   overriding
   procedure Validate_Error
     (Data                : in     Error_Message;
      Tree                : in     Syntax_Trees.Tree'Class;
      Error_Node          : in     Syntax_Trees.Valid_Node_Access;
      Node_Error_Reported : in out Boolean);

   type Recover_Op_Array_Const_Ref_Type (Element : not null access constant Recover_Op_Nodes_Arrays.Vector) is private
   with Implicit_Dereference => Element;

   --  WORKAROUND: if Recover_Op_Array_Const_Ref_From_Cursor is named
   --  Recover_Op_Array_Const_Ref, GNAT Community 2021 confuses it with
   --  the other Recover_Op_Array_Const_Ref.
   function Recover_Op_Array_Const_Ref_From_Cursor
     (Item : in Syntax_Trees.Error_Data_Lists.Cursor)
     return Recover_Op_Array_Const_Ref_Type
   with Pre => Syntax_Trees.Error_Data_Lists.Has_Element (Item) and then
               not (Syntax_Trees.Error_Data_Lists.Element (Item) in Lexer_Error);

   function Recover_Op_Array_Const_Ref
     (Error : aliased in Syntax_Trees.Error_Data'Class)
     return Recover_Op_Array_Const_Ref_Type
   with Pre => not (Error in Lexer_Error);

   type Recover_Op_Array_Var_Ref_Type (Element : not null access Recover_Op_Nodes_Arrays.Vector) is private
   with Implicit_Dereference => Element;

   function Recover_Op_Array_Var_Ref
     (Error : aliased in out Syntax_Trees.Error_Data'Class)
     return Recover_Op_Array_Var_Ref_Type
   with Pre => not (Error in Lexer_Error);

   type Recover_Test_Const_Ref_Type (Element : not null access constant Recover_Test_Info) is private
   with Implicit_Dereference => Element;

   function Recover_Test_Const_Ref
     (Item : in Syntax_Trees.Error_Data_Lists.Cursor)
     return Recover_Test_Const_Ref_Type
   with Pre => Syntax_Trees.Error_Data_Lists.Has_Element (Item) and then
               not (Syntax_Trees.Error_Data_Lists.Element (Item) in Lexer_Error);

   type Recover_Test_Var_Ref_Type (Element : not null access Recover_Test_Info_Access) is private
   with Implicit_Dereference => Element;

   function Recover_Test_Var_Ref
     (Error : aliased in out Syntax_Trees.Error_Data'Class)
     return Recover_Test_Var_Ref_Type
   with Pre => not (Error in Lexer_Error);

   function Recover_Image
     (Error : in Syntax_Trees.Error_Data'Class;
      Tree  : in Syntax_Trees.Tree)
     return String
   with Pre => not (Error in Lexer_Error);
   --  Aggregate image of Error.Recover_Ops.

   function Recover_Image is new Syntax_Trees.Error_Data_Lists.Gen_Image_Aux (Syntax_Trees.Tree, Recover_Image);

   function Input_Error_Message (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Error_Message;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_Error_Message
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Error_Message);
   for Error_Message'Input use Input_Error_Message;
   for Error_Message'Output use Output_Error_Message;

   overriding
   procedure Set_Node_Access
     (Data           : in out Error_Message;
      Node_Index_Map : in     Syntax_Trees.Node_Index_Array_Node_Access.Vector);

   function Error_Pred_Lexer (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is a Lexer_Error; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_Parse (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is a Parse_Error; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_In_Parse_Action (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is In_Parse_Action_Error;
   --  for Syntax_Trees.Error_Predicate.

   function Error_Pred_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is an Error_Message; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_Parse_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is one of Parse_Error or Error_Message; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_Lexer_Parse_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is one of Lexer_Error, Parse_Error, or
   --  Error_Message; for Syntax_Trees.Error_Predicate.

   function Find_Parse_In_Parse_Action_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;
   --  Return the first Parse_Error or In_Parse_Action_Error from Node.
   --
   --  This does not return a reference, because any update to an error
   --  requires copying the error node; see note at declaration of
   --  Syntax_Trees.Error_Data.
   --  FIXME: delete if not used.

   function Find_Non_Lexer_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;
   --  FIXME: delete if not used.

   type Base_Parser is abstract new Ada.Finalization.Limited_Controlled
   with record
      Tree        : aliased Syntax_Trees.Tree;
      Productions : Syntax_Trees.Production_Info_Trees.Vector;
      User_Data   : Syntax_Trees.User_Data_Access;

      Partial_Parse_Active    : access Boolean;
      Partial_Parse_Byte_Goal : access WisiToken.Buffer_Pos;
      --  Used by In_Parse_Actions to terminate Partial_Parse.
   end record;
   --  Common to all parsers. Finalize should free any allocated objects.

   type Base_Parser_Access is access all Base_Parser'Class;
   type Factory is access function return Base_Parser_Access;
   type Free_Parser is access procedure (Object : in out Base_Parser_Access);

   function Source_File_Name (Item : in Base_Parser'Class) return String
   is (Item.Tree.Lexer.File_Name);

   function Get_In_Parse_Action
     (Parser : in Base_Parser;
      ID     : in Production_ID)
     return Syntax_Trees.In_Parse_Actions.In_Parse_Action;

   function Get_Post_Parse_Action
     (Productions : in Syntax_Trees.Production_Info_Trees.Vector;
      ID          : in Production_ID)
     return Syntax_Trees.Post_Parse_Action;

   function Get_Post_Parse_Action
     (Parser : in Base_Parser;
      ID     : in Production_ID)
     return Syntax_Trees.Post_Parse_Action;

   function Next_Grammar_Token
     (Parser            : in out Base_Parser'Class;
      Last_Grammar_Node : in out WisiToken.Syntax_Trees.Node_Access)
     return Token_ID
   with Post => Next_Grammar_Token'Result /= Invalid_Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Parser.Tree (Stream) and return
   --  its ID. If is it a non_grammar token, store it in
   --  Last_Grammar_Node.Non_Grammar or Parser.Tree.Non_Grammar, and
   --  repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All (Parser : in out Base_Parser'Class)
   with Pre => Parser.Tree.Cleared;
   --  Call Tree.Start_Lex, clear Last_Grammar_Node; reset User_Data.
   --  Then call Next_Grammar_Token repeatedly until EOF_ID is returned,
   --  storing all tokens in Parser.Tree.Shared_Stream.
   --
   --  The user must first call Lexer.Reset_* to set the input text.

   type KMN is record
      --  Similar to [Lahav 2004] page 6; describes changed and unchanged
      --  regions in a text buffer. We assume the range boundaries do not
      --  break a multi-byte character.

      Stable_Bytes : Zero_Buffer_Pos; -- Count of unmodified bytes before change
      Stable_Chars : Zero_Buffer_Pos; -- "" characters

      Inserted_Bytes : Zero_Buffer_Pos; -- Count of inserted bytes, after Stable.
      Inserted_Chars : Zero_Buffer_Pos;

      Deleted_Bytes : Zero_Buffer_Pos; -- Count of deleted bytes, after Stable
      Deleted_Chars : Zero_Buffer_Pos;
   end record;

   Invalid_KMN : constant KMN := (others => 0);

   function Image (KMN : in WisiToken.Parse.KMN) return String;

   procedure Validate_KMN
     (KMN                      : in WisiToken.Parse.KMN;
      Initial_Stable_Byte_First : in Buffer_Pos;
      Initial_Stable_Char_First : in Buffer_Pos;
      Edited_Stable_Byte_First  : in Buffer_Pos;
      Edited_Stable_Char_First  : in Buffer_Pos;
      Initial_Text_Byte_Region  : in Buffer_Region;
      Initial_Text_Char_Region  : in Buffer_Region;
      Edited_Text_Byte_Region   : in Buffer_Region;
      Edited_Text_Char_Region   : in Buffer_Region);
   --  Raise User_Error if KMN violates text regions.

   package KMN_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (KMN);

   function Image is new KMN_Lists.Gen_Image (Image);

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region);

   procedure Parse
     (Parser     : in out Base_Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is abstract;
   --  If Pre_Edited, skip this first step (used for unit tests). Else if
   --  Edits is empty, call Lex_All; if Edits is not empty, call
   --  Edit_Tree.
   --
   --  Then execute parse algorithm to parse the new tokens,
   --  storing the result in Parser.Tree for Execute_Actions.
   --
   --  If Log_File is open, write information about each error recover
   --  session to it. See implementation for format.
   --
   --  If a non-recoverable parse error is encountered, raises
   --  Syntax_Error. Parser.Lexer_Errors and Parser contain information
   --  about the errors.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.

   procedure Put_Errors (Tree : in Syntax_Trees.Tree)
   with Pre => Tree.Editable;
   procedure Put_Errors (Parser : in Base_Parser'Class)
   with Pre => Parser.Tree.Editable;
   --  Output Parser.Tree errors to Tree.Lexer.Trace.

   procedure Put_Errors (Tree : Syntax_Trees.Tree; Stream : in Syntax_Trees.Stream_ID);
   procedure Put_Errors (Parser : in Base_Parser'Class; Stream : in Syntax_Trees.Stream_ID);
   --  Output Parser.Tree.Stream errors to Tree.Lexer.Trace.

   procedure Execute_Actions
     (Tree                : in out Syntax_Trees.Tree;
      Productions         : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data           : in     Syntax_Trees.User_Data_Access;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region);
   --  Implements Execute_Actions, allows specifying different tree
   --  (needed by wisitoken-bnf-generate).

   procedure Execute_Actions
     (Parser              : in out Base_Parser'Class;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region);
   --  Execute all actions in Parser.Tree nodes that overlap
   --  Action_Region_Bytes; all nodes if Action_Region_Bytes =
   --  Null_Buffer_Region. See wisitoken-syntax_trees.ads for other
   --  actions performed by Execute_Actions.

private

   type Recover_Op_Array_Const_Ref_Type (Element : not null access constant Recover_Op_Nodes_Arrays.Vector) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Recover_Op_Array_Var_Ref_Type (Element : not null access Recover_Op_Nodes_Arrays.Vector) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Recover_Test_Const_Ref_Type (Element : not null access constant Recover_Test_Info) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Recover_Test_Var_Ref_Type (Element : not null access Recover_Test_Info_Access) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   --  Visible for child packages

   procedure Process_Grammar_Token
     (Parser : in out Base_Parser'Class;
      Token  : in     Lexer.Token;
      Node   : in     Syntax_Trees.Valid_Node_Access);

   procedure Process_Non_Grammar_Token
     (Parser       : in out Base_Parser'Class;
      Grammar_Node : in     Syntax_Trees.Valid_Node_Access;
      Token        : in     Lexer.Token);

end WisiToken.Parse;
