--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi-parse-common.el - defines common stuff.
--
--  [2] wisi.texi - defines parse action functions.
--
--  [3] wisi-process-parse.el - defines elisp/process API
--
--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SAL.Gen_Unbounded_Definite_Vectors;
with SAL.Generic_Decimal_Image;
with WisiToken.Parse;
with WisiToken.Syntax_Trees;
package Wisi is
   use all type WisiToken.Syntax_Trees.Node_Access;
   use all type WisiToken.Line_Region;
   use all type WisiToken.Cache_Version;
   use all type WisiToken.Syntax_Trees.Augmented_Class_Access;
   use all type WisiToken.Base_Buffer_Pos;

   Protocol_Error : exception;

   procedure Skip
     (Source : in     String;
      Last   : in out Integer;
      Char   : in     Character);
   --  Check that Source (Last + 1) = Char. If so, increment Last.
   --  If not, raise Protocol_Error.

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String;
   --  Returns content of quoted string in Source at Last + 1 ... Handles
   --  all '\' escapes by copying them literally into result, while using
   --  them to find the terminating quote.
   --
   --  Raises Protocol_Error for a missing end quote.

   function Get_Enum
     (Source : in     String;
      Last   : in out Integer)
     return String;
   --  Returns next space-delimited word (nominally the value of some
   --  enumeration type) in Source at Last + 1 ...

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer;

   procedure To_Unix_Line_Endings
     (Source           : in     Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer);
   --  Source is assumed to have DOS line endings; convert them to Unix.

   function Image_Action (Action : in WisiToken.Syntax_Trees.Post_Parse_Action) return String;
   --  For Image_Action in Syntax_Trees.Image

   function Elisp_Escape_Quotes (Item : in String) return String;
   --  Prefix any '"' in Item with '\' for elisp.

   type Base_Post_Parse_Action_Type is (Navigate, Face, Indent, None);
   --  Must match first few items in wisi-parse-common.el wisi-post-parse-actions.

   subtype Post_Parse_Action_Type is Base_Post_Parse_Action_Type range Navigate .. Indent;

   type Parse_Data_Type is abstract new WisiToken.Syntax_Trees.User_Data_Type with private;
   type Parse_Data_Access is access all Parse_Data_Type'Class;
   type Parse_Data_Access_Constant is access constant Parse_Data_Type'Class;

   procedure Initialize (Data : in out Parse_Data_Type)
   is null;
   --  Initialize Data before parse.
   --
   --  User should later call Reset_Post_Parse before any post_parse
   --  action.

   procedure Parse_Language_Params
     (Data   : in out Parse_Data_Type;
      Params : in     String)
   is null;
   --  If Params /= "", set all language-specific parameters from Params,
   --  in declaration order; otherwise keep default values. Boolean is
   --  represented by 0 | 1. Parameter values are space delimited.

   procedure Reset_Post_Parse
     (Data                : in out Parse_Data_Type;
      Tree                : in     WisiToken.Syntax_Trees.Tree'Class;
      Post_Parse_Action   : in     Post_Parse_Action_Type;
      Action_Region_Bytes : in     WisiToken.Buffer_Region;
      Action_Region_Chars : in     WisiToken.Buffer_Region;
      Begin_Indent        : in     Integer);
   --  Reset for a new post-parse action.

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type;
   function Action_Region_Bytes (Data : in Parse_Data_Type) return WisiToken.Buffer_Region;

   overriding
   function Copy_Augmented
     (User_Data : in Parse_Data_Type;
      Augmented : in WisiToken.Syntax_Trees.Augmented_Class_Access)
     return WisiToken.Syntax_Trees.Augmented_Class_Access;

   overriding
   procedure Initialize_Actions
     (Data : in out Parse_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class);

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out WisiToken.Syntax_Trees.Tree'Class;
      Inserted_Token : in     WisiToken.Syntax_Trees.Valid_Node_Access);

   type Navigate_Class_Type is (Motion, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Navigate_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Navigate_Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Navigate_Class;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Statement_Param_Array);
   --  Implements [2] wisi-statement-action.

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Name    : in     WisiToken.Positive_Index_Type);
   --  Implements [2] wisi-name-action.

   type Index_ID is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      ID    : WisiToken.Token_ID;
      --  If ID is not Invalid_Token_ID, it is the first token in the
      --  nonterm that Index points to that should have a navigate cache for
      --  Motion_Action to link to; an error is reported by Motion_Action if
      --  it does not.
      --
      --  If ID is Invalid_Token_ID, and the token at Index is a
      --  nonterminal, the first token in that nonterminal must have a
      --  navigate cache; an error is reported by Motion_Action if not.
   end record;

   package Index_ID_Vectors is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Index_ID);

   subtype Motion_Param_Array is Index_ID_Vectors.Vector;

   Invalid_Token_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
   --  So Create_Parser can just use "Invalid_Token_ID".

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Motion_Param_Array);
   --  Implements [2] wisi-motion-action.

   type Index_Faces is record
      Index       : WisiToken.Positive_Index_Type; -- into Tokens
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-action.

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-list-action.

   type Face_Class_Type is (Prefix, Suffix);

   type Index_Face_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Face_Class_Type;
   end record;

   type Face_Mark_Param_Array is array (Natural range <>) of Index_Face_Class;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Mark_Param_Array);
   --  Implements [2] wisi-face-mark-action.

   type Face_Remove_Param_Array is array (Natural range <>) of WisiToken.Positive_Index_Type;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Remove_Param_Array);
   --  Implements [2] wisi-face-remove-action.

   ----------
   --  Indent
   --
   --  Indent functions are represented by the Indent_Param type.

   type Simple_Indent_Param_Label is
     --  Not hanging
     (None,
      Int,
      Anchored_0, -- [2] wisi-anchored
      Anchored_1, -- [2] wisi-anchored%
      Block,      -- [2] wisi-block
      Language    -- [2] language-specific function
     );
   subtype Simple_Param_Anchored is Simple_Indent_Param_Label range Anchored_0 .. Anchored_1;

   --  Arguments to language-specific functions are integers; one of
   --  delta, Token_Number, or Token_ID - the syntax does not distinguish
   --  among these three types.

   package Indent_Arg_Arrays is new Ada.Containers.Vectors (WisiToken.Positive_Index_Type, Integer);

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector;

   type Delta_Type (<>) is private;

   type Indenting_Comment_Label is (None, Leading, Trailing);
   --  None    : indenting code
   --  Leading : comment indent from following token
   --  Trailing: comment indent from preceding token

   type Language_Indent_Function is access function
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Indent_Arg_Arrays.Vector)
     return Delta_Type;

   Null_Args : Indent_Arg_Arrays.Vector renames Indent_Arg_Arrays.Empty_Vector;

   type Simple_Indent_Param (Label : Simple_Indent_Param_Label := None) is
   record
      case Label is
      when None =>
         null;

      when Block | Int =>
         Int_Delta : Integer;

      when Simple_Param_Anchored =>
         Anchored_Index : WisiToken.Positive_Index_Type;
         Anchored_Delta : Integer;

      when Language =>
         Function_Ptr : Language_Indent_Function;
         Args         : Indent_Arg_Arrays.Vector;
      end case;
   end record;

   function Image (Item : in Simple_Indent_Param) return String;

   function Add_Simple_Indent_Param (Left, Right : in Simple_Indent_Param) return Simple_Indent_Param;

   type Indent_Param_Label is
     (Simple,
      Hanging_0, -- [2] wisi-hanging
      Hanging_1, -- [2] wisi-hanging%
      Hanging_2  -- [2] wisi-hanging*
     );
   subtype Hanging_Label is Indent_Param_Label range Hanging_0 .. Hanging_2;

   type Indent_Param (Label : Indent_Param_Label := Simple) is
   record
      case Label is
      when Simple =>
         Param : Simple_Indent_Param;

      when Hanging_Label =>
         Hanging_Delta_1 : Simple_Indent_Param;
         Hanging_Delta_2 : Simple_Indent_Param;

      end case;
   end record;

   function Image (Item : in Indent_Param) return String;

   type Indent_Pair (Comment_Present : Boolean := False) is
   record
      Code_Delta : Indent_Param;
      case Comment_Present is
      when True =>
         Comment_Delta : Indent_Param;
      when False =>
         null;
      end case;
   end record;

   function Image (Item : in Indent_Pair) return String;

   type Indent_Param_Array is array (WisiToken.Positive_Index_Type range <>) of Indent_Pair;

   type Indenting is record
      Code : WisiToken.Line_Region := WisiToken.Null_Line_Region;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token. Includes blank and comment lines between
      --  grammar tokens, but excludes trailing blanks and comments after the
      --  last token, so they can be indented differently.

      Comment : WisiToken.Line_Region := WisiToken.Null_Line_Region;
      --  Trailing comment or blank lines (after the last contained grammar
      --  token). Excludes comment following code on a line.
   end record;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action.

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Token   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Label             : in     Hanging_Label)
     return Delta_Type;
   --  Implements [2] wisi-hanging, wisi-hanging%, wisi-hanging*
   --
   --  Language specific child packages may override this to implement
   --  language-specific cases.

   ----------
   --  Other

   type Refactor_Action is range 0 .. Integer'Last;

   function Refactor_Parse  (Data : in Parse_Data_Type; Item : in String) return Refactor_Action;

   procedure Refactor_Help (Data : in Parse_Data_Type) is null;

   procedure Refactor
     (Data       : in out Parse_Data_Type;
      Tree       : in out WisiToken.Syntax_Trees.Tree;
      Action     : in     Refactor_Action;
      Edit_Begin : in     WisiToken.Buffer_Pos) is null;

   type Query_Label is (Node, Containing_Statement, Ancestor, Parent, Child, Print, Dump);
   --  Must match wisi-parse-common.el wisi-parse-tree-queries

   subtype Point_Query is Query_Label range Node .. Ancestor;
   subtype Node_Query is Query_Label range Parent .. Child;

   type Query (Label : Query_Label) is
   record
      case Label is
      when Point_Query =>
         Char_Point : WisiToken.Buffer_Pos;

         case Label is
         when Ancestor =>
            IDs : WisiToken.Token_ID_Arrays.Vector;
         when others =>
            null;
         end case;

      when Parent | Child =>
         Node : WisiToken.Syntax_Trees.Node_Access;
         N    : Positive;

      when Print =>
         null;

      when Dump =>
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Address_Image (Item : in WisiToken.Syntax_Trees.Valid_Node_Access) return String;
   --  Hexadecimal address of Item, for Query_Tree.

   function To_Node_Access (Item : in String) return WisiToken.Syntax_Trees.Valid_Node_Access;

   function Get_Token_IDs
     (User_Data    : in     Parse_Data_Type;
      Command_Line : in     String;
      Last         : in out Integer)
     return WisiToken.Token_ID_Arrays.Vector
   is abstract;
   --  Read an aggregate of Token_Enum_IDs from Command_Line.
   --
   --  Dispatching on User_Data because Token_Enum_IDs is
   --  language-specific.

   procedure Query_Tree
     (Data  : in Parse_Data_Access_Constant;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Query : in Wisi.Query);

   type Arg_Index_Array is array (Positive range <>) of WisiToken.Positive_Index_Type;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String);
   --  Send a Language_Action message to Emacs.

   procedure Put (Data : in out Parse_Data_Type; Parser : in WisiToken.Parse.Base_Parser'Class);
   --  Perform additional post-parse actions, then put result to
   --  Ada.Text_IO.Current_Output, as encoded responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put_Errors (Tree : in WisiToken.Syntax_Trees.Tree);
   --  Put errors in Tree to Ada.Text_IO.Current_Output,
   --  as encoded error and recover responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put_Error
     (Tree        : in WisiToken.Syntax_Trees.Tree;
      Line_Number : in WisiToken.Line_Number_Type;
      Message     : in String);
   --  Put an error elisp form to Ada.Text_IO.Current_Output.

   function Integer_Filled_Image is new SAL.Generic_Decimal_Image (Integer);

private

   type Augmented is new WisiToken.Syntax_Trees.Base_Augmented with
   record
      Cache_Version : WisiToken.Cache_Version := WisiToken.Cache_Version'First;

      Indenting : Wisi.Indenting;
      --  Computed on demand; see Compute_Indenting.
   end record;
   type Augmented_Access is access all Augmented;

   function Get_Augmented
     (Tree : in WisiToken.Syntax_Trees.Tree'Class;
      Node : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Augmented_Access;
   --  Return Node.Augmented. If that is null, set it to the default
   --  Augmented first.

   type Nil_Buffer_Pos (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : WisiToken.Buffer_Pos;
      when False =>
         null;
      end case;
   end record;

   Nil : constant Nil_Buffer_Pos := (Set => False);

   function Image (Item : in Nil_Buffer_Pos) return String
   is (if Item.Set then Item.Item'Image else " nil");

   type Navigate_Cache_Type is record
      Pos : WisiToken.Buffer_Pos;
      --  Implicit in [1] wisi-cache. This is a character position in the
      --  source text; it must be on a Source_Terminal (not a virtual terminal).

      Statement_ID   : WisiToken.Token_ID;   -- [1] wisi-cache-nonterm
      ID             : WisiToken.Token_ID;   -- [1] wisi-cache-token
      Length         : Natural;              -- [1] wisi-cache-last
      Class          : Navigate_Class_Type;  -- [1] wisi-cache-class
      Containing_Pos : Nil_Buffer_Pos;       -- [1] wisi-cache-containing
      Prev_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-prev
      Next_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-next
      End_Pos        : Nil_Buffer_Pos;       -- [1] wisi-cache-end
   end record;

   function Key (Cache : in Navigate_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Pos);

   function Key_Compare (Left, Right : in WisiToken.Buffer_Pos) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left = Right then SAL.Equal
      else SAL.Less);

   package Navigate_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Navigate_Cache_Type, WisiToken.Buffer_Pos);

   function Key (Cache : in WisiToken.Buffer_Region) return WisiToken.Buffer_Pos is (Cache.First);

   package Name_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (WisiToken.Buffer_Region, WisiToken.Buffer_Pos);
   --  Character positions of names.

   type Nil_Integer (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : Integer;
      when False =>
         null;
      end case;
   end record;

   type Face_Cache_Type is record
      Char_Region : WisiToken.Buffer_Region;
      Class       : Face_Class_Type;
      Face        : Nil_Integer; -- not set, or index into *-process-faces-names
   end record;

   function Key (Cache : in Face_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Char_Region.First);

   package Face_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Face_Cache_Type, WisiToken.Buffer_Pos);

   type Indent_Label is (Not_Set, Int, Anchored);

   type Indent_Type (Label : Indent_Label := Not_Set) is record
      --  Indent values may be negative while indents are being computed.

      Controlling_Token_Line : WisiToken.Base_Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  See [2] Indent actions for description of controlling token.

      case Label is
      when Not_Set =>
         null;

      when Int =>
         Int_Indent : Integer;

      when Anchored =>
         Anchor_Line  : WisiToken.Line_Number_Type;
         Anchor_Delta : Integer;
      end case;
   end record;

   package Indent_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, Indent_Type, Default_Element => (others => <>));
   package Navigate_Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Navigate_Cache_Trees.Cursor, Navigate_Cache_Trees."=");

   type Parse_Data_Type is abstract new WisiToken.Syntax_Trees.User_Data_Type with
   record
      --  Aux token info
      First_Comment_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Last_Comment_ID  : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Left_Paren_ID    : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Right_Paren_ID   : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;

      Statement_IDs : WisiToken.Token_ID_Arrays.Vector;
      --  Nonterms returned by containing_statement query.

      --  Data for post-parse actions

      Post_Parse_Action   : Post_Parse_Action_Type;
      Action_Region_Bytes : WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region;
      Action_Region_Chars : WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region;
      Action_Region_Lines : WisiToken.Line_Region   := WisiToken.Null_Line_Region;
      --  Actions are applied to tokens that overlap this region.

      Navigate_Caches   : Navigate_Cache_Trees.Tree;  -- Set by Navigate.
      Name_Caches       : Name_Cache_Trees.Tree;      -- Set by Navigate.
      End_Positions     : Navigate_Cursor_Lists.List; -- Dynamic data for Navigate.
      Face_Caches       : Face_Cache_Trees.Tree;      -- Set by Face.
      Indents           : Indent_Vectors.Vector;      -- Set by Indent.
      Begin_Indent      : Integer;                    -- Indentation of line at start of parse.

      --  Copied from language-specific parameters
      Indent_Comment_Col_0 : Boolean := False;

      Augmented_Cache_Version : WisiToken.Cache_Version := WisiToken.Cache_Version'First + 1;
   end record;

   type Simple_Delta_Labels is (None, Int, Anchored);

   type Simple_Delta_Type (Label : Simple_Delta_Labels := None) is
   record
      Controlling_Token_Line : WisiToken.Base_Line_Number_Type;
      --  If Invalid_Line_Number, delta should not be ignored.

      case Label is
      when None =>
         null;

      when Int =>
         Int_Delta : Integer;

      when Anchored =>
         Anchor_Line    : WisiToken.Line_Number_Type;
         Anchored_Delta : Integer;

      end case;
   end record;

   function Image (Item : in Simple_Delta_Type) return String;
   --  For debugging

   type Delta_Labels is (Simple, Hanging);

   type Delta_Type (Label : Delta_Labels := Simple) is
   record
      case Label is
      when Simple =>
         Simple_Delta : Simple_Delta_Type;

      when Hanging =>
         Hanging_First_Line : WisiToken.Line_Number_Type;

         Hanging_Delta_1 : Simple_Delta_Type;
         --  Indentation of first line in token; Null_Delta if first line does
         --  not need indenting

         Hanging_Delta_2 : Simple_Delta_Type; -- indentation of continuation lines
      end case;
   end record;

   Null_Delta : constant Delta_Type := (Simple, (None, WisiToken.Invalid_Line_Number));

   function Image (Item : in Delta_Type) return String;
   --  For debugging

   ----------
   --  Utilities for language-specific child packages

   Emacs_Lisp_New_Line : constant String := "\n";
   --  For includinge New_Line in a text string sent to Emacs.

   function Compute_Indenting
     (Data : in Parse_Data_Type'Class;
      Tree : in WisiToken.Syntax_Trees.Tree;
      Node : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Wisi.Indenting
   with Pre => Tree.Line_Region (Node, Trailing_Non_Grammar => False) /= WisiToken.Null_Line_Region and
               Tree.SOI /= Node and Tree.EOI /= Node;
   --  Return Node.Augmented.Indenting, computing it first if needed.

   function Current_Indent_Offset
     (Tree         : in WisiToken.Syntax_Trees.Tree'Class;
      Anchor_Token : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Offset       : in Integer)
     return Integer;
   --  Return offset from beginning of first token on line containing
   --  Anchor_Token, to beginning of Anchor_Token, plus Offset.

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return String;
   --  Return text contained by Tree_Index token in source file
   --  (lexer.buffer).

   function Indent_Anchored_2
     (Data              : in Parse_Data_Type'Class;
      Tree              : in WisiToken.Syntax_Trees.Tree;
      Anchor_Token      : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Token   : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in Boolean;
      Offset            : in Integer)
     return Delta_Type;
   --  If Anchor_Token.Line = Indenting_Token.Line, return Null_Delta. Otherwise
   --  return an anchored delta using Anchor_Token.Line, Offset.

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Param             : in     Indent_Param;
      Indenting_Token   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean)
     return Delta_Type;
   --  Return indent defined by Param for Tree_Indenting in Nonterm.

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Line_Region       : in     WisiToken.Line_Region;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Indenting_Comment_Label;
      Controlling_Delta : in     Delta_Type := Null_Delta);
   --  Apply Delta_Indent to lines in Line_Region.
   --
   --  Controlling_Delta should be Null_Delta if Indenting_Comment is
   --  None; it should be any existing indent for
   --  Controlling_Token.Line_Region.[First | Last] if Indenting_Comment
   --  is Leading | Trailing. This allows adding previously computed
   --  indents for the token controlling a comment line to the comment
   --  line indent.
   --
   --  Sets Data.Indents, so caller may not be in a renames for a
   --  Data.Indents element.

   --  Visible for language-specific children. Must match list in
   --  [3] wisi-process-parse--execute.
   Navigate_Cache_Code        : constant String := "1";
   Face_Property_Code         : constant String := "2";
   Indent_Code                : constant String := "3";
   Lexer_Error_Code           : constant String := "4";
   Parser_Error_Code          : constant String := "5";
   In_Parse_Action_Error_Code : constant String := "6";
   Recover_Code               : constant String := "7 ";
   End_Code                   : constant String := "8";
   Name_Property_Code         : constant String := "9";
   Edit_Action_Code           : constant String := "10";
   Language_Action_Code       : constant String := "11 "; -- used by wisitoken_grammar for Check_Parens
   Query_Tree_Code            : constant String := "12";
end Wisi;
