--  Abstract :
--
--  See spec.
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

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.BNF.Generate_Grammar;
with WisiToken.BNF.Utils;
with WisiToken.Generate; use WisiToken.Generate;
with WisiToken.Parse.LR;
with WisiToken.Productions;
package body WisiToken.BNF.Output_Ada_Common is

   --  Body subprograms, alphabetical

   function Duplicate_Reduce (State : in Parse.LR.Parse_State) return Boolean
   is
      use Parse.LR;
      Action_Node : Parse_Action_Node_Ptr;
      First       : Boolean := True;
      Action      : Reduce_Action_Rec;
   begin
      for Node of State.Action_List loop
         Action_Node := Node.Actions;
         if Action_Node.Next /= null then
            --  conflict
            return False;
         elsif Action_Node.Item.Verb /= Reduce then
            return False;
         end if;

         if First then
            Action    := Action_Node.Item;
            First     := False;
         else
            if not Equal (Action, Action_Node.Item) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Duplicate_Reduce;

   function Image (Item : in Boolean) return String
   is (if Item then "True" else "False");

   function Symbols_Image (State : in Parse.LR.Parse_State) return String
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Strings.Unbounded;

      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      if State.Action_List.Length = 1 then
         return "(1 => " & Token_ID'Image (State.Action_List (1).Symbol) & ")";
      else
         Result := +"(";
         for Node of State.Action_List loop
            Result := Result &
              (if Need_Comma then ", " else "") &
              Trimmed_Image (Node.Symbol);
            Need_Comma := True;
         end loop;
         Result := Result & ")";
         return -Result;
      end if;
   end Symbols_Image;

   ----------
   --  Public subprograms in alphabetical order

   procedure Create_Ada_Actions_Spec
     (Output_File_Name :         in String;
      Package_Name     :         in String;
      Input_Data       :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data      :         in Output_Ada_Common.Common_Data;
      Generate_Data    : aliased in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Generate_Utils;

      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;
      Spec_File  : File_Type;
      Paren_Done : Boolean      := False;
      Cursor     : Token_Cursor := First (Generate_Data);
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      if not (Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0) then
         Put_Line ("with WisiToken;");
      end if;
      if Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0 then
         Put_Line ("with WisiToken.Syntax_Trees;");
      end if;
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Pre));

      Indent_Line ("Descriptor : aliased constant WisiToken.Descriptor :=");
      Indent_Line ("  (First_Terminal    =>" & WisiToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal       =>" & WisiToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal   =>" & WisiToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal    =>" & WisiToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("SOI_ID              =>" & WisiToken.Token_ID'Image (Descriptor.SOI_ID) & ",");
      Indent_Line ("EOI_ID              =>" & WisiToken.Token_ID'Image (Descriptor.EOI_ID) & ",");
      Indent_Line ("Accept_ID           =>" & WisiToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Case_Insensitive    => " & Image (Input_Data.Language_Params.Case_Insensitive) & ",");
      Indent_Line ("New_Line_ID         =>" & WisiToken.Token_ID'Image (Descriptor.New_Line_ID) & ",");
      Indent_Line ("String_1_ID         =>" & WisiToken.Token_ID'Image (Descriptor.String_1_ID) & ",");
      Indent_Line ("String_2_ID         =>" & WisiToken.Token_ID'Image (Descriptor.String_2_ID) & ",");
      Indent_Line ("Image               =>");
      Indent_Start ("  (");
      Indent := Indent + 3;
      loop
         exit when Is_Done (Cursor);
         if Paren_Done then
            Indent_Start ("new String'(""" & (Name (Generate_Data, Cursor)));
         else
            Put ("new String'(""" & (Name (Generate_Data, Cursor)));
            Paren_Done := True;
         end if;
         Next (Generate_Data, Cursor);
         if Is_Done (Cursor) then
            Put_Line (""")),");
         else
            Put_Line ("""),");
         end if;
      end loop;

      Indent := Indent - 3;
      Indent_Line ("Terminal_Image_Width =>" & Integer'Image (Descriptor.Terminal_Image_Width) & ",");
      Indent_Line ("Image_Width          =>" & Integer'Image (Descriptor.Image_Width) & ",");
      Indent_Line ("Last_Lookahead       =>" & WisiToken.Token_ID'Image (Descriptor.Last_Lookahead) & ");");
      Indent := Indent - 3;
      New_Line;

      if Input_Data.Language_Params.Declare_Enums then
         Paren_Done := False;

         Cursor := First (Generate_Data);
         Indent_Line ("type Token_Enum_ID is");
         Indent_Start ("  (");
         Indent := Indent + 3;
         loop
            exit when Is_Done (Cursor);
            if Paren_Done then
               Indent_Start (To_Token_Ada_Name (Name (Generate_Data, Cursor)));
            else
               Put (To_Token_Ada_Name (Name (Generate_Data, Cursor)));
               Paren_Done := True;
            end if;
            Next (Generate_Data, Cursor);
            if Is_Done (Cursor) then
               Put_Line (");");
            else
               Put_Line (",");
            end if;
         end loop;

         Indent := Indent - 3;
         New_Line;

         Indent_Line ("type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;");
         Indent_Line ("use all type WisiToken.Token_ID;");
         Indent_Line ("function ""+"" (Item : in Token_Enum_ID) return WisiToken.Token_ID");
         Indent_Line ("  is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));");

         Indent_Line ("function To_Token_Enum (Item : in WisiToken.Token_ID) return Token_Enum_ID");
         Indent_Line ("  is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));");
         Indent_Line ("function ""-"" (Item : in WisiToken.Token_ID) return Token_Enum_ID renames To_Token_Enum;");
         New_Line;

      end if;

      for Name_List of Generate_Data.Action_Names.all loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("procedure " & Name.all);
                  Indent_Line ("  (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
                  Indent_Line ("   Tree      : in out WisiToken.Syntax_Trees.Tree;");
                  Indent_Line ("   Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);");
               end if;
            end loop;
         end if;
      end loop;

      for Name_List of Generate_Data.Check_Names.all loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("function " & Name.all);
                  Indent_Line (" (Tree           : in     WisiToken.Syntax_Trees.Tree;");
                  Indent_Line ("  Nonterm        : in out WisiToken.Syntax_Trees.Recover_Token;");
                  Indent_Line ("  Tokens         : in     WisiToken.Syntax_Trees.Recover_Token_Array;");
                  Indent_Line ("  Recover_Active : in     Boolean)");
                  Indent_Line (" return WisiToken.Syntax_Trees.In_Parse_Actions.Status;");
               end if;
            end loop;
         end if;
      end loop;

      Indent_Line ("Partial_Parse_Active    : aliased Boolean := False;");
      Indent_Line ("Partial_Parse_Byte_Goal : aliased WisiToken.Buffer_Pos := WisiToken.Buffer_Pos'Last;");

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Actions_Spec;

   procedure Create_Ada_Main_Spec
     (Output_File_Name  : in String;
      Main_Package_Name : in String;
      Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data       : in Output_Ada_Common.Common_Data)
   is
      Lower_Package_Name : constant String := To_Lower (Main_Package_Name);

      Spec_File : File_Type;

      procedure LR_Process
      is begin
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Trace      : in WisiToken.Trace_Access;");
         Indent_Start   ("   User_Data  : in WisiToken.Syntax_Trees.User_Data_Access");
         if Input_Data.Language_Params.Error_Recover then
            Put_Line (";");
            Indent_Line ("   Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;");
            Indent_Line
              ("   Language_Matching_Begin_Tokens : in " &
                 "WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;");
            Indent_Start
              ("   Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access");
         end if;
         if Common_Data.Text_Rep then
            Put_Line (";");
            Indent_Start ("   Text_Rep_File_Name : in String");
         end if;
         Put_Line (")");
         Indent_Line
           ("  return WisiToken.Parse.LR.Parser"  &
              (if Input_Data.Language_Params.Error_Recover
               then ""
               else "_No_Recover") &
              ".Parser;");
         New_Line;
      end LR_Process;

      procedure Packrat_Procedural_Process
      is begin
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Trace      : in WisiToken.Trace_Access;");
         Indent_Line ("   User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)");
         Indent_Line ("  return WisiToken.Parse.Packrat.Procedural.Parser;");
         New_Line;
      end Packrat_Procedural_Process;

      procedure Packrat_Generated_Process
      is begin
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Trace      : in WisiToken.Trace_Access;");
         Indent_Line ("   User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)");
         Indent_Line ("  return WisiToken.Parse.Packrat.Generated.Parser;");
         New_Line;
      end Packrat_Generated_Process;

   begin
      if Common_Data.Generate_Algorithm = External then
         raise SAL.Programmer_Error;
      end if;

      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Put_Line ("with WisiToken.Syntax_Trees;");

      case Common_Data.Generate_Algorithm is
      when LR_Generate_Algorithm =>
         Put_Line
           ("with WisiToken.Parse.LR.Parser"  &
              (if Input_Data.Language_Params.Error_Recover
               then ""
               else "_No_Recover") &
              ";");

      when Packrat_Proc =>
         Put_Line ("with WisiToken.Parse.Packrat.Procedural;");

      when Packrat_Gen =>
         Put_Line ("with WisiToken.Parse.Packrat.Generated;");

      when External | Tree_Sitter =>
         null;
      end case;

      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Common_Data.Output_Language is
      when Ada_Lang =>
         case Common_Data.Generate_Algorithm is
         when LR_Generate_Algorithm =>
            LR_Process;
         when Packrat_Proc =>
            Packrat_Procedural_Process;
         when Packrat_Gen =>
            Packrat_Generated_Process;
         when External | Tree_Sitter =>
            null;
         end case;

      when Ada_Emacs_Lang =>
         case Common_Data.Interface_Kind is
         when Process =>
            case Common_Data.Generate_Algorithm is
            when LR_Generate_Algorithm =>
               LR_Process;
            when Packrat_Proc =>
               Packrat_Procedural_Process;
            when Packrat_Gen =>
               Packrat_Generated_Process;
            when External | Tree_Sitter =>
               null;
            end case;

         when Module =>
            Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
            Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name & "_wisi_module_parse"");");
            Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
            Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name & "_wisi_module_parse_init"");");
            New_Line;

         end case;
      end case;

      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);
   end Create_Ada_Main_Spec;

   procedure Create_External_Main_Spec
     (Main_Package_Name    : in String;
      Tuple                : in Generate_Tuple;
      Input_Data           : in WisiToken_Grammar_Runtime.User_Data_Type)
   is
      File_Name : constant String := To_Lower (Main_Package_Name) & ".ads";
      Spec_File : File_Type;
   begin
      Create (Spec_File, Out_File, File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Put_Line ("with WisiToken.Productions;");
      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("function Create_Grammar return WisiToken.Productions.Prod_Arrays.Vector;");

      Indent := Indent - 3;
      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);
   end Create_External_Main_Spec;

   procedure Create_LR_Parser_Core_1
     (Input_Data    : in WisiToken_Grammar_Runtime.User_Data_Type;
      Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Ada.Strings.Unbounded;

      Table : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Line  : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Append (Line, Item);
      end Append;

      procedure Put (Label : in String; Item : in Token_ID_Array_Natural)
      is begin
         Indent_Line (Label & " =>");
         Indent_Start ("  (");
         Indent := Indent + 3;
         Line := +"";
         for I in Item'Range loop
            Append (Trimmed_Image (Item (I)));

            if I = Item'Last then
               Append ("),");

            else
               Append (", ");
            end if;
         end loop;
         Indent_Wrap (-Line);
         Indent := Indent - 3;
      end Put;

   begin
      if Input_Data.Language_Params.Error_Recover then
         Indent_Line ("McKenzie_Param : constant McKenzie_Param_Type :=");
         Indent_Line ("  (First_Terminal    =>" & Token_ID'Image (Table.McKenzie_Param.First_Terminal) & ",");
         Indent := Indent + 3;
         Indent_Line ("Last_Terminal     =>" & Token_ID'Image (Table.McKenzie_Param.Last_Terminal) & ",");
         Indent_Line ("First_Nonterminal =>" & Token_ID'Image (Table.McKenzie_Param.First_Nonterminal) & ",");
         Indent_Line ("Last_Nonterminal  =>" & Token_ID'Image (Table.McKenzie_Param.Last_Nonterminal) & ",");
         Put ("Insert", Table.McKenzie_Param.Insert);
         Put ("Delete", Table.McKenzie_Param.Delete);
         Put ("Push_Back", Table.McKenzie_Param.Push_Back);
         Put ("Undo_Reduce", Table.McKenzie_Param.Undo_Reduce);
         Indent_Line
           ("Minimal_Complete_Cost_Delta => " & Integer'Image (Table.McKenzie_Param.Minimal_Complete_Cost_Delta) & ",");
         Indent_Line ("Fast_Forward => " & Integer'Image (Table.McKenzie_Param.Fast_Forward) & ",");
         Indent_Line ("Matching_Begin => " & Integer'Image (Table.McKenzie_Param.Matching_Begin) & ",");
         Indent_Line ("Ignore_Check_Fail  =>" & Integer'Image (Table.McKenzie_Param.Ignore_Check_Fail) & ",");
         Indent_Line ("Check_Limit =>" & Table.McKenzie_Param.Check_Limit'Image & ",");
         Indent_Line ("Zombie_Limit =>" & Table.McKenzie_Param.Zombie_Limit'Image & ",");
         Indent_Line ("Check_Delta_Limit =>" & Integer'Image (Table.McKenzie_Param.Check_Delta_Limit) & ",");
         Indent_Line ("Enqueue_Limit =>" & Integer'Image (Table.McKenzie_Param.Enqueue_Limit) & ");");
         Indent := Indent - 3;
         New_Line;
      end if;
   end Create_LR_Parser_Core_1;

   procedure Create_LR_Parser_Table
     (Input_Data    : in WisiToken_Grammar_Runtime.User_Data_Type;
      Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use all type Ada.Containers.Count_Type;
      use WisiToken.Parse.LR;
      use Ada.Strings.Unbounded;

      --  Optimize source structure for GNAT compile time; one subroutine
      --  with thousands of "Table.States (*) := ..." takes forever to
      --  compile (apparently depending on available memory). But hundreds
      --  of subroutines, containing the same lines in chunks of
      --  Lines_Per_Subr, compiles in acceptable time.

      Table            : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Lines_Per_Subr   : constant := 500;
      Subr_Count       : Integer  := 1;
      Last_Subr_Closed : Boolean  := False;
      Line             : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Line := Line & Item;
      end Append;
   begin
      Indent_Line ("declare");
      Indent := Indent + 3;

      Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
      Indent_Line ("is begin");
      Indent     := Indent + 3;
      Line_Count := 0;

      Declare_Subroutines :
      for State_Index in Table.States'Range loop
         Actions :
         declare
            use Ada.Containers;
            Base_Indent : constant Ada.Text_IO.Count := Indent;
         begin
            Indent_Line
              ("Table.States (" & Trimmed_Image (State_Index) & ").Action_List.Set_Capacity (" &
                 Trimmed_Image (Table.States (State_Index).Action_List.Length) & ");");

            if Duplicate_Reduce (Table.States (State_Index)) then
               if Table.States (State_Index).Action_List.Length > 0 then
                  --  We only get here with Length = 0 when there's a bug in LALR_Generate.
                  declare
                     Node   : Action_Node renames Table.States (State_Index).Action_List (1);
                     Action : constant Reduce_Action_Rec := Node.Actions.Item;
                  begin
                     Set_Col (Indent);
                     Indent_Wrap
                       ("Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Symbols_Image (Table.States (State_Index)) & ", " &
                          Image (Action.Production) & ", " &
                          Count_Type'Image (Action.Token_Count) & ");");
                     Line_Count := Line_Count + 1;
                     Indent     := Base_Indent;
                  end;
               end if;

            else
               for Node of Table.States (State_Index).Action_List loop
                  Set_Col (Indent);
                  declare
                     Action_Node : Parse_Action_Node_Ptr := Node.Actions;
                  begin
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Trimmed_Image (Node.Symbol) & ", ";
                        Append (Image (Action_Node.Item.Production) & ", ");
                        Append (Trimmed_Image (Action_Node.Item.State));
                        Append (");");

                     when Reduce | Accept_It =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Trimmed_Image (Node.Symbol);
                        if Action_Node.Item.Verb = Reduce then
                           Append (", Reduce");
                        else
                           Append (", Accept_It");
                        end if;
                        Append (", ");
                        Append (Image (Action_Node.Item.Production) & ", ");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ");");

                     when Parse.LR.Error =>
                        raise SAL.Programmer_Error;
                     end case;
                     Indent_Wrap (-Line);
                     Line_Count := Line_Count + 1;

                     loop
                        Action_Node := Action_Node.Next;
                        exit when Action_Node = null;
                        --  There is a conflict; must be Shift/{Reduce|Accept} or Reduce/{Reduce|Accept}.
                        --  The added parameters are the same in either case.
                        case Action_Node.Item.Verb is
                        when Reduce | Accept_It =>
                           Line := +"Add_Conflict (Table.States (" & Trimmed_Image (State_Index) & "), " &
                             Trimmed_Image (Node.Symbol) & ", ";
                           Append (Image (Action_Node.Item.Production) & ", ");
                           Append (Count_Type'Image (Action_Node.Item.Token_Count) & ");");
                           Indent_Wrap (-Line);
                           Line_Count := Line_Count + 1;

                        when others =>
                           raise SAL.Programmer_Error with "invalid conflict action verb: " &
                             Parse.LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                        end case;
                     end loop;
                  end;
                  Indent := Base_Indent;
               end loop;
            end if;
         end Actions;

         if Table.States (State_Index).Goto_List.Length > 0 then
            Indent_Line
              ("Table.States (" & Trimmed_Image (State_Index) & ").Goto_List.Set_Capacity (" &
                 Trimmed_Image (Table.States (State_Index).Goto_List.Length) & ");");
         end if;
         Gotos :
         for Node of Table.States (State_Index).Goto_List loop
            Set_Col (Indent);
            Put ("Add_Goto (Table.States (" & Trimmed_Image (State_Index) & "), ");
            Put_Line (Trimmed_Image (Node.Symbol) & ", " & Trimmed_Image (Node.State) & ");");
            Line_Count := Line_Count + 1;
         end loop Gotos;

         if Input_Data.Language_Params.Error_Recover then
            if Table.States (State_Index).Kernel.Length > 0 then
               Indent_Wrap
                 ("Table.States (" & Trimmed_Image (State_Index) & ").Kernel := To_Vector (" &
                    Image (Table.States (State_Index).Kernel, Strict => True) & ");");
            end if;
            if Table.States (State_Index).Minimal_Complete_Actions.Length > 0 then
               Indent_Wrap
                 ("Table.States (" & Trimmed_Image (State_Index) & ").Minimal_Complete_Actions := To_Vector (" &
                    Strict_Image (Table.States (State_Index).Minimal_Complete_Actions, Strict => True) & ");");
            end if;
         end if;

         if Line_Count > Lines_Per_Subr then
            Line_Count := 0;
            Indent := Indent - 3;
            Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");

            if State_Index < Table.States'Last then
               Subr_Count := Subr_Count + 1;
               Last_Subr_Closed := False;
               Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
               Indent_Line ("is begin");
               Indent := Indent + 3;
            else
               Last_Subr_Closed := True;
            end if;
         end if;

      end loop Declare_Subroutines;

      if not Last_Subr_Closed then
         Indent := Indent - 3;
         Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");
      end if;

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      for Subr in 1 .. Subr_Count loop
         Indent_Line ("Subr_" & Trimmed_Image (Subr) & ";");
      end loop;
      Indent_Line ("Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);");
      Indent := Indent - 3;
      Indent_Line ("end;");
   end Create_LR_Parser_Table;

   procedure LR_Create_Create_Parse_Table
     (Input_Data    :         in     WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      Table : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
   begin
      Indent_Line ("function Create_Parse_Table");

      if Common_Data.Text_Rep then
         Indent_Line (" (Text_Rep_File_Name : in String)");
      end if;
      Indent_Line ("  return WisiToken.Parse.LR.Parse_Table_Ptr");

      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use WisiToken.Parse.LR;");

      Create_LR_Parser_Core_1 (Input_Data, Generate_Data);

      if Common_Data.Text_Rep then
         Indent_Line ("Table : constant Parse_Table_Ptr := Get_Text_Rep (Text_Rep_File_Name);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;

      else
         Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
         Indent_Line ("  (State_First       => 0,");
         Indent := Indent + 3;
         Indent_Line ("State_Last        =>" & State_Index'Image (Table.State_Last) & ",");
         Indent_Line ("First_Terminal    =>" & Token_ID'Image (Table.First_Terminal) & ",");
         Indent_Line ("Last_Terminal     =>" & Token_ID'Image (Table.Last_Terminal) & ",");
         Indent_Line ("First_Nonterminal =>" & Token_ID'Image (Table.First_Nonterminal) & ",");
         Indent_Line ("Last_Nonterminal  =>" & Token_ID'Image (Table.Last_Nonterminal) & ");");
         Indent := Indent - 3;

         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_LR_Parser_Table (Input_Data, Generate_Data);
         New_Line;
      end if;

      if Input_Data.Language_Params.Error_Recover then
         Indent_Line ("Table.Error_Recover_Enabled := True;");
         Indent_Line ("Table.McKenzie_Param := McKenzie_Param;");
      end if;
      Indent_Line ("Table.Max_Parallel :=" & Table.Max_Parallel'Image & ";");

      Indent_Line ("return Table;");

      Indent := Indent - 3;
      Indent_Line ("end Create_Parse_Table;");
      New_Line;
   end LR_Create_Create_Parse_Table;

   procedure LR_Create_Create_Parser
     (Actions_Package_Name :         in     String;
      Common_Data          :         in out Output_Ada_Common.Common_Data;
      Generate_Data        : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      Parser_Type : constant String :=
        "WisiToken.Parse.LR.Parser" &
        (if Generate_Data.LR_Parse_Table.Error_Recover_Enabled
         then ""
         else "_No_Recover") &
        ".Parser";
   begin
      Indent_Line ("function Create_Parser");
      Indent_Line    ("  (Trace      : in WisiToken.Trace_Access;");
      Indent_Start   ("   User_Data  : in WisiToken.Syntax_Trees.User_Data_Access");
      if Generate_Data.LR_Parse_Table.Error_Recover_Enabled then
         Put_Line (";");
         Indent_Line
           ("   Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;");
         Indent_Line
           ("   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;");
         Indent_Start
           ("   Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access");
      end if;
      if Common_Data.Text_Rep then
         Put_Line (";");
         Indent_Start ("   Text_Rep_File_Name : in String");
      end if;
      Put_Line (")");
      Indent_Line ("  return " & Parser_Type);

      Indent_Line ("is begin");
      Indent := Indent + 3;
      Indent_Line ("return Parser : " & Parser_Type & " do");
      Indent := Indent + 3;
      Indent_Line ("Parser.Tree.Lexer := Lexer.New_Lexer (Trace, " & Actions_Package_Name & ".Descriptor'Access);");
      Indent_Line ("Parser.Productions := Create_Productions;");
      Indent_Line ("Parser.User_Data := User_Data;");
      if Generate_Data.LR_Parse_Table.Error_Recover_Enabled then
         Indent_Line ("Parser.Partial_Parse_Active := " & Actions_Package_Name & ".Partial_Parse_Active'Access;");
         Indent_Line ("Parser.Partial_Parse_Byte_Goal := " & Actions_Package_Name & ".Partial_Parse_Byte_Goal'Access;");
      end if;

      if Common_Data.Text_Rep then
         Indent_Line ("Parser.Table := Create_Parse_Table (Text_Rep_File_Name);");
      else
         Indent_Line ("Parser.Table := Create_Parse_Table;");
      end if;
      if Generate_Data.LR_Parse_Table.Error_Recover_Enabled then
         Indent_Line ("Parser.Language_Fixes                 := Language_Fixes;");
         Indent_Line ("Parser.Language_Matching_Begin_Tokens := Language_Matching_Begin_Tokens;");
         Indent_Line ("Parser.Language_String_ID_Set         := Language_String_ID_Set;");
      end if;
      Indent := Indent - 3;
      Indent_Line ("end return;");
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
   end LR_Create_Create_Parser;

   procedure Packrat_Create_Create_Parser
     (Actions_Package_Name :         in     String;
      Common_Data          :         in out Output_Ada_Common.Common_Data;
      Generate_Data        : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data;
      Packrat_Data         :         in     WisiToken.Generate.Packrat.Data)
   is
      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

      procedure Put_Direct_Left_Recursive
      is
         use Ada.Strings.Unbounded;
         Text     : Unbounded_String;
         Need_Bar : Boolean := False;
      begin
         Indent_Line
           ("Direct_Left_Recursive : constant WisiToken.Token_ID_Set (" &
              Trimmed_Image (Generate_Data.Grammar.First_Index) & " .. " &
              Trimmed_Image (Generate_Data.Grammar.Last_Index) & ") :=");

         if Any (Packrat_Data.Direct_Left_Recursive) then
            for I in Packrat_Data.Direct_Left_Recursive'Range loop
               if Packrat_Data.Direct_Left_Recursive (I) then
                  if Need_Bar then
                     Text := Text & " | ";
                  else
                     Need_Bar := True;
                  end if;
                  Text := Text & Trimmed_Image (I);
               end if;
            end loop;
            Indent_Start ("  (");
            Indent := Indent + 3;
            Indent_Wrap (-Text & " => True,");
            Indent_Line ("others => False);");
            Indent := Indent - 3;
         else
            Indent_Line ("  (others => False);");
         end if;
      end Put_Direct_Left_Recursive;

   begin
      Indent_Line ("function Create_Parser");
      Indent_Line ("  (Trace      : in WisiToken.Trace_Access;");
      Indent_Line ("   User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)");

      case Packrat_Generate_Algorithm'(Common_Data.Generate_Algorithm) is
      when Packrat_Gen =>
         Indent_Line ("  return WisiToken.Parse.Packrat.Generated.Parser");
         Indent_Line ("is");
         Indent := @ + 3;
         Indent := @ - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line
           ("return Parser : WisiToken.Parse.Packrat.Generated.Parser (" &
              Trimmed_Image (Descriptor.First_Nonterminal) & ", " &
              Trimmed_Image (Descriptor.Last_Nonterminal) & ") do");
         Indent := Indent + 3;
         Indent_Line ("Parser.Tree.Lexer := Lexer.New_Lexer (Trace, " & Actions_Package_Name & ".Descriptor'Access);");
         Indent_Line ("Parser.Productions := Create_Productions;");
         Indent_Line ("Parser.User_Data := User_Data;");
         Indent_Line ("Parser.Parse_WisiToken_Accept := Parse_wisitoken_accept'Access;");
         Indent := Indent - 3;
         Indent_Line ("end return;");

      when Packrat_Proc =>
         Indent_Line
           ("  return WisiToken.Parse.Packrat.Procedural.Parser");
         Indent_Line ("is");
         Indent := Indent + 3;
         Indent_Line ("use WisiToken;");
         Indent_Line ("use WisiToken.Productions;");
         Indent_Line ("Grammar               : Prod_Arrays.Vector;");
         Put_Direct_Left_Recursive;
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         WisiToken.BNF.Generate_Grammar (Generate_Data.Grammar, Generate_Data.Action_Names.all);

         Indent_Line
           ("return Parser : WisiToken.Parse.Packrat.Procedural.Parser (" &
              Trimmed_Image (Descriptor.First_Nonterminal) & ", " &
              Trimmed_Image (Descriptor.Last_Nonterminal) & ") do");
         Indent := Indent + 3;
         Indent_Line ("Parser.Tree.Lexer := Lexer.New_Lexer (Trace, " & Actions_Package_Name & ".Descriptor'Access);");
         Indent_Line ("Parser.Productions := Create_Productions;");
         Indent_Line ("Parser.User_Data := User_Data;");
         Indent_Line ("Parser.Grammar := Grammar;");
         Indent_Line ("Parser.Direct_Left_Recursive := Direct_Left_Recursive;");
         Indent_Line ("Parser.Start_ID := " & Trimmed_Image (Generate_Data.Descriptor.Accept_ID) & ";");
         Indent := Indent - 3;
         Indent_Line ("end return;");
      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;
   end Packrat_Create_Create_Parser;

   procedure External_Create_Create_Grammar
     (Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is begin
      Indent_Line ("function Create_Grammar return WisiToken.Productions.Prod_Arrays.Vector");
      Indent_Line ("is");
      Indent_Line ("   use WisiToken;");
      Indent_Line ("   use WisiToken.Productions;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("return Grammar : WisiToken.Productions.Prod_Arrays.Vector do");
      Indent := Indent + 3;
      WisiToken.BNF.Generate_Grammar (Generate_Data.Grammar, Generate_Data.Action_Names.all);
      Indent := Indent - 3;
      Indent_Line ("end return;");
      Indent := Indent - 3;
      Indent_Line ("end Create_Grammar;");
   end External_Create_Create_Grammar;

   procedure Create_Create_Productions
     (Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      subtype Nonterminal_ID is Token_ID range Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index;

      Actions_Present : Boolean := False;
   begin
      Indent_Line ("function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector");
      Indent_Line ("is begin");
      Indent := Indent + 3;
      Indent_Line ("return Result : WisiToken.Syntax_Trees.Production_Info_Trees.Vector do");
      Indent := Indent + 3;
      Indent_Line
        ("Result.Set_First_Last (" &
           Trimmed_Image (Generate_Data.Grammar.First_Index) & ", " &
           Trimmed_Image (Generate_Data.Grammar.Last_Index) & ");");

      for I in Nonterminal_ID loop
         declare
            P : Productions.Instance renames Generate_Data.Grammar (I);
         begin
            if P.Optimized_List then
               Indent_Line ("Result (" & Trimmed_Image (P.LHS) & ").Optimized_List := True;");
               Actions_Present := True;
            end if;

            if Generate_Data.Check_Names (P.LHS) /= null or
              Generate_Data.Action_Names (P.LHS) /= null
            then
               Indent_Line
                 ("Result (" & Trimmed_Image (P.LHS) & ").RHSs.Set_First_Last (" &
                    Trimmed_Image (P.RHSs.First_Index) & ", " &
                    Trimmed_Image (P.RHSs.Last_Index) & ");");

               for J in P.RHSs.First_Index .. P.RHSs.Last_Index loop
                  if Generate_Data.Check_Names (P.LHS) = null then
                     Indent_Line
                       ("Result (" & Trimmed_Image (P.LHS) & ").RHSs (" & Trimmed_Image (J) &
                          ").In_Parse_Action := null;");
                  else
                     Actions_Present := True;
                     Indent_Line
                       ("Result (" & Trimmed_Image (P.LHS) & ").RHSs (" & Trimmed_Image (J) & ").In_Parse_Action := " &
                          (if Generate_Data.Check_Names (P.LHS)(J) = null then "null"
                           else Generate_Data.Check_Names (P.LHS)(J).all & "'Access") &
                          ";");
                  end if;
                  if Generate_Data.Action_Names (P.LHS) = null then
                     Indent_Line
                       ("Result (" & Trimmed_Image (P.LHS) & ").RHSs (" & Trimmed_Image (J) &
                          ").Post_Parse_Action := null;");
                  else
                     Actions_Present := True;
                     Indent_Line
                       ("Result (" & Trimmed_Image (P.LHS) & ").RHSs (" & Trimmed_Image (J) &
                          ").Post_Parse_Action := " &
                          (if Generate_Data.Action_Names (P.LHS)(J) = null then "null"
                           else Generate_Data.Action_Names (P.LHS)(J).all & "'Access") &
                          ";");
                  end if;
               end loop;
            end if;
         end;
      end loop;
      if not Actions_Present then
         Indent_Line ("null;");
      end if;

      Indent := Indent - 3;
      Indent_Line ("end return;");
      Indent := Indent - 3;
      Indent_Line ("end Create_Productions;");
      New_Line;
   end Create_Create_Productions;

   procedure Create_re2c_File
     (Input_Data            :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple                 :         in Generate_Tuple;
      Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String)
   is
      use Ada.Strings.Fixed;
      use Generate_Utils;
      use WisiToken.BNF.Utils;
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".re2c");
      Set_Output (File);
      Indent := 1;

      Put_File_Header (C_Comment, "mode: C", Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (C_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Indent_Line ("#include <stddef.h>"); -- size_t
      Indent_Line ("#include <stdio.h>"); -- printf
      Indent_Line ("#include <stdlib.h>"); -- malloc
      New_Line;

      Indent_Line ("typedef struct wisi_lexer");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("unsigned char* buffer;           // input text, in utf-8 encoding");
      Indent_Line ("unsigned char* buffer_last;      // last byte in buffer");
      Indent_Line ("unsigned char* cursor;           // current byte");
      Indent_Line ("unsigned char* byte_token_start; // byte position at start of current token");
      Indent_Line ("size_t         char_pos;         // character position of current character");
      Indent_Line ("size_t         char_token_start; // character position at start of current token");
      Indent_Line ("int            line;             // 1 indexed");
      Indent_Line ("int            line_token_start; // line at start of current token");
      Indent_Line ("unsigned char* marker;           // saved cursor");
      Indent_Line ("size_t         marker_pos;       // saved character position");
      Indent_Line ("size_t         marker_line;      // saved line");
      Indent_Line ("unsigned char* context;          // saved cursor");
      Indent_Line ("size_t         context_pos;      // saved character position");
      Indent_Line ("int            context_line;     // saved line");
      Indent_Line ("int            verbosity;");
      New_Line;
      Indent := Indent - 3;
      Indent_Line ("} wisi_lexer;");
      New_Line;
      Indent_Line ("#define YYCTYPE unsigned char");
      New_Line;

      --  Status values:
      Indent_Line ("#define NO_ERROR 0");
      Indent_Line ("#define ERROR_unrecognized_character 1");

      ----------
      --  new_lexer, free_lexer, reset_lexer

      --  It's normal to increment lexer->cursor one past the end of input,
      --  but not to read that character. To support memory mapped files, we
      --  enforce this strictly; YYPEEK returns EOT (end of text) when
      --  reading past end of buffer; that's how we recognize the end of
      --  text token.

      Indent_Line ("wisi_lexer* " & Output_File_Name_Root & "_new_lexer");
      Indent_Line ("   (unsigned char* input, size_t length, int verbosity)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("wisi_lexer* result        = malloc (sizeof (wisi_lexer));");
      Indent_Line ("result->buffer            = input;");
      Indent_Line ("result->buffer_last       = input + length - 1;");
      Indent_Line ("result->cursor            = input;");
      Indent_Line ("result->byte_token_start  = input;");
      Indent_Line ("result->char_pos          = 1; /* match WisiToken.Buffer_Region */");
      Indent_Line ("result->char_token_start  = 1;");
      Indent_Line ("result->line              = 1;");
      Indent_Line ("result->line_token_start  = result->line;");
      Indent_Line ("result->verbosity         = 0;");
      Indent_Line ("return result;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_free_lexer(wisi_lexer** lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("free(*lexer);");
      Indent_Line ("*lexer = 0;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_reset_lexer(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->cursor   = lexer->buffer;");
      Indent_Line ("lexer->char_pos = 1;");
      Indent_Line ("lexer->line     = 1;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_set_verbosity");
      Indent_Line ("   (wisi_lexer* lexer, int verbosity)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->verbosity = verbosity;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_set_position");
      Indent_Line ("   (wisi_lexer* lexer, size_t byte_position, size_t char_position, int line)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->cursor   = lexer->buffer + byte_position - 1;");
      Indent_Line ("lexer->char_pos = char_position;");
      Indent_Line ("lexer->line     = line;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      ----------
      --  next_token utils

      Indent_Line ("static void debug(wisi_lexer* lexer, int state, unsigned char ch)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->verbosity > 0)");
      Indent_Line ("   {");
      Indent_Line ("   if (ch < ' ')");
      Indent_Line ("      printf (""lexer: %d, 0x%x\n"", state, ch);");
      Indent_Line ("   else");
      Indent_Line ("      printf (""lexer: %d, '%c' 0x%x\n"", state, ch, ch);");
      Indent_Line ("   }");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Line ("#define YYDEBUG(state, ch) debug(lexer, state, ch)");

      --  YYCURSOR is only used in calls of YYDEBUG; we can't define it as
      --  YYPEEK because it is used as '*YYCURSOR'.
      Indent_Line ("#define YYCURSOR lexer->cursor");
      New_Line;

      Indent_Line ("#define YYPEEK() (lexer->cursor <= lexer->buffer_last) ? *lexer->cursor : 4");
      New_Line;

      Indent_Line ("static void skip(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("++lexer->cursor;");
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      Indent_Line ("{");
      Indent_Line ("   /* UFT-8 encoding: https://en.wikipedia.org/wiki/UTF-8#Description */");
      Indent_Line ("   if (*lexer->cursor == 0x0A && lexer->cursor > lexer->buffer && *(lexer->cursor - 1) == 0x0D)");
      Indent_Line ("     {/* second byte of DOS line ending */");
      Indent_Line ("     }");
      Indent_Line ("   else if ((*lexer->cursor & 0x80) == 0x80 && (*lexer->cursor & 0xC0) != 0xC0)");
      Indent_Line ("     {/* byte 2, 3 or 4 of multi-byte UTF-8 char */");
      Indent_Line ("     }");
      Indent_Line ("   else");
      Indent_Line ("     lexer->char_pos++;");
      Indent_Line ("} else ");
      Indent_Line ("   lexer->char_pos++;");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Start ("#define YYSKIP() skip(lexer)");
      New_Line;

      Indent_Line ("#define YYBACKUP() lexer->marker = lexer->cursor; lexer->marker_pos = lexer->char_pos;" &
                     "lexer->marker_line = lexer->line");
      Indent_Line ("#define YYRESTORE() lexer->cursor = lexer->marker; lexer->char_pos = lexer->marker_pos;" &
                     "lexer->line = lexer->marker_line");
      Indent_Line ("#define YYBACKUPCTX() lexer->context = lexer->cursor; lexer->context_pos = lexer->char_pos;" &
                     "lexer->context_line = lexer->line");
      Indent_Line ("#define YYRESTORECTX() lexer->cursor = lexer->context; lexer->char_pos = lexer->context_pos;" &
                     "lexer->line = lexer->context_line");
      New_Line;

      if Is_In (Input_Data.Tokens.Tokens, "delimited-text") or
        Is_In (Input_Data.Tokens.Non_Grammar, "delimited-text")
      then
         Indent_Line ("static void skip_to(wisi_lexer* lexer, char* target)");
         Indent_Line ("{");
         Indent_Line ("  int i, j;");
         New_Line;
         Indent_Line ("  // Count all new-lines contained in the skip region. Caller has ");
         Indent_Line ("  // skipped the start delimiter; if lexer->cursor is a new-line it");
         Indent_Line ("  // has not yet been counted. Start and end delimiters do not contain new-line.");
         Indent_Line ("  while (lexer->cursor <= lexer->buffer_last)");
         Indent_Line ("    {");
         Indent_Line ("      if (*lexer->cursor == 0x0A)");
         Indent_Line ("      {");
         Indent_Line ("        lexer->line++;");
         Indent_Line ("      }");
         Indent_Line ("      if (*lexer->cursor == target[0])");
         Indent_Line ("      {");
         Indent_Line ("        i = 0;");
         Indent_Line ("        do");
         Indent_Line ("          i++;");
         Indent_Line ("        while (0 != target[i] &&");
         Indent_Line ("               lexer->cursor + i <= lexer->buffer_last &&");
         Indent_Line ("               *(lexer->cursor + i) == target[i]);");
         New_Line;
         Indent_Line ("        if (0 == target[i])");
         Indent_Line ("          {");
         Indent_Line ("            for (j = 0; j < i; j++)");
         Indent_Line ("               skip(lexer);");
         Indent_Line ("            break;");
         Indent_Line ("          }");
         Indent_Line ("      }");
         Indent_Line ("      skip(lexer);");
         Indent_Line ("    };");
         Indent_Line ("}");
         New_Line;
      end if;

      ----------
      --  next_token
      Indent_Line ("int " & Output_File_Name_Root & "_next_token");
      Indent_Line ("  (wisi_lexer* lexer,");
      Indent_Line ("   int* id,");
      Indent_Line ("   size_t* byte_position,");
      Indent_Line ("   size_t* byte_length,");
      Indent_Line ("   size_t* char_position,");
      Indent_Line ("   size_t* char_length,");
      Indent_Line ("   int*    line_start,");
      Indent_Line ("   int*    line_length)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Indent_Line ("int status = NO_ERROR;");
      Indent_Line ("*id = -1;"); --  Token_ID'First = 0; see dragon_4_43.wy

      Indent_Line ("if (lexer->cursor > lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("*id            =" & WisiToken.Token_ID'Image (Generate_Data.Descriptor.EOI_ID) & ";");
      --  EOI position.last = last char of input, so byte_region (root) = all of input (in packrat parse)
      --  EOI position.first = last + 1 => null region.
      Indent_Line ("*byte_position = lexer->buffer_last - lexer->buffer + 2;");
      Indent_Line ("*byte_length   = 0;");
      Indent_Line ("*char_position = lexer->char_pos;");
      Indent_Line ("*char_length   = 0;");
      Indent_Line ("*line_start    = lexer->line;");
      Indent_Line ("*line_length   = 0;");
      Indent_Line ("return status;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("lexer->byte_token_start = lexer->cursor;");
      Indent_Line ("lexer->char_token_start = lexer->char_pos;");
      Indent_Line ("lexer->line_token_start = lexer->line;");
      New_Line;

      Indent_Line ("while (*id == -1 && status == 0)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Put_Line ("/*!re2c");
      Indent_Line ("re2c:yyfill:enable   = 0;");
      Indent_Line ("re2c:sentinel   = 4;");
      New_Line;

      --  Regexps used in definitions
      for Pair of Input_Data.Tokens.Lexer_Regexps loop
         Indent_Line (-Pair.Name & " = " & (-Pair.Value) & ";");
      end loop;
      New_Line;

      --  definitions
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop

         if Kind (Generate_Data, I) = "comment-new-line" then
            --  This must be before the check for "trailing context syntax", to
            --  handle Java comments.
            Indent_Line
              (Name (Generate_Data, I) & " = " & Value (Generate_Data, I) &
                 "[^\x0a\x04]*([\x0a]|[\x0d][\x0a]|[\x04]) ;");

         elsif Kind (Generate_Data, I) = "comment-one-line" then
            declare
               Open  : constant String := Value (Generate_Data, I);
               Close : constant String := Repair_Image (Generate_Data, I);
            begin
               --  Open and Close are both strings.
               if Close'Length = 3 then
                  --  Here we handle the special case of Close being a single character.
                  Indent_Line
                    (Name (Generate_Data, I) & " = " & Open & " [^\x0a\x04" & Close (2) & "]* " & Close & ";");
               else
                  raise SAL.Not_Implemented;
                  --  IMPROVEME: similar to delimited-text, but exclude new-line.
               end if;
            end;

         elsif 0 /= Index (Source => Value (Generate_Data, I), Pattern => "/") then
            --  Trailing context syntax; forbidden in definitions
            null;

         elsif Kind (Generate_Data, I) = "EOI" then
            Indent_Line (Name (Generate_Data, I) & " = [\x04];");

         elsif Kind (Generate_Data, I) = "delimited-text" then
            --  Not declared in definitions
            null;

         elsif Kind (Generate_Data, I) = "keyword" and Input_Data.Language_Params.Case_Insensitive then
            --  This assumes re2c regular expression syntax, where single quote
            --  means case insensitive.
            Indent_Line (Name (Generate_Data, I) & " = '" & Strip_Quotes (Value (Generate_Data, I)) & "';");

         elsif Kind (Generate_Data, I) = "new-line" then
            Indent_Line (Name (Generate_Data, I) & " = [\x0a]|[\x0d][\x0a];");

         else
            --  Other kinds have values that are regular expressions, in lexer syntax
            Indent_Line (Name (Generate_Data, I) & " = " & Value (Generate_Data, I) & ";");
         end if;
      end loop;
      New_Line;

      --  lexer rules
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop
         declare
            Val : constant String := Value (Generate_Data, I);
         begin

            if Kind (Generate_Data, I) = "non-reporting" then
               Indent_Line (Name (Generate_Data, I) & " { lexer->byte_token_start = lexer->cursor;");
               Indent_Line ("    lexer->char_token_start = lexer->char_pos;");
               Indent_Line ("    lexer->line_token_start = lexer->line;");
               Indent_Line ("    continue; }");

            elsif Kind (Generate_Data, I) = "delimited-text" then
               Indent_Line
                 (Val & " {*id =" & WisiToken.Token_ID'Image (ID (I)) &
                    "; skip_to(lexer, " & Repair_Image (Generate_Data, I) & "); continue;}");

            elsif Kind (Generate_Data, I) = "new-line"
            then
               Indent_Line
                 (Name (Generate_Data, I) &
                    " {*id =" & WisiToken.Token_ID'Image (ID (I)) & "; lexer->line++; continue;}");

            elsif Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "comment-new-line"
            then
               --  Comments can be terminated by new_line or EOI
               Indent_Line
                 (Name (Generate_Data, I) &
                    " {*id =" & WisiToken.Token_ID'Image (ID (I)) &
                    "; if (lexer->cursor[-1] == 0x0a || (lexer->cursor[-1] == 0x0d && lexer->cursor[-2] == 0x0a))" &
                    " lexer->line++; continue;}");

            elsif 0 /= Index (Source => Val, Pattern => "/") then
               Indent_Line (Val & " {*id =" & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");

            else
               Indent_Line (Name (Generate_Data, I) & " {*id =" & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");
            end if;
         end;
      end loop;
      New_Line;

      --  Default action.
      Indent_Line ("* {status = ERROR_unrecognized_character; continue;}");

      Put_Line ("*/");
      Indent := Indent - 3;
      Indent_Line ("}");

      Indent_Line ("/* lexer->cursor and lexer ->char_pos are one char past end of token */");
      Indent_Line ("*byte_position = lexer->byte_token_start - lexer->buffer + 1;");
      Indent_Line ("*byte_length   = lexer->cursor - lexer->byte_token_start;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = lexer->char_pos - lexer->char_token_start;");
      Indent_Line ("*line_start    = lexer->line_token_start;");
      Indent_Line ("*line_length   = lexer->line - lexer->line_token_start;");
      Indent_Line ("return status;");
      Indent_Line ("}");
      Indent := Indent - 3;
      Set_Output (Standard_Output);
      Close (File);

      declare
         Ada_Name : constant String := Output_File_Name_Root & "_re2c_c";
         --  Output_File_Name_Root is the file name of the grammar file -
         --  assume it is a legal Ada name.
      begin
         Create (File, Out_File, Output_File_Name_Root & "_re2c_c.ads");
         Set_Output (File);
         Indent := 1;
         Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
         Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
         New_Line;

         Put_Line ("with Interfaces.C;");
         Put_Line ("with WisiToken;");
         Put_Line ("with System;");
         Put_Line ("package " & Ada_Name & " is");
         Indent := Indent + 3;
         New_Line;

         Indent_Line ("function New_Lexer");
         Indent_Line ("  (Buffer    : in System.Address;");
         Indent_Line ("   Length    : in Interfaces.C.size_t)");
         Indent_Line ("  return System.Address");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_new_lexer"";");
         Indent_Line ("--  Create the lexer object, passing it the text buffer.");
         New_Line;
         Indent_Line ("procedure Free_Lexer (Lexer : in out System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_free_lexer"";");
         Indent_Line ("--  Free the lexer object");
         New_Line;

         Indent_Line ("procedure Reset_Lexer (Lexer : in System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_reset_lexer"";");
         New_Line;

         Indent_Line ("procedure Set_Verbosity");
         Indent_Line ("  (Lexer     : in System.Address;");
         Indent_Line ("   Verbosity : in Interfaces.C.int)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_set_verbosity"";");

         Indent_Line ("procedure Set_Position");
         Indent_Line ("  (Lexer         : in System.Address;");
         Indent_Line ("   Byte_Position : in Interfaces.C.size_t;");
         Indent_Line ("   Char_Position : in Interfaces.C.size_t;");
         Indent_Line ("   Line          : in Interfaces.C.int)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_set_position"";");
         New_Line;

         Indent_Line ("function Next_Token");
         Indent_Line ("  (Lexer         : in     System.Address;");
         Indent_Line ("   ID            :    out WisiToken.Token_ID;");
         Indent_Line ("   Byte_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Byte_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Line_Start    :    out Interfaces.C.int;");
         Indent_Line ("   Line_Length   :    out Interfaces.C.int)");
         Indent_Line ("  return Interfaces.C.int");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_next_token"";");
         New_Line;

         Indent := Indent - 3;
         Put_Line ("end " & Ada_Name & ";");
         Set_Output (Standard_Output);
         Close (File);
      end;
   end Create_re2c_File;

   procedure Create_re2c_Lexer
     (Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String)
   is
      use WisiToken.BNF.Generate_Utils;

      New_Line_Count : Integer := 0;
      Block_Count    : Integer := 0;
      Need_Separator : Boolean := False;
   begin
      for I in All_Tokens (Generate_Data).Iterate
        (Non_Grammar  => True,
         Nonterminals => False,
         Include_SOI  => False)
      loop
         if Kind (Generate_Data, I) = "comment-new-line" or
           Kind (Generate_Data, I) = "comment-one-line" or
           Kind (Generate_Data, I) = "string-double-one-line" or
           Kind (Generate_Data, I) = "string-single-one-line"
           --  comment-one-line, strings do not always contain a new_line, but
           --  the preconditions in WisiToken.Lexer guarantee it does if we ask
           --  for Line_Begin_Char_Pos from one.
         then
            New_Line_Count := @ + 1;
            Block_Count  := @ + 1;

         elsif Kind (Generate_Data, I) = "new-line" then
            New_Line_Count := @ + 1;

         elsif Kind (Generate_Data, I) = "string-double" or
           Kind (Generate_Data, I) = "string-single" or
           Kind (Generate_Data, I) = "delimited-text"
         then
            Block_Count := @ + 1;
         end if;
      end loop;

      Indent_Line ("function Is_Block_Delimited (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");
      if Block_Count > 0 then
         Indent_Line ("when");
         Need_Separator := False;
         Indent := @ + 3;

         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line" or
              Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               if Need_Separator then
                  Put_Line (" |");
               else
                  Need_Separator := True;
               end if;
               Indent_Start (Name (Generate_Data, I) & "_ID");
            end if;
         end loop;
         Put_Line (" => return True;");
         Indent := @ - 3;
      end if;

      Indent_Line ("when others => return False;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Is_Block_Delimited;");
      New_Line;

      Indent_Line ("function Same_Block_Delimiters (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line" or
              Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return True;");

            elsif Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return False;");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => return False;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Same_Block_Delimiters;");
      New_Line;

      Indent_Line ("function Escape_Delimiter_Doubled (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Generate_Data.Tokens.Escape_Delimiter_Doubled.Contains (Name (Generate_Data, I)) then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return True;");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => return False;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Escape_Delimiter_Doubled;");
      New_Line;

      Indent_Line ("function Start_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) & "_ID => return" &
                    Integer'Image (Utils.Strip_Quotes (Value (Generate_Data, I))'Length) & ";");

            elsif Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line" or
              Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return 1;");

            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error; return 0;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Start_Delimiter_Length;");
      New_Line;

      Indent_Line ("function End_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");
      if Block_Count > 0 then
         Indent_Line ("when");
         Need_Separator := False;
         Indent := @ + 3;

         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line" or
              Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single"
            then
               if Need_Separator then
                  Put_Line (" |");
               else
                  Need_Separator := True;
               end if;
               Indent_Start (Name (Generate_Data, I) & "_ID");
            end if;
         end loop;
         Put_Line (" => return 1;");
         Indent := @ - 3;

         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) & "_ID => return" &
                    Integer'Image (Utils.Strip_Quotes (Repair_Image (Generate_Data, I))'Length) & ";");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error; return 0;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end End_Delimiter_Length;");
      New_Line;

      Indent_Line ("function New_Line_Is_End_Delimiter (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("return");
      Indent_Line ("  (case To_Token_Enum (ID) is");
      Indent := @ + 3;
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => True,");

            elsif Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => False,");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error);");
      Indent := @ - 6;
      Indent_Line ("end New_Line_Is_End_Delimiter;");
      New_Line;

      Indent_Line ("function Find_End_Delimiter");
      Indent_Line ("  (Source      : in WisiToken.Lexer.Source;");
      Indent_Line ("   ID          : in WisiToken.Token_ID;");
      Indent_Line ("   Token_Start : in WisiToken.Buffer_Pos)");
      Indent_Line ("  return WisiToken.Buffer_Pos");
      if Block_Count > 0 then
         Indent_Line ("is begin");
      else
         Indent_Line ("is");
         Indent_Line ("   pragma Unreferenced (Source, Token_Start);");
         Indent_Line ("begin");
      end if;
      Indent := @ + 3;
      Indent_Line ("return");
      Indent_Line ("  (case To_Token_Enum (ID) is");
      Indent := @ + 3;
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) & "_ID => WisiToken.Lexer.Find_New_Line (Source, Token_Start),");

            elsif Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"""""),");

            elsif Kind (Generate_Data, I) = "string-double" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => WisiToken.Lexer.Find_String (Source, Token_Start, """"""""),");

            elsif Kind (Generate_Data, I) = "string-single" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => WisiToken.Lexer.Find_String (Source, Token_Start, ""'""),");

            elsif Kind (Generate_Data, I) = "comment-one-line" or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => WisiToken.Lexer.Find_String (Source, Token_Start, " &
                    --  Repair_Image includes quotes.
                    Repair_Image (Generate_Data, I) & "),");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error);");
      Indent := @ - 6;
      Indent_Line ("end Find_End_Delimiter;");
      New_Line;

      Indent_Line ("function Find_Scan_End");
      Indent_Line ("  (Source   : in WisiToken.Lexer.Source;");
      Indent_Line ("   ID       : in WisiToken.Token_ID;");
      Indent_Line ("   Region   : in WisiToken.Buffer_Region;");
      Indent_Line ("   Inserted : in Boolean;");
      Indent_Line ("   Start    : in Boolean)");
      Indent_Line ("  return WisiToken.Buffer_Pos");
      declare
         Need_Region : constant Boolean :=
           (for some I in All_Tokens (Generate_Data).Iterate
              (Non_Grammar  => True,
               Nonterminals => False,
               Include_SOI  => False)
              => Kind (Generate_Data, I) = "comment-new-line" or
                Kind (Generate_Data, I) = "string-double-one-line" or
                Kind (Generate_Data, I) = "string-single-one-line" or
                Kind (Generate_Data, I) = "comment-one-line" or
                Kind (Generate_Data, I) = "delimited-text");

         Need_Inserted : constant Boolean :=
           (for some I in All_Tokens (Generate_Data).Iterate
              (Non_Grammar  => True,
               Nonterminals => False,
               Include_SOI  => False)
              => Kind (Generate_Data, I) = "comment-new-line" or
                Kind (Generate_Data, I) = "comment-one-line" or
                Kind (Generate_Data, I) = "delimited-text");

         Need_Start : constant Boolean :=
           (for some I in All_Tokens (Generate_Data).Iterate
              (Non_Grammar  => True,
               Nonterminals => False,
               Include_SOI  => False)
              => Kind (Generate_Data, I) = "comment-new-line" or
                Kind (Generate_Data, I) = "comment-one-line" or
                Kind (Generate_Data, I) = "delimited-text");
      begin
         if Block_Count > 0 then
            Indent_Line ("is");
            Indent_Line ("   use WisiToken;");
            if not Need_Region then
               Indent_Line ("   pragma Unreferenced (Region);");
            end if;
            if not Need_Inserted then
               Indent_Line ("   pragma Unreferenced (Inserted);");
            end if;
            if not Need_Start then
               Indent_Line ("   pragma Unreferenced (Start);");
            end if;
            Indent_Line ("begin");
         else
            Indent_Line ("is");
            Indent_Line ("   pragma Unreferenced (Source, Region, Inserted, Start);");
            Indent_Line ("begin");
         end if;
      end;
      Indent := @ + 3;
      Indent_Line ("return");
      Indent_Line ("  (case To_Token_Enum (ID) is");
      Indent := @ + 3;
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            --  Inserted : a start or end delimiter was inserted
            --  Start    : start delimeter
            --  Return position where lex can end
            --  start delimiter in Value, end delimiter in Repair_Image
            if Kind (Generate_Data, I) = "comment-new-line" then
               --  If Inserted, Start; a comment start was inserted in an existing
               --  comment; just scan the existing comment.
               --
               --  If Inserted, not Start; a comment end was inserted in an existing
               --  comment; scan to the previous comment end.
               --
               --  If not inserted, Start; a comment start was deleted; scan to the
               --  previous comment end.
               --
               --  If not inserted, not Start; a comment end was deleted; find a new
               --  comment end.
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID =>");
               Indent_Line ("(if Inserted then Region.Last");
               Indent_Line (" elsif Start then Region.Last");
               Indent_Line (" else Lexer.Find_New_Line (Source, Region.Last)),");

            elsif Kind (Generate_Data, I) = "string-double-one-line" or
              Kind (Generate_Data, I) = "string-single-one-line"
            then
               --  Delimiters are the same, so all delimiters flip state; terminated
               --  by new_line.
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => Lexer.Find_New_Line (Source, Region.Last),");

            elsif Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single"
            then
               --  Delimiters are the same, so all delimiters flip state; terminated
               --  by EOI.
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => Lexer.Buffer_Region_Byte (Source).Last,");

            elsif Kind (Generate_Data, I) = "comment-one-line" then
               --  Similar to comment-new-line, terminated by either end delimiter or new_line
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID =>");
               Indent_Line ("(if Inserted then Region.Last");
               Indent_Line (" elsif Start then Region.Last");
               Indent_Line (" else Lexer.Find_String_Or_New_Line (Source, Region.Last, " &
                              Value (Generate_Data, I) & ")),");

            elsif Kind (Generate_Data, I) = "delimited-text"
            then
               --  Similar to comment-new-line, terminated by either end delimiter or EOI
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID =>");
               Indent_Line ("(if Inserted then Region.Last");
               Indent_Line (" elsif Start then Region.Last");
               Indent_Line (" else Lexer.Find_String (Source, Region.First, " &
                              Repair_Image (Generate_Data, I) & ")),");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error);");
      Indent := @ - 6;
      Indent_Line ("end Find_Scan_End;");
      New_Line;

      Indent_Line ("function Contains_End_Delimiter");
      Indent_Line ("  (Source : in WisiToken.Lexer.Source;");
      Indent_Line ("   ID     : in WisiToken.Token_ID;");
      Indent_Line ("   Region : in WisiToken.Buffer_Region)");
      Indent_Line ("  return WisiToken.Base_Buffer_Pos");
      if Block_Count > 0 then
         Indent_Line ("is");
         Indent_Line ("   use WisiToken;");
         Indent_Line ("begin");
      else
         Indent_Line ("is");
         Indent_Line ("   use WisiToken;");
         Indent_Line ("   pragma Unreferenced (Source, Region);");
         Indent_Line ("begin");
      end if;
      Indent := @ + 3;
      Indent_Line ("return");
      Indent_Line ("  (case To_Token_Enum (ID) is");
      Indent := @ + 3;
      if Block_Count > 0 then
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            if Kind (Generate_Data, I) = "comment-new-line" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_New_Line (Source, Region),");

            elsif Kind (Generate_Data, I) = "string-double-one-line" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_String_Or_New_Line (Source, Region, """"""""),");

            elsif Kind (Generate_Data, I) = "string-single-one-line" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_String_Or_New_Line (Source, Region, ""'""),");

            elsif Kind (Generate_Data, I) = "string-double" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_String (Source, Region, """"""""),");

            elsif Kind (Generate_Data, I) = "string-single" then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_String (Source, Region, ""'""),");

            elsif Kind (Generate_Data, I) = "comment-one-line"  or
              Kind (Generate_Data, I) = "delimited-text"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) &
                    "_ID => Lexer.Find_String_Or_New_Line (Source, Region, " &
                    --  Repair_Image includes quotes.
                    Repair_Image (Generate_Data, I) & "),");
            end if;
         end loop;
      end if;

      Indent_Line ("when others => raise SAL.Programmer_Error);");
      Indent := @ - 6;
      Indent_Line ("end Contains_End_Delimiter;");
      New_Line;

      Indent_Line ("function Line_Begin_Char_Pos");
      Indent_Line (" (Source : in WisiToken.Lexer.Source;");
      Indent_Line ("  Token  : in WisiToken.Lexer.Token;");
      Indent_Line ("  Line   : in WisiToken.Line_Number_Type)");
      Indent_Line ("return WisiToken.Buffer_Pos");
      Indent_Line ("is");
      if New_Line_Count + Block_Count = 0 then
         Indent_Line ("   pragma Unreferenced (Source, Token, Line);");

      elsif Block_Count = 0 then
         Indent_Line ("   pragma Unreferenced (Source, Line);");
      else
         declare
            Need_Source_Line : Boolean := False;
         begin
            for I in All_Tokens (Generate_Data).Iterate
              (Non_Grammar  => True,
               Nonterminals => False,
               Include_SOI  => False)
            loop
               if Kind (Generate_Data, I) = "comment-new-line" or
                 Kind (Generate_Data, I) = "comment-one-line" or
                 Kind (Generate_Data, I) = "string-double-one-line" or
                 Kind (Generate_Data, I) = "string-single-one-line" or
                 Kind (Generate_Data, I) = "new-line"
               then
                  null;
               elsif Kind (Generate_Data, I) = "delimited-text" or
                 Kind (Generate_Data, I) = "string-double" or
                 Kind (Generate_Data, I) = "string-single"
               then
                  Need_Source_Line := True;
               end if;
            end loop;
            if not Need_Source_Line then
               Indent_Line ("   pragma Unreferenced (Source, Line);");
            end if;
         end;
      end if;

      if New_Line_Count > 0 then
         Indent_Line ("   use all type WisiToken.Base_Buffer_Pos;");
      end if;
      Indent_Line ("begin");
      Indent := @ + 3;
      if New_Line_Count + Block_Count = 0 then
         Indent_Line ("return WisiToken.Invalid_Buffer_Pos;");

      else
         Indent_Line ("case To_Token_Enum (Token.ID) is");
         for I in All_Tokens (Generate_Data).Iterate
           (Non_Grammar  => True,
            Nonterminals => False,
            Include_SOI  => False)
         loop
            --  The preconditions on Lexer.Line_Begin_Char_Pos ensure that Token
            --  contains the new-line for Line. "comment-one-line",
            --  "string-double-one-line", "string-single-one-line" cannot contain
            --  new-line.
            if Kind (Generate_Data, I) = "comment-new-line" or
              Kind (Generate_Data, I) = "new-line"
            then
               Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return Token.Char_Region.Last + 1;");

            elsif Kind (Generate_Data, I) = "delimited-text" or
              Kind (Generate_Data, I) = "string-double" or
              Kind (Generate_Data, I) = "string-single"
            then
               Indent_Line
                 ("when " & Name (Generate_Data, I) & "_ID => return " &
                    "WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);");

            end if;
         end loop;

         Indent_Line ("when others => raise SAL.Programmer_Error;");
         Indent_Line ("end case;");
      end if;
      Indent := @ - 3;
      Indent_Line ("end Line_Begin_Char_Pos;");
      New_Line;

      Indent_Line ("function Can_Contain_New_Line (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");

      for I in All_Tokens (Generate_Data).Iterate
        (Non_Grammar  => True,
         Nonterminals => False,
         Include_SOI  => False)
      loop
         if Kind (Generate_Data, I) = "new-line" or
           Kind (Generate_Data, I) = "comment-new-line" or
           Kind (Generate_Data, I) = "delimited-text"
         then
            Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return True;");
         end if;
      end loop;

      Indent_Line ("when others => return False;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Can_Contain_New_Line;");
      New_Line;

      Indent_Line ("function Terminated_By_New_Line (ID : in WisiToken.Token_ID) return Boolean");
      Indent_Line ("is begin");
      Indent := @ + 3;
      Indent_Line ("case To_Token_Enum (ID) is");

      for I in All_Tokens (Generate_Data).Iterate
        (Non_Grammar  => True,
         Nonterminals => False,
         Include_SOI  => False)
      loop
         if Kind (Generate_Data, I) = "new-line" or
           Kind (Generate_Data, I) = "comment-new-line" or
           Kind (Generate_Data, I) = "string-double-one-line" or
           Kind (Generate_Data, I) = "string-single-one-line"
         then
            Indent_Line ("when " & Name (Generate_Data, I) & "_ID => return True;");
         end if;
      end loop;

      Indent_Line ("when others => return False;");
      Indent_Line ("end case;");
      Indent := @ - 3;
      Indent_Line ("end Terminated_By_New_Line;");
      New_Line;

      Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
      Indent_Line ("  (" & Output_File_Name_Root & "_re2c_c.New_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Free_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Reset_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Set_Verbosity,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Set_Position,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Next_Token,");
      Indent_Line ("   Is_Block_Delimited,");
      Indent_Line ("   Same_Block_Delimiters,");
      Indent_Line ("   Escape_Delimiter_Doubled,");
      Indent_Line ("   Start_Delimiter_Length,");
      Indent_Line ("   End_Delimiter_Length,");
      Indent_Line ("   New_Line_Is_End_Delimiter,");
      Indent_Line ("   Find_End_Delimiter,");
      Indent_Line ("   Contains_End_Delimiter,");
      Indent_Line ("   Find_Scan_End,");
      Indent_Line ("   Line_Begin_Char_Pos,");
      Indent_Line ("   Can_Contain_New_Line,");
      Indent_Line ("   Terminated_By_New_Line);");
      New_Line;
   end Create_re2c_Lexer;

   function File_Name_To_Ada (File_Name : in String) return String
   is
      Result : String := File_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '.';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end File_Name_To_Ada;

   function Initialize
     (Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple             : in Generate_Tuple;
      Grammar_File_Name : in String;
      Output_File_Root  : in String;
      Check_Interface   : in Boolean)
     return Common_Data
   is begin
      return Data : Common_Data do
         Data.Generate_Algorithm := Tuple.Gen_Alg;

         Data.Output_Language := Ada_Output_Language (Tuple.Out_Lang);

         if Tuple.Gen_Alg = External or else Input_Data.User_Lexer in Valid_Lexer then
            Data.Lexer := Input_Data.User_Lexer;
         else
            raise SAL.Programmer_Error with "tuple.alg " & Generate_Algorithm'Image (Tuple.Gen_Alg) &
              " input_data.user_lexer " & Lexer_Image (Input_Data.User_Lexer).all;
         end if;

         if Check_Interface then
            if Tuple.Interface_Kind in Valid_Interface then
               Data.Interface_Kind := Valid_Interface (Tuple.Interface_Kind);
            else
               Put_Error (Error_Message (Grammar_File_Name, 1, "Interface_Kind not set"));
            end if;
         else
            Data.Interface_Kind := Process;
         end if;

         Data.Text_Rep := Tuple.Text_Rep;

         Data.Lower_File_Name_Root := +To_Lower (Output_File_Root);
      end return;
   end Initialize;

   function To_Token_Ada_Name (WY_Name : in String) return String
   is
      --  Convert WY_Name to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := WY_Name;
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

end WisiToken.BNF.Output_Ada_Common;
