--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
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

package body WisiToken.Parse.Packrat is

   function Image (Item : in Memo_Entry; Tree : in Syntax_Trees.Tree) return String
   is begin
      return
        (case Item.State is
         when No_Result => "",
         when Failure => "fail @" & Image_Pos (Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True) &
           " @" & Image_Pos (Item.Max_Examined_Pos) &
           "," & Image_Pos (Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      return
        Syntax_Trees.Trimmed_Image (Pos) & ", " &
        (case Item.State is
         when No_Result => "",
         when Failure => Image (Nonterm, Descriptor) & " fail @" &
           Image_Pos (Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True, RHS_Index => True) &
           "," & Image_Pos (Item.Max_Examined_Pos) &
           "," & Image_Pos (Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      return
        Pos'Image & ", " &
        (case Item.State is
         when No_Result => "",
         when Failure => Image (Production_ID'(Nonterm, RHS_Index), Descriptor) & " fail @" &
           Image_Pos (Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True, RHS_Index => True) &
           "," & Image_Pos (Item.Max_Examined_Pos) &
           "," & Image_Pos (Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is begin
      return Image (Item, Nonterm, Tree.Get_Node_Index (Tree.Shared_Stream, Pos), Tree);
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is begin
      return Image (Item, Nonterm, RHS_Index, Tree.Get_Node_Index (Tree.Shared_Stream, Pos), Tree);
   end Image;

   function Image_Pos (Element : in Syntax_Trees.Stream_Index) return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Get_Node_Index (Element)'Image;
      end if;
   end Image_Pos;

   procedure Clear (Derivs : in out Packrat.Derivs)
   is begin
      for D of Derivs loop
         D.Clear (Free_Memory => True);
      end loop;
   end Clear;

   function Get_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index)
     return Memo_Entry
   is begin
      if Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index then
         return Derivs (Nonterm)(Pos);
      else
         return No_Result_Memo;
      end if;
   end Get_Deriv;

   procedure Set_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index;
      Memo    : in     Memo_Entry)
   is
      use all type WisiToken.Syntax_Trees.Node_Index;
   begin
      if Pos < Derivs (Nonterm).First_Index then
         Derivs (Nonterm).Set_First_Last (Pos, Derivs (Nonterm).Last_Index);

      elsif Pos > Derivs (Nonterm).Last_Index then
         Derivs (Nonterm).Set_First_Last (Derivs (Nonterm).First_Index, Pos);
      end if;

      Derivs (Nonterm).Replace_Element (Pos, Memo);
   end Set_Deriv;

   overriding procedure Finalize (Object : in out Parser)
   is begin
      Clear (Object.Derivs);
   end Finalize;


   procedure Finish_Parse
     (Parser : in out Packrat.Parser'Class;
      Result : in out Memo_Entry)
   is
      use WisiToken.Syntax_Trees;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Trace : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

   begin
      if Result.State = Packrat.Success then
         Tree.Set_Root (Result.Result);
         Result := No_Result_Memo;
         Clear (Parser.Derivs);
         Tree.Finish_Parse;

         if Trace_Parse > Outline then
            Trace.Put_Line ("packrat parse succeed");
         end if;

      else
         if Trace_Parse > Outline then
            Trace.Put_Line ("packrat parse fail @" & Image_Pos (Result.Max_Examined_Pos));
         end if;
         raise Syntax_Error with Tree.Error_Message
           (Tree.To_Rooted_Ref (Tree.Shared_Stream, Result.Max_Examined_Pos), "packrat parse fail");
      end if;
   end Finish_Parse;

end WisiToken.Parse.Packrat;
