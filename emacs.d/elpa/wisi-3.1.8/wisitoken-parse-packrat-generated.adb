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

package body WisiToken.Parse.Packrat.Generated is

   overriding procedure Parse
     (Parser     : in out Generated.Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is
      pragma Unreferenced (Log_File, Pre_Edited);
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      use all type Ada.Containers.Count_Type;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;

      Result : Memo_Entry;
   begin
      if Edits.Length > 0 then
         raise WisiToken.Parse_Error;
      end if;

      for Deriv of Parser.Derivs loop
         for Memo of Deriv loop
            case Memo.State is
            when No_Result | Failure =>
               null;
            when Success =>
               Memo.Last_Pos := Syntax_Trees.Invalid_Stream_Index;
            end case;
         end loop;
      end loop;

      Parser.Derivs.Set_First_Last (Descriptor.First_Nonterminal, Descriptor.Last_Nonterminal);

      Parser.Tree.Clear;

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Lex_All; -- Creates Tree.Shared_Stream

      --  WORKAROUND: there appears to be a bug in GNAT Community 2021 that makes
      --  ref_count fail in this usage. May be related to AdaCore ticket V107-045.
      Parser.Tree.Enable_Ref_Count_Check (Parser.Tree.Shared_Stream, Enable => False);

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => True)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream, Skip_EOI => False)));
      end loop;

      Result := Parser.Parse_WisiToken_Accept
        (Parser, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));

      if Result.State /= Success then
         if Trace_Parse > Outline then
            Parser.Tree.Lexer.Trace.Put_Line ("parse failed");
         end if;

         raise Syntax_Error with "parse failed"; --  FIXME packrat: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;

   end Parse;

end WisiToken.Parse.Packrat.Generated;
