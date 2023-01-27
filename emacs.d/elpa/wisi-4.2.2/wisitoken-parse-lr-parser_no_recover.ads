--  Abstract :
--
--  A generalized LR parser, with no error recovery, no semantic
--  checks, no incremental parse.
--
--  This allows wisi-generate (which uses the generated wisi_grammar)
--  to not depend on wisitoken-lr-mckenzie_recover, so editing that
--  does not cause everything to be regenerated/compiled.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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

with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR.Parser_No_Recover is

   type Parser is new WisiToken.Parse.Base_Parser with record
      Table   : Parse_Table_Ptr;
      Parsers : aliased Parser_Lists.List;
   end record;

   overriding procedure Finalize (Object : in out LR.Parser_No_Recover.Parser);
   --  Deep free Object.Table.

   procedure New_Parser
     (Parser      :    out LR.Parser_No_Recover.Parser;
      Lexer       : in     WisiToken.Lexer.Handle;
      Table       : in     Parse_Table_Ptr;
      Productions : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data   : in     Syntax_Trees.User_Data_Access);

   overriding procedure Parse
     (Shared_Parser : in out LR.Parser_No_Recover.Parser;
      Log_File      : in     Ada.Text_IO.File_Type;
      Edits         : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited    : in     Boolean        := False);
   --  Raises SAL.Programmer_Error if Edits is not empty. Pre_Edited and
   --  Log_File are ignored.

end WisiToken.Parse.LR.Parser_No_Recover;
