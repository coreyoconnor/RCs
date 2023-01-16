--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with SAL;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_re2c_c;
with WisiToken.Parse.LR;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
package body Wisitoken_Grammar_Main is

   function Is_Block_Delimited (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         RAW_CODE_ID |
         REGEXP_ID |
         ACTION_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Is_Block_Delimited;

   function Same_Block_Delimiters (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return False;
      when RAW_CODE_ID => return False;
      when REGEXP_ID => return False;
      when ACTION_ID => return False;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Same_Block_Delimiters;

   function Escape_Delimiter_Doubled (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when others => return False;
      end case;
   end Escape_Delimiter_Doubled;

   function Start_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return 2;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when STRING_LITERAL_1_ID => return 1;
      when STRING_LITERAL_2_ID => return 1;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end Start_Delimiter_Length;

   function End_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return 1;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end End_Delimiter_Length;

   function New_Line_Is_End_Delimiter (ID : in WisiToken.Token_ID) return Boolean
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => True,
         when RAW_CODE_ID => False,
         when REGEXP_ID => False,
         when ACTION_ID => False,
         when STRING_LITERAL_1_ID => True,
         when STRING_LITERAL_2_ID => True,
         when others => raise SAL.Programmer_Error);
   end New_Line_Is_End_Delimiter;

   function Find_End_Delimiter
     (Source      : in WisiToken.Lexer.Source;
      ID          : in WisiToken.Token_ID;
      Token_Start : in WisiToken.Buffer_Pos)
     return WisiToken.Buffer_Pos
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => WisiToken.Lexer.Find_New_Line (Source, Token_Start),
         when RAW_CODE_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "}%"),
         when REGEXP_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "]%"),
         when ACTION_ID => WisiToken.Lexer.Find_String (Source, Token_Start, ")%"),
         when STRING_LITERAL_1_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when STRING_LITERAL_2_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when others => raise SAL.Programmer_Error);
   end Find_End_Delimiter;

   function Find_Scan_End
     (Source   : in WisiToken.Lexer.Source;
      ID       : in WisiToken.Token_ID;
      Region   : in WisiToken.Buffer_Region;
      Inserted : in Boolean;
      Start    : in Boolean)
     return WisiToken.Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_New_Line (Source, Region.Last)),
         when RAW_CODE_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "}%")),
         when REGEXP_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "]%")),
         when ACTION_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, ")%")),
         when STRING_LITERAL_1_ID => Lexer.Find_New_Line (Source, Region.Last),
         when STRING_LITERAL_2_ID => Lexer.Find_New_Line (Source, Region.Last),
         when others => raise SAL.Programmer_Error);
   end Find_Scan_End;

   function Contains_End_Delimiter
     (Source : in WisiToken.Lexer.Source;
      ID     : in WisiToken.Token_ID;
      Region : in WisiToken.Buffer_Region)
     return WisiToken.Base_Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => Lexer.Find_New_Line (Source, Region),
         when RAW_CODE_ID => Lexer.Find_String_Or_New_Line (Source, Region, "}%"),
         when REGEXP_ID => Lexer.Find_String_Or_New_Line (Source, Region, "]%"),
         when ACTION_ID => Lexer.Find_String_Or_New_Line (Source, Region, ")%"),
         when STRING_LITERAL_1_ID => Lexer.Find_String_Or_New_Line (Source, Region, """"),
         when STRING_LITERAL_2_ID => Lexer.Find_String_Or_New_Line (Source, Region, "'"),
         when others => raise SAL.Programmer_Error);
   end Contains_End_Delimiter;

   function Line_Begin_Char_Pos
    (Source : in WisiToken.Lexer.Source;
     Token  : in WisiToken.Lexer.Token;
     Line   : in WisiToken.Line_Number_Type)
   return WisiToken.Buffer_Pos
   is
      use all type WisiToken.Base_Buffer_Pos;
   begin
      case To_Token_Enum (Token.ID) is
      when NEW_LINE_ID => return Token.Char_Region.Last + 1;
      when COMMENT_ID => return Token.Char_Region.Last + 1;
      when RAW_CODE_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when REGEXP_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when ACTION_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when others => raise SAL.Programmer_Error;
      end case;
   end Line_Begin_Char_Pos;

   function Can_Contain_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when RAW_CODE_ID => return True;
      when REGEXP_ID => return True;
      when ACTION_ID => return True;
      when others => return False;
      end case;
   end Can_Contain_New_Line;

   function Terminated_By_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Terminated_By_New_Line;

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_re2c_c.New_Lexer,
      wisitoken_grammar_re2c_c.Free_Lexer,
      wisitoken_grammar_re2c_c.Reset_Lexer,
      wisitoken_grammar_re2c_c.Set_Verbosity,
      wisitoken_grammar_re2c_c.Set_Position,
      wisitoken_grammar_re2c_c.Next_Token,
      Is_Block_Delimited,
      Same_Block_Delimiters,
      Escape_Delimiter_Doubled,
      Start_Delimiter_Length,
      End_Delimiter_Length,
      New_Line_Is_End_Delimiter,
      Find_End_Delimiter,
      Contains_End_Delimiter,
      Find_Scan_End,
      Line_Begin_Char_Pos,
      Can_Contain_New_Line,
      Terminated_By_New_Line);

   function Create_Parse_Table
     return WisiToken.Parse.LR.Parse_Table_Ptr
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 145,
         First_Terminal    => 3,
         Last_Terminal     => 42,
         First_Nonterminal => 43,
         Last_Nonterminal  => 66);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 30, (48, 0), 1);
            Add_Action (Table.States (0), 39, (53, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 48, 3);
            Add_Goto (Table.States (0), 53, 4);
            Add_Goto (Table.States (0), 65, 5);
            Add_Goto (Table.States (0), 66, 6);
            Table.States (1).Action_List.Set_Capacity (10);
            Add_Action (Table.States (1), 4, (48, 6), 7);
            Add_Action (Table.States (1), 5, (48, 7), 8);
            Add_Action (Table.States (1), 6, (48, 8), 9);
            Add_Action (Table.States (1), 7, (48, 15), 10);
            Add_Action (Table.States (1), 8, (48, 13), 11);
            Add_Action (Table.States (1), 9, (48, 11), 12);
            Add_Action (Table.States (1), 11, (48, 5), 13);
            Add_Action (Table.States (1), 12, (48, 2), 14);
            Add_Action (Table.States (1), 16, (48, 0), 15);
            Add_Action (Table.States (1), 39, (48, 9), 16);
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 21, (53, 0), 17);
            Add_Action (Table.States (2), 22, (53, 1), 18);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (30, 39, 42), (65, 0),  1);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (30, 39, 42), (65, 1),  1);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (30, 39, 42), (66, 0),  1);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 30, (48, 0), 1);
            Add_Action (Table.States (6), 39, (53, 0), 2);
            Add_Action (Table.States (6), 42, Accept_It, (43, 0),  1);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 48, 3);
            Add_Goto (Table.States (6), 53, 4);
            Add_Goto (Table.States (6), 65, 19);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 39, (49, 0), 20);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 49, 21);
            Table.States (8).Action_List.Set_Capacity (4);
            Add_Action (Table.States (8), 3, (45, 2), 22);
            Add_Action (Table.States (8), 14, (45, 1), 23);
            Add_Action (Table.States (8), 15, (45, 0), 24);
            Add_Action (Table.States (8), 39, (45, 3), 25);
            Table.States (8).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (8), 45, 26);
            Add_Goto (Table.States (8), 46, 27);
            Table.States (9).Action_List.Set_Capacity (4);
            Add_Action (Table.States (9), 3, (45, 2), 22);
            Add_Action (Table.States (9), 14, (45, 1), 23);
            Add_Action (Table.States (9), 15, (45, 0), 24);
            Add_Action (Table.States (9), 39, (45, 3), 25);
            Table.States (9).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (9), 45, 26);
            Add_Goto (Table.States (9), 46, 28);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 9, (48, 15), 29);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 39, (48, 13), 30);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 39, (48, 11), 31);
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 39, (48, 5), 32);
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 28, (48, 2), 33);
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 28, (48, 0), 34);
            Table.States (16).Action_List.Set_Capacity (7);
            Add_Action (Table.States (16), 18, (44, 0), 35);
            Add_Action (Table.States (16), 30, Reduce, (48, 10),  2);
            Add_Action (Table.States (16), 38, (51, 1), 36);
            Add_Action (Table.States (16), 39, (51, 0), 37);
            Add_Conflict (Table.States (16), 39, (48, 10),  2);
            Add_Action (Table.States (16), 40, (44, 1), 38);
            Add_Action (Table.States (16), 41, (44, 2), 39);
            Add_Action (Table.States (16), 42, Reduce, (48, 10),  2);
            Table.States (16).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (16), 44, 40);
            Add_Goto (Table.States (16), 51, 41);
            Add_Goto (Table.States (16), 52, 42);
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 25, (63, 0), 43);
            Add_Action (Table.States (17), 26, (62, 0), 44);
            Add_Action (Table.States (17), 27, (61, 0), 45);
            Add_Action (Table.States (17), 28, (57, 0), 46);
            Add_Action (Table.States (17), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 39, (58, 1), 47);
            Add_Conflict (Table.States (17), 39, (56, 0),  0);
            Add_Action (Table.States (17), 41, (60, 1), 48);
            Add_Action (Table.States (17), 42, Reduce, (56, 0),  0);
            Table.States (17).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (17), 55, 49);
            Add_Goto (Table.States (17), 56, 50);
            Add_Goto (Table.States (17), 57, 51);
            Add_Goto (Table.States (17), 58, 52);
            Add_Goto (Table.States (17), 59, 53);
            Add_Goto (Table.States (17), 60, 54);
            Add_Goto (Table.States (17), 61, 55);
            Add_Goto (Table.States (17), 62, 56);
            Add_Goto (Table.States (17), 63, 57);
            Table.States (18).Action_List.Set_Capacity (10);
            Add_Action (Table.States (18), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 25, (63, 0), 43);
            Add_Action (Table.States (18), 26, (62, 0), 44);
            Add_Action (Table.States (18), 27, (61, 0), 45);
            Add_Action (Table.States (18), 28, (57, 0), 46);
            Add_Action (Table.States (18), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 39, (58, 1), 47);
            Add_Conflict (Table.States (18), 39, (56, 0),  0);
            Add_Action (Table.States (18), 41, (60, 1), 48);
            Add_Action (Table.States (18), 42, Reduce, (56, 0),  0);
            Table.States (18).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (18), 55, 58);
            Add_Goto (Table.States (18), 56, 50);
            Add_Goto (Table.States (18), 57, 51);
            Add_Goto (Table.States (18), 58, 52);
            Add_Goto (Table.States (18), 59, 53);
            Add_Goto (Table.States (18), 60, 54);
            Add_Goto (Table.States (18), 61, 55);
            Add_Goto (Table.States (18), 62, 56);
            Add_Goto (Table.States (18), 63, 57);
            Table.States (19).Action_List.Set_Capacity (3);
            Add_Action (Table.States (19), (30, 39, 42), (66, 1),  2);
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), (17, 39), (49, 0),  1);
            Table.States (21).Action_List.Set_Capacity (2);
            Add_Action (Table.States (21), 17, (48, 6), 59);
            Add_Action (Table.States (21), 39, (49, 1), 60);
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 39, (45, 2), 61);
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 39, (45, 1), 62);
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 39, (45, 0), 63);
            Table.States (25).Action_List.Set_Capacity (2);
            Add_Action (Table.States (25), (13, 20), (45, 3),  1);
            Table.States (26).Action_List.Set_Capacity (2);
            Add_Action (Table.States (26), (13, 20), (46, 0),  1);
            Table.States (27).Action_List.Set_Capacity (2);
            Add_Action (Table.States (27), 13, (48, 7), 64);
            Add_Action (Table.States (27), 20, (46, 1), 65);
            Table.States (28).Action_List.Set_Capacity (2);
            Add_Action (Table.States (28), 13, (48, 8), 66);
            Add_Action (Table.States (28), 20, (46, 1), 65);
            Table.States (29).Action_List.Set_Capacity (3);
            Add_Action (Table.States (29), (30, 39, 42), (48, 15),  3);
            Table.States (30).Action_List.Set_Capacity (2);
            Add_Action (Table.States (30), 10, (48, 14), 67);
            Add_Action (Table.States (30), 23, (48, 13), 68);
            Table.States (31).Action_List.Set_Capacity (2);
            Add_Action (Table.States (31), 10, (48, 12), 69);
            Add_Action (Table.States (31), 23, (48, 11), 70);
            Table.States (32).Action_List.Set_Capacity (3);
            Add_Action (Table.States (32), 18, (44, 0), 35);
            Add_Action (Table.States (32), 40, (44, 1), 38);
            Add_Action (Table.States (32), 41, (44, 2), 39);
            Table.States (32).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (32), 44, 71);
            Table.States (33).Action_List.Set_Capacity (1);
            Add_Action (Table.States (33), 39, (48, 2), 72);
            Table.States (34).Action_List.Set_Capacity (1);
            Add_Action (Table.States (34), 39, (48, 0), 73);
            Table.States (35).Action_List.Set_Capacity (7);
            Add_Action (Table.States (35), (18, 30, 38, 39, 40, 41, 42), (44, 0),  1);
            Table.States (36).Action_List.Set_Capacity (7);
            Add_Action (Table.States (36), (18, 30, 38, 39, 40, 41, 42), (51, 1),  1);
            Table.States (37).Action_List.Set_Capacity (7);
            Add_Action (Table.States (37), (18, 30, 38, 39, 40, 41, 42), (51, 0),  1);
            Table.States (38).Action_List.Set_Capacity (7);
            Add_Action (Table.States (38), (18, 30, 38, 39, 40, 41, 42), (44, 1),  1);
            Table.States (39).Action_List.Set_Capacity (7);
            Add_Action (Table.States (39), (18, 30, 38, 39, 40, 41, 42), (44, 2),  1);
            Table.States (40).Action_List.Set_Capacity (7);
            Add_Action (Table.States (40), (18, 30, 38, 39, 40, 41, 42), (51, 2),  1);
            Table.States (41).Action_List.Set_Capacity (7);
            Add_Action (Table.States (41), (18, 30, 38, 39, 40, 41, 42), (52, 0),  1);
            Table.States (42).Action_List.Set_Capacity (7);
            Add_Action (Table.States (42), 18, (44, 0), 35);
            Add_Action (Table.States (42), 30, Reduce, (48, 9),  3);
            Add_Action (Table.States (42), 38, (51, 1), 36);
            Add_Action (Table.States (42), 39, (51, 0), 37);
            Add_Conflict (Table.States (42), 39, (48, 9),  3);
            Add_Action (Table.States (42), 40, (44, 1), 38);
            Add_Action (Table.States (42), 41, (44, 2), 39);
            Add_Action (Table.States (42), 42, Reduce, (48, 9),  3);
            Table.States (42).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (42), 44, 40);
            Add_Goto (Table.States (42), 51, 74);
            Table.States (43).Action_List.Set_Capacity (6);
            Add_Action (Table.States (43), 25, (63, 0), 43);
            Add_Action (Table.States (43), 26, (62, 0), 44);
            Add_Action (Table.States (43), 27, (61, 0), 45);
            Add_Action (Table.States (43), 28, (57, 0), 46);
            Add_Action (Table.States (43), 39, (58, 1), 47);
            Add_Action (Table.States (43), 41, (60, 1), 48);
            Table.States (43).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (43), 57, 51);
            Add_Goto (Table.States (43), 58, 52);
            Add_Goto (Table.States (43), 59, 75);
            Add_Goto (Table.States (43), 60, 54);
            Add_Goto (Table.States (43), 61, 55);
            Add_Goto (Table.States (43), 62, 56);
            Add_Goto (Table.States (43), 63, 57);
            Add_Goto (Table.States (43), 64, 76);
            Table.States (44).Action_List.Set_Capacity (6);
            Add_Action (Table.States (44), 25, (63, 0), 43);
            Add_Action (Table.States (44), 26, (62, 0), 44);
            Add_Action (Table.States (44), 27, (61, 0), 45);
            Add_Action (Table.States (44), 28, (57, 0), 46);
            Add_Action (Table.States (44), 39, (58, 1), 47);
            Add_Action (Table.States (44), 41, (60, 1), 48);
            Table.States (44).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (44), 57, 51);
            Add_Goto (Table.States (44), 58, 52);
            Add_Goto (Table.States (44), 59, 75);
            Add_Goto (Table.States (44), 60, 54);
            Add_Goto (Table.States (44), 61, 55);
            Add_Goto (Table.States (44), 62, 56);
            Add_Goto (Table.States (44), 63, 57);
            Add_Goto (Table.States (44), 64, 77);
            Table.States (45).Action_List.Set_Capacity (6);
            Add_Action (Table.States (45), 25, (63, 0), 43);
            Add_Action (Table.States (45), 26, (62, 0), 44);
            Add_Action (Table.States (45), 27, (61, 0), 45);
            Add_Action (Table.States (45), 28, (57, 0), 46);
            Add_Action (Table.States (45), 39, (58, 1), 47);
            Add_Action (Table.States (45), 41, (60, 1), 48);
            Table.States (45).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (45), 57, 51);
            Add_Goto (Table.States (45), 58, 52);
            Add_Goto (Table.States (45), 59, 75);
            Add_Goto (Table.States (45), 60, 54);
            Add_Goto (Table.States (45), 61, 55);
            Add_Goto (Table.States (45), 62, 56);
            Add_Goto (Table.States (45), 63, 57);
            Add_Goto (Table.States (45), 64, 78);
            Table.States (46).Action_List.Set_Capacity (1);
            Add_Action (Table.States (46), 39, (57, 0), 79);
            Table.States (47).Action_List.Set_Capacity (18);
            Add_Action (Table.States (47), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 23, (58, 1), 80);
            Add_Action (Table.States (47), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 31, (63, 4), 81);
            Add_Action (Table.States (47), 32, (62, 2), 82);
            Add_Action (Table.States (47), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 37, (63, 5), 83);
            Add_Action (Table.States (47), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 41, Reduce, (60, 0),  1);
            Add_Action (Table.States (47), 42, Reduce, (60, 0),  1);
            Table.States (48).Action_List.Set_Capacity (15);
            Add_Action (Table.States (48), 19, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 30, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 32, (62, 3), 84);
            Add_Action (Table.States (48), 33, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 34, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 35, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 36, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 41, Reduce, (60, 1),  1);
            Add_Action (Table.States (48), 42, Reduce, (60, 1),  1);
            Table.States (49).Action_List.Set_Capacity (5);
            Add_Action (Table.States (49), 20, (55, 1), 85);
            Add_Action (Table.States (49), 30, (55, 2), 86);
            Add_Conflict (Table.States (49), 30, (54, 1),  0);
            Add_Action (Table.States (49), 36, (54, 0), 87);
            Add_Action (Table.States (49), 39, Reduce, (54, 1),  0);
            Add_Action (Table.States (49), 42, Reduce, (54, 1),  0);
            Table.States (49).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (49), 54, 88);
            Table.States (50).Action_List.Set_Capacity (5);
            Add_Action (Table.States (50), (20, 30, 36, 39, 42), (55, 0),  1);
            Table.States (51).Action_List.Set_Capacity (14);
            Add_Action (Table.States (51), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 2),  1);
            Table.States (52).Action_List.Set_Capacity (14);
            Add_Action (Table.States (52), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (59, 0),  1);
            Table.States (53).Action_List.Set_Capacity (11);
            Add_Action (Table.States (53), 19, (56, 2), 89);
            Add_Action (Table.States (53), 20, Reduce, (56, 1),  1);
            Add_Action (Table.States (53), 25, (63, 0), 43);
            Add_Action (Table.States (53), 26, (62, 0), 44);
            Add_Action (Table.States (53), 27, (61, 0), 45);
            Add_Action (Table.States (53), 28, (57, 0), 46);
            Add_Action (Table.States (53), 30, Reduce, (56, 1),  1);
            Add_Action (Table.States (53), 36, Reduce, (56, 1),  1);
            Add_Action (Table.States (53), 39, (58, 1), 47);
            Add_Conflict (Table.States (53), 39, (56, 1),  1);
            Add_Action (Table.States (53), 41, (60, 1), 48);
            Add_Action (Table.States (53), 42, Reduce, (56, 1),  1);
            Table.States (53).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (53), 57, 51);
            Add_Goto (Table.States (53), 58, 90);
            Add_Goto (Table.States (53), 60, 54);
            Add_Goto (Table.States (53), 61, 55);
            Add_Goto (Table.States (53), 62, 56);
            Add_Goto (Table.States (53), 63, 57);
            Table.States (54).Action_List.Set_Capacity (14);
            Add_Action (Table.States (54), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (58, 0),  1);
            Table.States (55).Action_List.Set_Capacity (14);
            Add_Action (Table.States (55), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 5),  1);
            Table.States (56).Action_List.Set_Capacity (14);
            Add_Action (Table.States (56), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 3),  1);
            Table.States (57).Action_List.Set_Capacity (14);
            Add_Action (Table.States (57), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 4),  1);
            Table.States (58).Action_List.Set_Capacity (5);
            Add_Action (Table.States (58), 20, (55, 1), 85);
            Add_Action (Table.States (58), 30, (55, 2), 86);
            Add_Conflict (Table.States (58), 30, (54, 1),  0);
            Add_Action (Table.States (58), 36, (54, 0), 87);
            Add_Action (Table.States (58), 39, Reduce, (54, 1),  0);
            Add_Action (Table.States (58), 42, Reduce, (54, 1),  0);
            Table.States (58).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (58), 54, 91);
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (59).Action_List.Set_Capacity (3);
            Add_Action (Table.States (59), (30, 39, 42), (48, 6),  4);
            Table.States (60).Action_List.Set_Capacity (2);
            Add_Action (Table.States (60), (17, 39), (49, 1),  2);
            Table.States (61).Action_List.Set_Capacity (2);
            Add_Action (Table.States (61), (13, 20), (45, 2),  2);
            Table.States (62).Action_List.Set_Capacity (2);
            Add_Action (Table.States (62), (13, 20), (45, 1),  2);
            Table.States (63).Action_List.Set_Capacity (2);
            Add_Action (Table.States (63), (13, 20), (45, 0),  2);
            Table.States (64).Action_List.Set_Capacity (1);
            Add_Action (Table.States (64), 16, (48, 7), 92);
            Table.States (65).Action_List.Set_Capacity (4);
            Add_Action (Table.States (65), 3, (45, 2), 22);
            Add_Action (Table.States (65), 14, (45, 1), 23);
            Add_Action (Table.States (65), 15, (45, 0), 24);
            Add_Action (Table.States (65), 39, (45, 3), 25);
            Table.States (65).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (65), 45, 93);
            Table.States (66).Action_List.Set_Capacity (1);
            Add_Action (Table.States (66), 16, (48, 8), 94);
            Table.States (67).Action_List.Set_Capacity (1);
            Add_Action (Table.States (67), 39, (50, 0), 95);
            Table.States (67).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (67), 50, 96);
            Table.States (68).Action_List.Set_Capacity (1);
            Add_Action (Table.States (68), 39, (48, 13), 97);
            Table.States (69).Action_List.Set_Capacity (1);
            Add_Action (Table.States (69), 39, (50, 0), 95);
            Table.States (69).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (69), 50, 98);
            Table.States (70).Action_List.Set_Capacity (1);
            Add_Action (Table.States (70), 39, (48, 11), 99);
            Table.States (71).Action_List.Set_Capacity (3);
            Add_Action (Table.States (71), (30, 39, 42), (48, 5),  4);
            Table.States (72).Action_List.Set_Capacity (1);
            Add_Action (Table.States (72), 24, (48, 2), 100);
            Table.States (73).Action_List.Set_Capacity (1);
            Add_Action (Table.States (73), 24, (48, 0), 101);
            Table.States (74).Action_List.Set_Capacity (7);
            Add_Action (Table.States (74), (18, 30, 38, 39, 40, 41, 42), (52, 1),  2);
            Table.States (75).Action_List.Set_Capacity (10);
            Add_Action (Table.States (75), 20, Reduce, (64, 0),  1);
            Add_Action (Table.States (75), 25, (63, 0), 43);
            Add_Action (Table.States (75), 26, (62, 0), 44);
            Add_Action (Table.States (75), 27, (61, 0), 45);
            Add_Action (Table.States (75), 28, (57, 0), 46);
            Add_Action (Table.States (75), 33, Reduce, (64, 0),  1);
            Add_Action (Table.States (75), 34, Reduce, (64, 0),  1);
            Add_Action (Table.States (75), 35, Reduce, (64, 0),  1);
            Add_Action (Table.States (75), 39, (58, 1), 47);
            Add_Action (Table.States (75), 41, (60, 1), 48);
            Table.States (75).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (75), 57, 51);
            Add_Goto (Table.States (75), 58, 90);
            Add_Goto (Table.States (75), 60, 54);
            Add_Goto (Table.States (75), 61, 55);
            Add_Goto (Table.States (75), 62, 56);
            Add_Goto (Table.States (75), 63, 57);
            Table.States (76).Action_List.Set_Capacity (2);
            Add_Action (Table.States (76), 20, (64, 1), 102);
            Add_Action (Table.States (76), 33, (63, 0), 103);
            Table.States (77).Action_List.Set_Capacity (2);
            Add_Action (Table.States (77), 20, (64, 1), 102);
            Add_Action (Table.States (77), 34, (62, 0), 104);
            Table.States (78).Action_List.Set_Capacity (2);
            Add_Action (Table.States (78), 20, (64, 1), 102);
            Add_Action (Table.States (78), 35, (61, 0), 105);
            Table.States (79).Action_List.Set_Capacity (1);
            Add_Action (Table.States (79), 23, (57, 0), 106);
            Table.States (80).Action_List.Set_Capacity (6);
            Add_Action (Table.States (80), 25, (63, 0), 43);
            Add_Action (Table.States (80), 26, (62, 0), 44);
            Add_Action (Table.States (80), 27, (61, 0), 45);
            Add_Action (Table.States (80), 28, (57, 0), 46);
            Add_Action (Table.States (80), 39, (60, 0), 107);
            Add_Action (Table.States (80), 41, (60, 1), 48);
            Table.States (80).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (80), 57, 51);
            Add_Goto (Table.States (80), 60, 108);
            Add_Goto (Table.States (80), 61, 55);
            Add_Goto (Table.States (80), 62, 56);
            Add_Goto (Table.States (80), 63, 57);
            Table.States (81).Action_List.Set_Capacity (14);
            Add_Action (Table.States (81), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 4),  2);
            Table.States (82).Action_List.Set_Capacity (14);
            Add_Action (Table.States (82), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 2),  2);
            Table.States (83).Action_List.Set_Capacity (14);
            Add_Action (Table.States (83), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 5),  2);
            Table.States (84).Action_List.Set_Capacity (14);
            Add_Action (Table.States (84), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 3),  2);
            Table.States (85).Action_List.Set_Capacity (10);
            Add_Action (Table.States (85), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (85), 25, (63, 0), 43);
            Add_Action (Table.States (85), 26, (62, 0), 44);
            Add_Action (Table.States (85), 27, (61, 0), 45);
            Add_Action (Table.States (85), 28, (57, 0), 46);
            Add_Action (Table.States (85), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (85), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (85), 39, (58, 1), 47);
            Add_Conflict (Table.States (85), 39, (56, 0),  0);
            Add_Action (Table.States (85), 41, (60, 1), 48);
            Add_Action (Table.States (85), 42, Reduce, (56, 0),  0);
            Table.States (85).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (85), 56, 109);
            Add_Goto (Table.States (85), 57, 51);
            Add_Goto (Table.States (85), 58, 52);
            Add_Goto (Table.States (85), 59, 53);
            Add_Goto (Table.States (85), 60, 54);
            Add_Goto (Table.States (85), 61, 55);
            Add_Goto (Table.States (85), 62, 56);
            Add_Goto (Table.States (85), 63, 57);
            Table.States (86).Action_List.Set_Capacity (3);
            Add_Action (Table.States (86), 7, (55, 6), 110);
            Add_Action (Table.States (86), 8, (55, 4), 111);
            Add_Action (Table.States (86), 9, (55, 2), 112);
            Table.States (87).Action_List.Set_Capacity (3);
            Add_Action (Table.States (87), (30, 39, 42), (54, 0),  1);
            Table.States (88).Action_List.Set_Capacity (3);
            Add_Action (Table.States (88), (30, 39, 42), (53, 0),  4);
            Table.States (89).Action_List.Set_Capacity (6);
            Add_Action (Table.States (89), 19, (56, 3), 113);
            Add_Action (Table.States (89), 20, Reduce, (56, 2),  2);
            Add_Action (Table.States (89), 30, Reduce, (56, 2),  2);
            Add_Action (Table.States (89), 36, Reduce, (56, 2),  2);
            Add_Action (Table.States (89), 39, Reduce, (56, 2),  2);
            Add_Action (Table.States (89), 42, Reduce, (56, 2),  2);
            Table.States (90).Action_List.Set_Capacity (14);
            Add_Action (Table.States (90), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (59, 1),  2);
            Table.States (91).Action_List.Set_Capacity (3);
            Add_Action (Table.States (91), (30, 39, 42), (53, 1),  4);
            Table.States (92).Action_List.Set_Capacity (2);
            Add_Action (Table.States (92), 39, (47, 0), 114);
            Add_Action (Table.States (92), 41, (47, 1), 115);
            Table.States (92).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (92), 47, 116);
            Table.States (93).Action_List.Set_Capacity (2);
            Add_Action (Table.States (93), (13, 20), (46, 1),  3);
            Table.States (94).Action_List.Set_Capacity (2);
            Add_Action (Table.States (94), 39, (47, 0), 114);
            Add_Action (Table.States (94), 41, (47, 1), 115);
            Table.States (94).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (94), 47, 117);
            Table.States (95).Action_List.Set_Capacity (5);
            Add_Action (Table.States (95), (20, 30, 36, 39, 42), (50, 0),  1);
            Table.States (96).Action_List.Set_Capacity (4);
            Add_Action (Table.States (96), 20, (50, 1), 118);
            Add_Action (Table.States (96), 30, Reduce, (48, 14),  5);
            Add_Action (Table.States (96), 39, Reduce, (48, 14),  5);
            Add_Action (Table.States (96), 42, Reduce, (48, 14),  5);
            Table.States (97).Action_List.Set_Capacity (3);
            Add_Action (Table.States (97), (30, 39, 42), (48, 13),  5);
            Table.States (98).Action_List.Set_Capacity (4);
            Add_Action (Table.States (98), 20, (50, 1), 118);
            Add_Action (Table.States (98), 30, Reduce, (48, 12),  5);
            Add_Action (Table.States (98), 39, Reduce, (48, 12),  5);
            Add_Action (Table.States (98), 42, Reduce, (48, 12),  5);
            Table.States (99).Action_List.Set_Capacity (3);
            Add_Action (Table.States (99), (30, 39, 42), (48, 11),  5);
            Table.States (100).Action_List.Set_Capacity (1);
            Add_Action (Table.States (100), 39, (48, 2), 119);
            Table.States (101).Action_List.Set_Capacity (1);
            Add_Action (Table.States (101), 39, (48, 0), 120);
            Table.States (102).Action_List.Set_Capacity (6);
            Add_Action (Table.States (102), 25, (63, 0), 43);
            Add_Action (Table.States (102), 26, (62, 0), 44);
            Add_Action (Table.States (102), 27, (61, 0), 45);
            Add_Action (Table.States (102), 28, (57, 0), 46);
            Add_Action (Table.States (102), 39, (58, 1), 47);
            Add_Action (Table.States (102), 41, (60, 1), 48);
            Table.States (102).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (102), 57, 51);
            Add_Goto (Table.States (102), 58, 52);
            Add_Goto (Table.States (102), 59, 121);
            Add_Goto (Table.States (102), 60, 54);
            Add_Goto (Table.States (102), 61, 55);
            Add_Goto (Table.States (102), 62, 56);
            Add_Goto (Table.States (102), 63, 57);
            Table.States (103).Action_List.Set_Capacity (15);
            Add_Action (Table.States (103), 19, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 29, (63, 1), 122);
            Add_Action (Table.States (103), 30, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 33, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 34, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 35, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 36, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 41, Reduce, (63, 0),  3);
            Add_Action (Table.States (103), 42, Reduce, (63, 0),  3);
            Table.States (104).Action_List.Set_Capacity (14);
            Add_Action (Table.States (104), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 0),  3);
            Table.States (105).Action_List.Set_Capacity (17);
            Add_Action (Table.States (105), 19, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 30, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 31, (63, 2), 123);
            Add_Action (Table.States (105), 32, (62, 1), 124);
            Add_Action (Table.States (105), 33, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 34, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 35, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 36, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 37, (63, 3), 125);
            Add_Action (Table.States (105), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 41, Reduce, (61, 0),  3);
            Add_Action (Table.States (105), 42, Reduce, (61, 0),  3);
            Table.States (106).Action_List.Set_Capacity (1);
            Add_Action (Table.States (106), 39, (57, 0), 126);
            Table.States (107).Action_List.Set_Capacity (17);
            Add_Action (Table.States (107), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 31, (63, 4), 81);
            Add_Action (Table.States (107), 32, (62, 2), 82);
            Add_Action (Table.States (107), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 37, (63, 5), 83);
            Add_Action (Table.States (107), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 41, Reduce, (60, 0),  1);
            Add_Action (Table.States (107), 42, Reduce, (60, 0),  1);
            Table.States (108).Action_List.Set_Capacity (14);
            Add_Action (Table.States (108), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (58, 1),  3);
            Table.States (109).Action_List.Set_Capacity (5);
            Add_Action (Table.States (109), (20, 30, 36, 39, 42), (55, 1),  3);
            Table.States (110).Action_List.Set_Capacity (1);
            Add_Action (Table.States (110), 9, (55, 6), 127);
            Table.States (111).Action_List.Set_Capacity (1);
            Add_Action (Table.States (111), 39, (55, 4), 128);
            Table.States (112).Action_List.Set_Capacity (1);
            Add_Action (Table.States (112), 39, (55, 2), 129);
            Table.States (113).Action_List.Set_Capacity (5);
            Add_Action (Table.States (113), (20, 30, 36, 39, 42), (56, 3),  3);
            Table.States (114).Action_List.Set_Capacity (4);
            Add_Action (Table.States (114), (21, 30, 39, 42), (47, 0),  1);
            Table.States (115).Action_List.Set_Capacity (4);
            Add_Action (Table.States (115), (21, 30, 39, 42), (47, 1),  1);
            Table.States (116).Action_List.Set_Capacity (3);
            Add_Action (Table.States (116), (30, 39, 42), (48, 7),  6);
            Table.States (117).Action_List.Set_Capacity (1);
            Add_Action (Table.States (117), 21, (48, 8), 130);
            Table.States (118).Action_List.Set_Capacity (1);
            Add_Action (Table.States (118), 39, (50, 1), 131);
            Table.States (119).Action_List.Set_Capacity (6);
            Add_Action (Table.States (119), 18, (44, 0), 35);
            Add_Action (Table.States (119), 30, Reduce, (48, 4),  6);
            Add_Action (Table.States (119), 39, Reduce, (48, 4),  6);
            Add_Action (Table.States (119), 40, (44, 1), 38);
            Add_Action (Table.States (119), 41, (44, 2), 39);
            Add_Action (Table.States (119), 42, Reduce, (48, 4),  6);
            Table.States (119).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (119), 44, 132);
            Table.States (120).Action_List.Set_Capacity (3);
            Add_Action (Table.States (120), 18, (44, 0), 35);
            Add_Action (Table.States (120), 40, (44, 1), 38);
            Add_Action (Table.States (120), 41, (44, 2), 39);
            Table.States (120).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (120), 44, 133);
            Table.States (121).Action_List.Set_Capacity (10);
            Add_Action (Table.States (121), 20, Reduce, (64, 1),  3);
            Add_Action (Table.States (121), 25, (63, 0), 43);
            Add_Action (Table.States (121), 26, (62, 0), 44);
            Add_Action (Table.States (121), 27, (61, 0), 45);
            Add_Action (Table.States (121), 28, (57, 0), 46);
            Add_Action (Table.States (121), 33, Reduce, (64, 1),  3);
            Add_Action (Table.States (121), 34, Reduce, (64, 1),  3);
            Add_Action (Table.States (121), 35, Reduce, (64, 1),  3);
            Add_Action (Table.States (121), 39, (58, 1), 47);
            Add_Action (Table.States (121), 41, (60, 1), 48);
            Table.States (121).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (121), 57, 51);
            Add_Goto (Table.States (121), 58, 90);
            Add_Goto (Table.States (121), 60, 54);
            Add_Goto (Table.States (121), 61, 55);
            Add_Goto (Table.States (121), 62, 56);
            Add_Goto (Table.States (121), 63, 57);
            Table.States (122).Action_List.Set_Capacity (14);
            Add_Action (Table.States (122), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 1),  4);
            Table.States (123).Action_List.Set_Capacity (14);
            Add_Action (Table.States (123), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 2),  4);
            Table.States (124).Action_List.Set_Capacity (14);
            Add_Action (Table.States (124), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 1),  4);
            Table.States (125).Action_List.Set_Capacity (14);
            Add_Action (Table.States (125), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 3),  4);
            Table.States (126).Action_List.Set_Capacity (1);
            Add_Action (Table.States (126), 24, (57, 0), 134);
            Table.States (127).Action_List.Set_Capacity (5);
            Add_Action (Table.States (127), (20, 30, 36, 39, 42), (55, 6),  4);
            Table.States (128).Action_List.Set_Capacity (2);
            Add_Action (Table.States (128), 10, (55, 5), 135);
            Add_Action (Table.States (128), 23, (55, 4), 136);
            Table.States (129).Action_List.Set_Capacity (2);
            Add_Action (Table.States (129), 10, (55, 3), 137);
            Add_Action (Table.States (129), 23, (55, 2), 138);
            Table.States (130).Action_List.Set_Capacity (1);
            Add_Action (Table.States (130), 39, (48, 8), 139);
            Table.States (131).Action_List.Set_Capacity (5);
            Add_Action (Table.States (131), (20, 30, 36, 39, 42), (50, 1),  3);
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (132).Action_List.Set_Capacity (6);
            Add_Action (Table.States (132), 18, (44, 0), 35);
            Add_Action (Table.States (132), 30, Reduce, (48, 3),  7);
            Add_Action (Table.States (132), 39, Reduce, (48, 3),  7);
            Add_Action (Table.States (132), 40, (44, 1), 38);
            Add_Action (Table.States (132), 41, (44, 2), 39);
            Add_Action (Table.States (132), 42, Reduce, (48, 3),  7);
            Table.States (132).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (132), 44, 140);
            Table.States (133).Action_List.Set_Capacity (6);
            Add_Action (Table.States (133), 18, (44, 0), 35);
            Add_Action (Table.States (133), 30, Reduce, (48, 1),  7);
            Add_Action (Table.States (133), 39, Reduce, (48, 1),  7);
            Add_Action (Table.States (133), 40, (44, 1), 38);
            Add_Action (Table.States (133), 41, (44, 2), 39);
            Add_Action (Table.States (133), 42, Reduce, (48, 1),  7);
            Table.States (133).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (133), 44, 141);
            Table.States (134).Action_List.Set_Capacity (14);
            Add_Action (Table.States (134), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (57, 0),  5);
            Table.States (135).Action_List.Set_Capacity (1);
            Add_Action (Table.States (135), 39, (50, 0), 95);
            Table.States (135).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (135), 50, 142);
            Table.States (136).Action_List.Set_Capacity (1);
            Add_Action (Table.States (136), 39, (55, 4), 143);
            Table.States (137).Action_List.Set_Capacity (1);
            Add_Action (Table.States (137), 39, (50, 0), 95);
            Table.States (137).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (137), 50, 144);
            Table.States (138).Action_List.Set_Capacity (1);
            Add_Action (Table.States (138), 39, (55, 2), 145);
            Table.States (139).Action_List.Set_Capacity (3);
            Add_Action (Table.States (139), (30, 39, 42), (48, 8),  8);
            Table.States (140).Action_List.Set_Capacity (3);
            Add_Action (Table.States (140), (30, 39, 42), (48, 2),  8);
            Table.States (141).Action_List.Set_Capacity (3);
            Add_Action (Table.States (141), (30, 39, 42), (48, 0),  8);
            Table.States (142).Action_List.Set_Capacity (5);
            Add_Action (Table.States (142), 20, (50, 1), 118);
            Add_Conflict (Table.States (142), 20, (55, 5),  6);
            Add_Action (Table.States (142), 30, Reduce, (55, 5),  6);
            Add_Action (Table.States (142), 36, Reduce, (55, 5),  6);
            Add_Action (Table.States (142), 39, Reduce, (55, 5),  6);
            Add_Action (Table.States (142), 42, Reduce, (55, 5),  6);
            Table.States (143).Action_List.Set_Capacity (5);
            Add_Action (Table.States (143), (20, 30, 36, 39, 42), (55, 4),  6);
            Table.States (144).Action_List.Set_Capacity (5);
            Add_Action (Table.States (144), 20, (50, 1), 118);
            Add_Conflict (Table.States (144), 20, (55, 3),  6);
            Add_Action (Table.States (144), 30, Reduce, (55, 3),  6);
            Add_Action (Table.States (144), 36, Reduce, (55, 3),  6);
            Add_Action (Table.States (144), 39, Reduce, (55, 3),  6);
            Add_Action (Table.States (144), 42, Reduce, (55, 3),  6);
            Table.States (145).Action_List.Set_Capacity (5);
            Add_Action (Table.States (145), (20, 30, 36, 39, 42), (55, 2),  6);
         end Subr_3;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      Table.Max_Parallel := 15;
      return Table;
   end Create_Parse_Table;

   function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector
   is begin
      return Result : WisiToken.Syntax_Trees.Production_Info_Trees.Vector do
         Result.Set_First_Last (43, 66);
         Result (48).RHSs.Set_First_Last (0, 15);
         Result (48).RHSs (0).In_Parse_Action := null;
         Result (48).RHSs (0).Post_Parse_Action := declaration_0'Access;
         Result (48).RHSs (1).In_Parse_Action := null;
         Result (48).RHSs (1).Post_Parse_Action := declaration_1'Access;
         Result (48).RHSs (2).In_Parse_Action := null;
         Result (48).RHSs (2).Post_Parse_Action := declaration_2'Access;
         Result (48).RHSs (3).In_Parse_Action := null;
         Result (48).RHSs (3).Post_Parse_Action := declaration_3'Access;
         Result (48).RHSs (4).In_Parse_Action := null;
         Result (48).RHSs (4).Post_Parse_Action := declaration_4'Access;
         Result (48).RHSs (5).In_Parse_Action := null;
         Result (48).RHSs (5).Post_Parse_Action := declaration_5'Access;
         Result (48).RHSs (6).In_Parse_Action := null;
         Result (48).RHSs (6).Post_Parse_Action := declaration_6'Access;
         Result (48).RHSs (7).In_Parse_Action := null;
         Result (48).RHSs (7).Post_Parse_Action := declaration_7'Access;
         Result (48).RHSs (8).In_Parse_Action := null;
         Result (48).RHSs (8).Post_Parse_Action := declaration_8'Access;
         Result (48).RHSs (9).In_Parse_Action := null;
         Result (48).RHSs (9).Post_Parse_Action := declaration_9'Access;
         Result (48).RHSs (10).In_Parse_Action := null;
         Result (48).RHSs (10).Post_Parse_Action := declaration_10'Access;
         Result (48).RHSs (11).In_Parse_Action := null;
         Result (48).RHSs (11).Post_Parse_Action := declaration_11'Access;
         Result (48).RHSs (12).In_Parse_Action := null;
         Result (48).RHSs (12).Post_Parse_Action := declaration_12'Access;
         Result (48).RHSs (13).In_Parse_Action := null;
         Result (48).RHSs (13).Post_Parse_Action := declaration_13'Access;
         Result (48).RHSs (14).In_Parse_Action := null;
         Result (48).RHSs (14).Post_Parse_Action := declaration_14'Access;
         Result (48).RHSs (15).In_Parse_Action := null;
         Result (48).RHSs (15).Post_Parse_Action := declaration_15'Access;
         Result (53).RHSs.Set_First_Last (0, 1);
         Result (53).RHSs (0).In_Parse_Action := null;
         Result (53).RHSs (0).Post_Parse_Action := nonterminal_0'Access;
         Result (53).RHSs (1).In_Parse_Action := null;
         Result (53).RHSs (1).Post_Parse_Action := nonterminal_1'Access;
         Result (60).RHSs.Set_First_Last (0, 5);
         Result (60).RHSs (0).In_Parse_Action := null;
         Result (60).RHSs (0).Post_Parse_Action := null;
         Result (60).RHSs (1).In_Parse_Action := null;
         Result (60).RHSs (1).Post_Parse_Action := rhs_item_1'Access;
         Result (60).RHSs (2).In_Parse_Action := null;
         Result (60).RHSs (2).Post_Parse_Action := rhs_item_2'Access;
         Result (60).RHSs (3).In_Parse_Action := null;
         Result (60).RHSs (3).Post_Parse_Action := rhs_item_3'Access;
         Result (60).RHSs (4).In_Parse_Action := null;
         Result (60).RHSs (4).Post_Parse_Action := rhs_item_4'Access;
         Result (60).RHSs (5).In_Parse_Action := null;
         Result (60).RHSs (5).Post_Parse_Action := rhs_item_5'Access;
         Result (62).RHSs.Set_First_Last (0, 3);
         Result (62).RHSs (0).In_Parse_Action := null;
         Result (62).RHSs (0).Post_Parse_Action := null;
         Result (62).RHSs (1).In_Parse_Action := null;
         Result (62).RHSs (1).Post_Parse_Action := null;
         Result (62).RHSs (2).In_Parse_Action := null;
         Result (62).RHSs (2).Post_Parse_Action := null;
         Result (62).RHSs (3).In_Parse_Action := null;
         Result (62).RHSs (3).Post_Parse_Action := rhs_optional_item_3'Access;
      end return;
   end Create_Productions;

   function Create_Parser
     (Trace      : in WisiToken.Trace_Access;
      User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.LR.Parser_No_Recover.Parser
   is begin
      return Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser do
         Parser.Tree.Lexer := Lexer.New_Lexer (Trace, Wisitoken_Grammar_Actions.Descriptor'Access);
         Parser.Productions := Create_Productions;
         Parser.User_Data := User_Data;
         Parser.Table := Create_Parse_Table;
      end return;
   end Create_Parser;
end Wisitoken_Grammar_Main;
