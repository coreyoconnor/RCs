--  Abstract :
--
--  Base utilities for McKenzie_Recover
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

with Ada.Exceptions;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
package WisiToken.Parse.LR.McKenzie_Recover.Base is

   type Recover_State is (Active, Ready, Success, Fail);

   type Parser_Status is record
      Recover_State : Base.Recover_State;
      Parser_State  : Parser_Lists.State_Access;
      Fail_Mode     : Recover_Status;
   end record;

   type Parser_Status_Array is array (SAL.Peek_Type range <>) of Parser_Status;

   type Supervisor (Parser_Count : SAL.Peek_Type) is tagged limited private;

   procedure Initialize
     (Super         : in out Supervisor;
      Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser);

   procedure Get
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  :    out SAL.Base_Peek_Type;
      Config        :    out Configuration);
   --  Get a new configuration to check. If Parser_Index =
   --  SAL.Base_Peek_Type'First, Config is invalid; there are no
   --  configurations left to check.

   procedure Success
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  : in     SAL.Peek_Type;
      Config        : in     Configuration;
      Configs       : in out Config_Heaps.Heap_Type);
   --  Report that Configuration succeeds for Parser_Label, and enqueue
   --  Configs.

   procedure Put
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  : in     SAL.Peek_Type;
      Configs       : in out Config_Heaps.Heap_Type);
   --  Add Configs to the McKenzie_Data Config_Heap for Parser_Label

   procedure Config_Full
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Prefix        : in     String;
      Parser_Index  : in     SAL.Peek_Type);
   --  Report that a config.ops was full when trying to add another op.
   --  This is counted towards the enqueue limit.

   function Recover_Result (Super : in Supervisor) return Recover_Status;

   function Done (Super : in Supervisor) return Boolean;
   --  True when all parsers have failed or succeeded.

   procedure Finish
     (Super         : in out Supervisor;
      Shared_Parser : in out Parser.Parser);

   function Parser_State
     (Super        : in Supervisor;
      Parser_Index : in SAL.Peek_Type)
     return Parser_Lists.Constant_Reference_Type;

   function Stream (Super : in Supervisor; Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_ID;

   procedure Extend_Sequential_Index
     (Super         : in out Supervisor;
      Shared_Parser : in out Parser.Parser;
      Thru          : in     Syntax_Trees.Valid_Node_Access;
      Positive      : in     Boolean)
   with Pre => Shared_Parser.Tree.Is_Terminal (Thru),
     Post => Shared_Parser.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index;
   --  If Thru.Node has valid Sequential_Index, return.
   --
   --  Else extend Sequential_Index range thru Thru; if Positive, towards
   --  EOI, else towards SOI.

   procedure Extend_Sequential_Index
     (Super         : in out Supervisor;
      Shared_Parser : in out Parser.Parser;
      Thru          : in     Syntax_Trees.Sequential_Index);
   --  Ensure Sequential_Index range includes Thru, or SOI/EOI.

   procedure Put
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser;
      Message       : in String;
      Parser_Index  : in SAL.Peek_Type;
      Config        : in Configuration);

private

   type Supervisor (Parser_Count : SAL.Peek_Type) is tagged limited
   record
      All_Parsers_Done        : Boolean                     := False;
      Success_Counter         : Natural                     := 0;
      Min_Success_Check_Count : Natural                     := Natural'Last;
      Total_Enqueue_Count     : Natural                     := 0;
      Fatal_Called            : Boolean                     := False;
      Error_ID                : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
      Error_Message           : Ada.Strings.Unbounded.Unbounded_String;
      Parser_Status           : Parser_Status_Array (1 .. Parser_Count);

      Min_Sequential_Indices : Syntax_Trees.Stream_Node_Parents_Array (1 .. Parser_Count);
      Max_Sequential_Indices : Syntax_Trees.Stream_Node_Parents_Array (1 .. Parser_Count);
   end record;

   function Min_Sequential_Index (Super : in Supervisor) return Syntax_Trees.Stream_Node_Parents_Array
   is (Super.Min_Sequential_Indices);

   function Max_Sequential_Index (Super : in Supervisor) return Syntax_Trees.Stream_Node_Parents_Array
   is (Super.Max_Sequential_Indices);

end WisiToken.Parse.LR.McKenzie_Recover.Base;
