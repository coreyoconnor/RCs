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

package body WisiToken.Parse.LR.McKenzie_Recover.Base is

   Default_Positive_Sequential_Index : constant Syntax_Trees.Sequential_Index := 10;
   Default_Negative_Sequential_Index : constant Syntax_Trees.Sequential_Index := -10;

   procedure Initialize
     (Super         : in out Supervisor;
      Shared_Parser : in out Parser.Parser)
   is
      Tree : WisiToken.Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      declare
         Index : SAL.Peek_Type := 1;
      begin
         for Cur in Shared_Parser.Parsers.Iterate loop
            pragma Assert (Shared_Parser.Parsers (Cur).Current_Recover_Op = No_Insert_Delete);
            --  Otherwise previous error recovery resume not finished; this is supposed to
            --  be checked in Parser.

            Super.Parser_Status (Index) :=
              (Recover_State  => Active,
               Parser_State   => Parser_Lists.Unchecked_State_Ref (Cur),
               Fail_Mode      => Success);

            declare
               Data : McKenzie_Data renames Shared_Parser.Parsers (Cur).Recover;
            begin
               Data.Config_Heap.Clear;
               Data.Results.Clear;
               Data.Enqueue_Count := 0;
               Data.Check_Count   := 0;
               Data.Success       := False;
            end;

            Index := Index + 1;
         end loop;
      end;

      --  Set Sequential_Index. Ensure that all of the error node in each
      --  parser is indexed (ada_mode-recover_incremental_02.adb). After
      --  that, Push_Back extends towards SOI, and everything else towards
      --  EOI.
      declare
         use Syntax_Trees;
         Streams     : Stream_ID_Array (1 .. Shared_Parser.Parsers.Count);
         First_Nodes : Valid_Node_Access_Array (1 .. Super.Parser_Count) := (others => Dummy_Node);
         Last_Nodes  : Valid_Node_Access_Array (1 .. Super.Parser_Count) := (others => Dummy_Node);

         Min_Target : Sequential_Index := Default_Negative_Sequential_Index;
         Max_Target : Sequential_Index := Default_Positive_Sequential_Index;
         Min_Done   : Boolean          := False;
         Max_Done   : Boolean          := False;
      begin
         for I in Last_Nodes'Range loop
            pragma Assert
              (not Tree.Is_Empty_Nonterm (Tree.Current_Token (Super.Parser_Status (I).Parser_State.Stream).Node));

            First_Nodes (I) := Tree.First_Terminal
              (Tree.Current_Token (Super.Parser_Status (I).Parser_State.Stream).Node);

            Last_Nodes (I) := Tree.Last_Terminal
              (Tree.Current_Token (Super.Parser_Status (I).Parser_State.Stream).Node);
         end loop;

         Set_Initial_Sequential_Index
           (Shared_Parser.Parsers, Tree, Streams, Super.Max_Sequential_Indices, Initialize => True);

         Super.Min_Sequential_Indices := Super.Max_Sequential_Indices;

         if (for all Term of Super.Max_Sequential_Indices => Tree.ID (Term.Ref.Node) = Tree.Lexer.Descriptor.EOI_ID)
         then
            Max_Done := True;
         end if;

         if (for all Term of Super.Min_Sequential_Indices => Tree.ID (Term.Ref.Node) = Tree.Lexer.Descriptor.SOI_ID)
         then
            Min_Done := True;
         end if;

         loop
            if not Max_Done then
               Extend_Sequential_Index
                 (Tree, Streams, Super.Max_Sequential_Indices,
                  Target   => Max_Target,
                  Positive => True,
                  Clear    => False);
            end if;

            if not Min_Done then
               Extend_Sequential_Index
                 (Tree, Streams, Super.Min_Sequential_Indices,
                  Target   => Min_Target,
                  Positive => False,
                  Clear    => False);
            end if;

            if (for some Node of First_Nodes => Tree.Get_Sequential_Index (Node) = Invalid_Sequential_Index) then
               Min_Target := 2 * @;
            else
               Min_Done := True;
            end if;

            if (for some Node of Last_Nodes  => Tree.Get_Sequential_Index (Node) = Invalid_Sequential_Index) then
               Max_Target := 2 * @;
            else
               Max_Done := True;
            end if;

            exit when Min_Done and Max_Done;
         end loop;
      end;
   end Initialize;

   procedure Get
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  :    out SAL.Base_Peek_Type;
      Config        :    out Configuration)
   is
      Done_Count     : SAL.Base_Peek_Type := 0;
      Skip           : Boolean;
      Min_Cost       : Integer            := Integer'Last;
      Min_Cost_Index : SAL.Base_Peek_Type;

      procedure Set_Outputs (I : in SAL.Peek_Type)
      is begin
         Parser_Index := I;
         Config       := Super.Parser_Status (I).Parser_State.Recover.Config_Heap.Remove;

         Super.Parser_Status (I).Parser_State.Recover.Check_Count :=
           Super.Parser_Status (I).Parser_State.Recover.Check_Count + 1;
      end Set_Outputs;

      procedure Set_All_Done
      is begin
         Parser_Index := SAL.Base_Peek_Type'First;

         pragma Warnings (Off, "aggregate not fully initialized");
         --  Config.Stack.Data is not initialized, but no uninitialized data is
         --  ever referenced.
         Config := (others => <>);
         pragma Warnings (On, "aggregate not fully initialized");
      end Set_All_Done;

   begin
      if Super.All_Parsers_Done then
         Set_All_Done;
         return;
      end if;

      --  Same logic as in Get_Barrier, but different actions.
      --
      --  No task_id in outline trace messages, because they may appear in
      --  .parse_good
      for I in Super.Parser_Status'Range loop
         Skip := False;

         declare
            P_Status          : Base.Parser_Status renames Super.Parser_Status (I);
            Check_Delta_Limit : Natural renames Shared_Parser.Table.McKenzie_Param.Check_Delta_Limit;
         begin
            case P_Status.Recover_State is
            when Active | Ready =>
               if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                  if P_Status.Parser_State.Recover.Check_Count - Check_Delta_Limit >= Super.Min_Success_Check_Count then
                     if Trace_McKenzie > Outline then
                        Put_Line
                          (Shared_Parser.Tree,
                           P_Status.Parser_State.Stream, "fail; check delta (limit" &
                             Integer'Image (Super.Min_Success_Check_Count + Check_Delta_Limit) & ")");
                     end if;
                     P_Status.Recover_State := Fail;
                     P_Status.Fail_Mode     := Fail_Check_Delta;

                     Done_Count := Done_Count + 1;
                     Skip := True;

                  elsif Super.Total_Enqueue_Count + P_Status.Parser_State.Recover.Config_Full_Count >=
                    Shared_Parser.Table.McKenzie_Param.Enqueue_Limit
                  then
                     if Trace_McKenzie > Outline then
                        Put_Line
                          (Shared_Parser.Tree,
                           P_Status.Parser_State.Stream, "fail; total enqueue limit (" &
                             Shared_Parser.Table.McKenzie_Param.Enqueue_Limit'Image & " cost" &
                             P_Status.Parser_State.Recover.Config_Heap.Min_Key'Image & ")");
                     end if;
                     P_Status.Recover_State := Fail;
                     P_Status.Fail_Mode     := Fail_Enqueue_Limit;

                     Done_Count := Done_Count + 1;
                     Skip := True;
                  end if;
               end if;

               if not Skip then
                  case P_Status.Recover_State is
                  when Active =>
                     if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                        if P_Status.Parser_State.Recover.Config_Heap.Min_Key < Min_Cost then
                           Min_Cost       := P_Status.Parser_State.Recover.Config_Heap.Min_Key;
                           Min_Cost_Index := I;
                           --  not done
                        end if;
                     else
                        --  No configs left to check (rarely happens with real languages).
                        if Trace_McKenzie > Outline then
                           Put_Line
                             (Shared_Parser.Tree, P_Status.Parser_State.Stream,
                              "fail; no configs left");
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_No_Configs_Left;

                        Done_Count := Done_Count + 1;
                     end if;

                  when Ready =>
                     if P_Status.Parser_State.Recover.Config_Heap.Count > 0 and then
                       P_Status.Parser_State.Recover.Config_Heap.Min_Key <=
                       P_Status.Parser_State.Recover.Results.Min_Key
                     then
                        --  Still more to check. We don't check Min_Cost here so this parser
                        --  can finish quickly.
                        Set_Outputs (I);
                        return;

                     else
                        P_Status.Recover_State := Success;
                        Done_Count             := Done_Count + 1;
                     end if;
                  when others =>
                     null;
                  end case;
               end if;

            when Success | Fail =>
               Done_Count := Done_Count + 1;
            end case;
         end;
      end loop;

      if Min_Cost /= Integer'Last then
         Set_Outputs (Min_Cost_Index);

      elsif Done_Count = Super.Parser_Count then
         if Trace_McKenzie > Extra then
            Shared_Parser.Tree.Lexer.Trace.Put_Line
              ("Supervisor: done, " & (if Super.Success_Counter > 0 then "succeed" else "fail"));
         end if;

         Set_All_Done;
         Super.All_Parsers_Done := True;
      else
         raise SAL.Programmer_Error with "Get_Barrier and Get logic do not match";
      end if;
   end Get;

   procedure Success
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  : in     SAL.Peek_Type;
      Config        : in     Configuration;
      Configs       : in out Config_Heaps.Heap_Type)
   is
      Data : McKenzie_Data renames Super.Parser_Status (Parser_Index).Parser_State.Recover;
   begin
      Super.Put (Shared_Parser, Parser_Index, Configs);

      if Trace_McKenzie > Detail then
         Put
           ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) & ", check " & Integer'Image (Data.Check_Count),
            Shared_Parser.Tree, Super.Parser_Status
              (Parser_Index).Parser_State.Stream,
            Config);
      end if;

      Super.Success_Counter := @ + 1;
      Data.Success          := True;

      if Force_Full_Explore then
         Data.Results.Add (Config);
         return;
      end if;

      if Data.Check_Count < Super.Min_Success_Check_Count then
         Super.Min_Success_Check_Count := Data.Check_Count;
      end if;

      if Force_High_Cost_Solutions then
         Data.Results.Add (Config);
         if Data.Results.Count > 3 then
            Super.Parser_Status (Parser_Index).Recover_State := Ready;
         end if;
      else
         if Data.Results.Count = 0 then
            Data.Results.Add (Config);

            Super.Parser_Status (Parser_Index).Recover_State := Ready;

         elsif Config.Cost < Data.Results.Min_Key then
            --  delete higher cost configs from Results
            loop
               Data.Results.Drop;
               exit when Data.Results.Count = 0 or else
                 Config.Cost >= Data.Results.Min_Key;
            end loop;

            Data.Results.Add (Config);

         elsif Config.Cost = Data.Results.Min_Key then
            Data.Results.Add (Config);

         else
            --  Config.Cost > Results.Min_Key
            null;
         end if;
      end if;
   end Success;

   procedure Put
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Parser_Index  : in     SAL.Peek_Type;
      Configs       : in out Config_Heaps.Heap_Type)
   is
      Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

      P_Status : Base.Parser_Status renames Super.Parser_Status (Parser_Index);
      Data     : McKenzie_Data renames P_Status.Parser_State.Recover;
   begin
      Super.Total_Enqueue_Count := @ + Integer (Configs_Count);
      Data.Enqueue_Count        := @ + Integer (Configs_Count);
      loop
         exit when Configs.Count = 0;

         Data.Config_Heap.Add (Configs.Remove);
      end loop;

      if Trace_McKenzie > Detail then
         Put_Line
           (Shared_Parser.Tree, P_Status.Parser_State.Stream,
            "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
              "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
              "/" & Trimmed_Image (Super.Total_Enqueue_Count) &
              "/" & Trimmed_Image (Data.Check_Count));
      end if;
   end Put;

   procedure Config_Full
     (Super         : in out Supervisor;
      Shared_Parser : in     Parser.Parser;
      Prefix        : in     String;
      Parser_Index  : in     SAL.Peek_Type)
   is
      P_Status : Base.Parser_Status renames Super.Parser_Status (Parser_Index);
      Data     : McKenzie_Data renames P_Status.Parser_State.Recover;
   begin
      Data.Config_Full_Count := Data.Config_Full_Count + 1;
      if Trace_McKenzie > Outline then
         Put_Line
           (Shared_Parser.Tree, Super.Stream (Parser_Index),
            Prefix & ": config.ops is full; " & Data.Config_Full_Count'Image);
      end if;
   end Config_Full;

   function Recover_Result (Super : in Supervisor) return Recover_Status
   is
      Temp : Recover_Status := Recover_Status'First;
   begin
      for S of Super.Parser_Status loop
         if S.Parser_State.Recover.Success then
            --  Can succeed while also exceeding max_enqueue_limit;
            --  test_mckenzie.adb Missing_Name_6 LR1, or while exceeding
            --  Check_Delta_Limit; ada_mode-recover_32.adb.
            Temp := Success;
         else
            Temp := Recover_Status'Max (Temp, S.Fail_Mode);
         end if;
      end loop;
      return Temp;
   end Recover_Result;

   function Done (Super : in Supervisor) return Boolean
   is begin
      return Super.All_Parsers_Done;
   end Done;

   function Min_Sequential_Index
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser)
     return Syntax_Trees.Sequential_Index
   is
      use WisiToken.Syntax_Trees;
   begin
      return Result : Sequential_Index := Sequential_Index'Last do
         for I in 1 .. Super.Parser_Count loop
            Result := Sequential_Index'Min
              (@,  Shared_Parser.Tree.Get_Sequential_Index (Super.Min_Sequential_Indices (I).Ref.Node));
         end loop;
      end return;
   end Min_Sequential_Index;

   function Max_Sequential_Index
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser)
     return Syntax_Trees.Sequential_Index
   is
      use Syntax_Trees;
   begin
      return Result : Sequential_Index := Sequential_Index'First do
         for I in Super.Max_Sequential_Indices'Range loop
            Result := Sequential_Index'Max
              (@,  Shared_Parser.Tree.Get_Sequential_Index (Super.Max_Sequential_Indices (I).Ref.Node));
         end loop;
      end return;
   end Max_Sequential_Index;

   procedure Finish
     (Super         : in out Supervisor;
      Shared_Parser : in out Parser.Parser)
   is begin
      --  Allow streams to be terminated.
      Shared_Parser.Min_Sequential_Index   := Super.Min_Sequential_Index (Shared_Parser);
      Shared_Parser.Max_Sequential_Index   := Super.Max_Sequential_Index (Shared_Parser);
      Super.Min_Sequential_Indices := (others => Syntax_Trees.Invalid_Stream_Node_Parents);
      Super.Max_Sequential_Indices := (others => Syntax_Trees.Invalid_Stream_Node_Parents);

      if Trace_McKenzie > Detail then
         Shared_Parser.Tree.Lexer.Trace.New_Line;
         Shared_Parser.Tree.Lexer.Trace.Put_Line ("Supervisor: Done");
      end if;
   end Finish;

   function Parser_State
     (Super        : in Supervisor;
      Parser_Index : in SAL.Peek_Type)
     return Parser_Lists.Constant_Reference_Type
   is begin
      return (Element => Super.Parser_Status (Parser_Index).Parser_State);
   end Parser_State;

   function Stream
     (Super        : in Supervisor;
      Parser_Index : in SAL.Peek_Type)
     return Syntax_Trees.Stream_ID
   is begin
      return Super.Parser_Status (Parser_Index).Parser_State.Stream;
   end Stream;

   function Min_Sequential_Index_All_SOI
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser)
     return Boolean
   is begin
      for I in Super.Min_Sequential_Indices'Range loop
         if Shared_Parser.Tree.ID (Super.Min_Sequential_Indices (I).Ref.Node) /=
           Shared_Parser.Tree.Lexer.Descriptor.SOI_ID
         then
            return False;
         end if;
      end loop;
      return True;
   end Min_Sequential_Index_All_SOI;

   function Max_Sequential_Index_All_EOI
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser)
     return Boolean
   is begin
      for I in 1 .. Super.Parser_Count loop
         if Shared_Parser.Tree.ID (Super.Max_Sequential_Indices (I).Ref.Node) /=
           Shared_Parser.Tree.Lexer.Descriptor.EOI_ID
         then
            return False;
         end if;
      end loop;
      return True;
   end Max_Sequential_Index_All_EOI;

   procedure Extend_Sequential_Index
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out Parser.Parser;
      Thru          : in     Syntax_Trees.Valid_Node_Access;
      Positive      : in     Boolean)
   is begin
      if Shared_Parser.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index then
         return;
      end if;

      declare
         Streams : Syntax_Trees.Stream_ID_Array (1 .. Shared_Parser.Parsers.Count);

         function Min_Target_Index return Syntax_Trees.Sequential_Index
         is begin
               declare
                  Min : constant Syntax_Trees.Sequential_Index := Super.Min_Sequential_Index (Shared_Parser);
                  pragma Assert (Min <= 0);
               begin
                  return (if Min = 0 then Default_Negative_Sequential_Index else 2 * Min);
               end;
         end Min_Target_Index;

      begin
         for I in Super.Parser_Status'Range loop
            Streams (I) := Super.Parser_Status (I).Parser_State.Stream;
         end loop;

         loop
            exit when Shared_Parser.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index;

            if Positive then
               exit when Super.Max_Sequential_Index_All_EOI (Shared_Parser);
               Extend_Sequential_Index
                 (Shared_Parser.Tree, Streams, Super.Max_Sequential_Indices,
                  Target   => 2 * Super.Max_Sequential_Index (Shared_Parser),
                  Positive => Positive,
                  Clear    => False);
            else
               exit when Super.Min_Sequential_Index_All_SOI (Shared_Parser);

               Extend_Sequential_Index
                 (Shared_Parser.Tree, Streams, Super.Min_Sequential_Indices,
                  Target   => Min_Target_Index,
                  Positive => Positive,
                  Clear    => False);
            end if;
         end loop;
      end;
   end Extend_Sequential_Index;

   procedure Extend_Sequential_Index
     (Super         : in out Base.Supervisor;
      Shared_Parser : in out Parser.Parser;
      Thru          : in     Syntax_Trees.Sequential_Index)
   is
      use Syntax_Trees;
   begin
      loop
         declare
            Min     : constant Sequential_Index := Super.Min_Sequential_Index (Shared_Parser);
            Max     : constant Sequential_Index := Super.Max_Sequential_Index (Shared_Parser);
            Streams : Syntax_Trees.Stream_ID_Array (1 .. Shared_Parser.Parsers.Count);
         begin
            exit when Thru in Min .. Max;

            for I in Super.Parser_Status'Range loop
               Streams (I) := Super.Parser_Status (I).Parser_State.Stream;
            end loop;

            if Thru < Min then
               exit when Super.Min_Sequential_Index_All_SOI (Shared_Parser);

               pragma Assert (Min < 0);
               Extend_Sequential_Index
                 (Shared_Parser.Tree, Streams, Super.Min_Sequential_Indices,
                  Target   => 2 * Min,
                  Positive => False,
                  Clear    => False);
            else
               exit when Super.Max_Sequential_Index_All_EOI (Shared_Parser);

               Extend_Sequential_Index
                 (Shared_Parser.Tree, Streams, Super.Max_Sequential_Indices,
                  Target   => 2 * Max,
                  Positive => True,
                  Clear    => False);
            end if;
         end;
      end loop;
   end Extend_Sequential_Index;

   procedure Put
     (Super         : in Supervisor;
      Shared_Parser : in Parser.Parser;
      Message       : in String;
      Parser_Index  : in SAL.Peek_Type;
      Config        : in Configuration)
   is begin
      Put (Message, Shared_Parser.Tree, Super.Parser_State (Parser_Index).Stream, Config);
   end Put;

end WisiToken.Parse.LR.McKenzie_Recover.Base;
