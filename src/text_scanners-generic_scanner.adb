
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;

--  with Ada.Text_IO;
--  pragma Warnings (Off, Ada.Text_IO);

package body Text_Scanners.Generic_Scanner with SPARK_Mode => Off is

   ------------
   -- Handle --
   ------------

   procedure Handle (Err : Basic_Scan.Error_Class)
   is
      use type Basic_Scan.Error_Class;
   begin
      case Err is
         when Basic_Scan.Ok =>
            return;

         when Basic_Scan.Unexpected_EOF =>
            raise Unexpected_EOF;

         when Basic_Scan.Unrecognized_Token =>
            raise Unrecognized_Token;
      end case;
   end Handle;

   ----------
   -- Next --
   ----------

   procedure Next (Scanner : Scanner_Type)
   is
      Err : Basic_Scan.Error_Class;
   begin
      Scanner.S.Next (Err);

      Handle (Err);
   end Next;
   -----------------
   -- New_Scanner --
   -----------------

   function New_Scanner (Input          : String;
                         Token_Regexps  : Token_Regexp_Array;
                         History_Size   : Positive := 1024;
                         Comment_Delim  : Regexps.Comment_Specs := Regexps.No_Comment;
                         Callbacks      : Post_Processor_Array := No_Processing;
                         Scan           : Boolean := True)
                         return Scanner_Type
   is
      use Ada.Strings.Maps;
      use Ada.Characters.Latin_1;

   begin

      return Result : Scanner_Type :=
        (Ada.Finalization.Limited_Controlled with
           S => new Basic_Scan.Basic_Scanner'
             (Basic_Scan.Create (Input => Input,
                                 Token_Regexps   => Token_Regexps,
                                 Comment_Delim   => Comment_Delim,
                                 Post_Processing => Callbacks)))
      do
         if Scan then
            Result.Next;
         end if;
      end return;
   end New_Scanner;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Scanner   : Scanner_Type;
      Expected  : Token_Type)
   is
   begin
      if Scanner.Next_Token /= Expected then
         raise Unmatched_Token;
      end if;

      Scanner.Next;
   end Expect;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Scanner  : Scanner_Type;
      Expected : Token_Array)
   is
   begin
      for I in Expected'Range loop
         if Scanner.S.Current_Token = Expected (I) then
            Scanner.Next;
            return;
         end if;
      end loop;

      raise Unmatched_Token;
   end Expect;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token (Scanner : Scanner_Type) return Token_Type is
   begin
      Scanner.Next;
      return Scanner.S.Current_Token;
   end Next_Token;


   function Eat_If_Equal (Scanner  : Scanner_Type;
                          Expected : Token_Type)
                          return Boolean
   is
   begin
      if Expected = Scanner.S.Current_Token then
         Scanner.Next;
         return True;
      else
         return False;
      end if;
   end Eat_If_Equal;


   -------------------
   -- Current_Token --
   -------------------

   function Current_Token (Scanner : Scanner_Type) return Token_Type is
   begin
      return Scanner.S.Current_Token;
   end Current_Token;

   ------------------
   -- String_Value --
   ------------------

   function String_Value (Scanner : Scanner_Type) return String is
   begin
      return Scanner.S.Current_Value;
   end String_Value;

   ------------
   -- At_EOF --
   ------------

   function At_EOF (Scanner : Scanner_Type) return Boolean is
   begin
      return Scanner.S.At_EOF;
   end At_EOF;


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Scanner_Type) with SPARK_Mode => Off  is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Basic_Scan.Basic_Scanner,
                                        Name   => Scanner_Access);
   begin
      --        Ada.Text_IO.Put_Line ("Finalize di Scanner");
      --        Destroy (Object.S.all);
      Free (Object.S);
      --        Ada.Text_IO.Put_Line ("Finalize di Scanner: DONE");
   end Finalize;

end Text_Scanners.Generic_Scanner;


