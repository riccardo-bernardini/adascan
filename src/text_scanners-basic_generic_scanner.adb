pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Ada.Strings.Maps; use Ada.Strings.Maps;


-----------------------------------------
-- Text_Scanners.Basic_Generic_Scanner --
-----------------------------------------

package body Text_Scanners.Basic_Generic_Scanner with SPARK_Mode => On is

   function Find (Source  : String;
                  Pattern : String)
                  return Natural
     with
       Pre =>
         Pattern'Length > 0 and Source'Length > 0;
--       Post =>
--         (if Find'Result > 0
--            then
--              (Find'Result <= Source'Last - (Pattern'Length - 1)  and Source (Find'Result .. Find'Result + (Pattern'Length - 1)) = Pattern)
--                else
--            (for all K in First .. Source'Last - Pattern'Length + 1 =>
--                   Source (K .. K + (Pattern'Length - 1)) /= Pattern)),
--       Global => null;


   function Find (Source  : String;
                  Pattern : String)
                  return Natural
   is
      D : constant Natural := Pattern'Length - 1;
   begin
      if Source'Length < Pattern'Length then
         return 0;
      end if;

      for K in Source'First .. Source'Last - D loop
         if Source (K .. K + D) = Pattern then
            pragma Assert (Source (K .. K + D) = Pattern);
            return K;
         else
            pragma Assert (not (Source (K .. K + D) = Pattern));
            null;
         end if;

         pragma Assert (Source'First > 0);
         pragma Assert (Source'Last >= Source'First);
         pragma Assert (Pattern'First > 0 and Pattern'Last > 0);
         pragma Assert (K > 0);
         pragma Assert (not (Source (K .. K + D) = Pattern));
         pragma Loop_Invariant (for all J in Source'First .. K => not (Source (J .. J + D) = Pattern));
      end loop;

      pragma Assert (for all K in Source'First .. Source'Last - D => Source (k .. k + D) /= Pattern);

      return 0;
   end Find;

   function Create (Input            : String;
                    Token_Regexps    : Regexp_Array;
                    Comment_Delim    : Text_Scanners.Regexps.Comment_Specs;
                    Post_Processing  : Post_Processor_Array)
                    return Basic_Scanner
   is
      use type Regexps.Regexp;
   begin
      return Result : Basic_Scanner := (Size            => Input'Length,
                                        Regexp_Table    => Token_Regexps,
                                        Post_Processing => Post_Processing,
                                        Input           => Input,
                                        Cursor          => 1,
                                        On_Eof          => Token_Type'First,
                                        On_EOF_Valid    => False,
                                        Current_Token   => Token_Type'First,
                                        String_Value    => Null_Unbounded_String,
                                        Whitespace      =>
                                          To_Set (" " & CR & LF & VT & HT),
                                        --                                          Callbacks       => Callback_Holder.To_Holder (Callbacks),
                                        Comment_Style   => Comment_Delim,
                                        First_Scan_Done => False)
      do
         Result.On_EOF_Valid := False;

         for I in Token_Type loop
            if Regexps.Is_EOF_Regexp (Result.Regexp_Table (I)) then
               if Result.On_EOF_Valid then
                  raise Constraint_Error with "Too many EOF symbols";
               end if;

               Result.On_EOF := I;
               Result.On_EOF_Valid := True;
            end if;
         end loop;

      end return;
   end Create;


   -----------------
   -- Skip_At_EOF --
   -----------------

   procedure Skip_At_EOF (Scanner : in out Basic_Scanner) is
   begin
      Scanner.Cursor := Scanner.Input'Last + 1;
   end Skip_At_EOF;

   function Current_Char (Scanner : Basic_Scanner) return Character
     with Pre => not Scanner.At_EOF;

   function Current_Char (Scanner : Basic_Scanner) return Character is
   begin
      return Scanner.Input (Scanner.Cursor);
   end Current_Char;

   pragma Inline (Current_Char);

   procedure Skip_Spaces (Scanner : in out Basic_Scanner)
     with Post => (Scanner.At_EOF
                   or else not Is_In (Scanner.Input (Scanner.Cursor), Scanner.Whitespace));
   --  Skip spaces until a non-space char or EOF.

   procedure Skip_Spaces (Scanner : in out Basic_Scanner) is
      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Pos : Natural;
   begin
      Pos := Index (Source => Scanner.Input,
                    Set    => Scanner.Whitespace,
                    From   => Scanner.Cursor,
                    Test   => Outside);

      pragma Assume (if Pos = 0
                     then
                       (for all Ch of Scanner.Input (Scanner.Cursor .. Scanner.Input'Last) => Is_In (Ch, Scanner.Whitespace))
                     else
                       ((Pos >= Scanner.Cursor and Pos < Scanner.Input'Last)
                        and then not Is_In (Scanner.Input (Pos), Scanner.Whitespace)));

      if Pos = 0 then
         Scanner.Skip_At_EOF;
      else
         Scanner.Cursor := Pos;
      end if;
   end Skip_Spaces;

   --------------
   -- Skip_EOL --
   --------------

   procedure Skip_EOL (Scanner : in out Basic_Scanner) is
      pragma Precondition (not Scanner.At_EOF);
   begin
      if Current_Char (Scanner) = CR then
         Scanner.Cursor := Scanner.Cursor + 1;
      end if;

      if not Scanner.At_EOF and then Current_Char (Scanner) = LF then
         Scanner.Cursor := Scanner.Cursor + 1;
      end if;
   end Skip_EOL;

   procedure Skip_Single_Delimiter_Comment (Scanner : in out Basic_Scanner;
                                            Err     : out Error_Class)
   is
      use Ada.Strings.Fixed;

      Pos : Natural;
   begin
      --  Search for the beginning of a line delimiter: CR or
      --  LF
      Pos := Index (Source => Scanner.Input,
                    Set    => To_Set (CR & LF),
                    From   => Scanner.Cursor);

      pragma Assume
        (if Pos = 0
         then
           (for all Ch of Scanner.Input (Scanner.Cursor .. Scanner.Input'Last) => not Is_In (Ch, To_Set (CR & LF)))
         else
           ((Pos >= Scanner.Cursor and Pos < Scanner.Input'Last)
            and then Is_In (Scanner.Input (Pos), To_Set (Cr & LF))));


      if Pos = 0 then
         --  No line delimiter found, but this is OK: the
         --  comment ends at EOF
         Scanner.Skip_At_EOF;
      else
         --  Move to the beginning of the line delimiter
         --  and skip it
         Scanner.Cursor := Pos;
         Skip_EOL (Scanner);
      end if;

      Err := Ok;
      return;
   end Skip_Single_Delimiter_Comment;

   -----------------------------------
   -- Skip_Double_Delimiter_Comment --
   -----------------------------------
   procedure Skip_Double_Delimiter_Comment (Scanner : in out Basic_Scanner;
                                            Error   : out Error_Class)
     with
       Pre => Regexps."=" (Regexps.Format (Scanner.Comment_Style), Regexps.End_At_Delimiter);

   procedure Skip_Double_Delimiter_Comment (Scanner : in out Basic_Scanner;
                                            Error   : out Error_Class)
   is
      use Regexps;
      use Ada.Strings.Fixed;

      Pos  : Natural;
      Stop : constant String := Comment_End (Scanner.Comment_Style);
   begin
--        Pos := Index (Source  => Scanner.Input,
--                      Pattern => Stop,
--                      From    => Scanner.Cursor);

      Pos := Find (Source  => Scanner.Input (Scanner.Cursor .. Scanner.Input'Last),
                   Pattern => Stop);
--                     First   => Scanner.Cursor);

      if Pos = 0 then
         --  No closing delimiter found
         Error := Unexpected_EOF;
         return;
      end if;

      --  Move to the first character after the delimiter
      Scanner.Cursor := Pos + Stop'Length;

      Error := Ok;
   end Skip_Double_Delimiter_Comment;

   ------------------
   -- Skip_Comment --
   ------------------

   procedure Skip_Comment (Scanner : in out Basic_Scanner;
                           Skipped : out Boolean;
                           Error   : out Error_Class)
   is
      use Ada.Strings.Fixed;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Maps;
      use Text_Scanners.Regexps;


      procedure Check_Comment_Start (Scanner : in out Basic_Scanner;
                                     Present :    out Boolean)
        with Post =>
          (if Format (Scanner.Comment_Style) = Void then not Present)
          and (Present = (Scanner.Cursor > Scanner.Cursor'Old));

      procedure Check_Comment_Start (Scanner : in out Basic_Scanner;
                                     Present :    out Boolean)
      is
      begin
         if Format (Scanner.Comment_Style) = Void then
            Present := False;

         else
            declare
               Start : constant String := Comment_Start (Scanner.Comment_Style);
               Last  : constant Natural := Scanner.Cursor + Start'Length - 1;
            begin
               if Start'Length > Scanner.Input'Last - Scanner.Cursor + 1 or else
                 Scanner.Input (Scanner.Cursor .. Last) /= Start
               then
                  Present := False;
               else
                  Present := True;
                  Scanner.Cursor := Last + 1;
               end if;
            end;
         end if;

      end Check_Comment_Start;


      In_Comment : Boolean;

      subtype Non_Void_Comment_Style is
        Comment_Format range End_At_EOL .. End_At_Delimiter;
   begin
      Check_Comment_Start (Scanner, In_Comment);

      if not In_Comment then
         Skipped := False;
         Error := Ok;
         return;
      end if;


      Skipped := True;

      pragma Assert (Format (Scanner.Comment_Style) /= Void);

      case Non_Void_Comment_Style'(Format (Scanner.Comment_Style)) is
         when End_At_EOL =>
            Skip_Single_Delimiter_Comment (Scanner, Error);
            return;

         when End_At_Delimiter =>
            pragma Assert (Format (Scanner.Comment_Style) = End_At_Delimiter);

            Skip_Double_Delimiter_Comment (Scanner, Error);
            return;
      end case;
   end Skip_Comment;


   ----------
   -- Next --
   ----------

   procedure Next (Scanner : in out Basic_Scanner;
                   Err     :    out Error_Class) is



      Skipped : Boolean;
      Error   : Error_Class;
   begin
      loop
         Skip_Spaces (Scanner);
         Skip_Comment (Scanner, Skipped, Error);

         if Error /= Ok then
            Err := Error;
            return;
         end if;

         exit when not Skipped;
      end loop;

      if Scanner.At_EOF then
         if not Scanner.On_Eof_Valid then
            --  The user did not declare any "EOF" symbol
            Err := Unexpected_EOF;
            return;

         else
            Scanner.Current_Token := Scanner.On_Eof;
            Scanner.String_Value := Null_Unbounded_String;
            Err := Ok;
            return;
         end if;
      end if;

      pragma Assert (not Scanner.At_EOF);

      declare
         use Text_Scanners.Regexps;

         Buffer : Match_Result;
         Expr   : constant String :=
                    Scanner.Input (Scanner.Cursor .. Scanner.Input'Last);
      begin
         --           Ada.Text_IO.Put_Line ("Expr = [" & Expr & "]");

         for Token in Scanner.Regexp_Table'Range loop
            Buffer := Scanner.Regexp_Table (Token).Match (Expr);

            if Buffer /= No_Match then
               Scanner.First_Scan_Done := True;

               Scanner.Current_Token := Token;

               declare
                  use Post_Processors;

                  Value : constant String :=
                            Scanner.Input (Buffer.First .. Buffer.Last);
               begin
                  Scanner.String_Value :=
                    To_Unbounded_String (Apply (Scanner.Post_Processing (Token), Value));
               end;

               Scanner.Cursor := Buffer.Last + 1;
               Err := Ok;
               return;

            end if;
         end loop;
      end;

      Err := Unrecognized_Token;
   end Next;
end Text_Scanners.Basic_Generic_Scanner;
