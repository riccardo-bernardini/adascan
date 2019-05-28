pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Ada.Strings.Maps; use Ada.Strings.Maps;

package body Text_Scanners.Basic_Generic_Scanner with SPARK_Mode => On is

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

   --     -----------------------------------
   --     -- Save_Current_Token_In_History --
   --     -----------------------------------
   --
   --     procedure Save_Current_Token_In_History (Scanner : in out Basic_Scanner) is
   --     begin
   --        if Scanner.First_Scan_Done then
   --           Scanner.History (Scanner.History_Cursor) :=
   --             (Token => Scanner.Current_Token,
   --              Value => Scanner.String_Value);
   --
   --           Scanner.History_Cursor :=
   --             (Scanner.History_Cursor + 1) mod Scanner.History_Size;
   --        end if;
   --     end Save_Current_Token_In_History;


   ----------
   -- Next --
   ----------

   procedure Next (Scanner : in out Basic_Scanner) is

      function Current_Char (Scanner : Basic_Scanner) return Character
        with Pre => not Scanner.At_EOF;

      function Current_Char (Scanner : Basic_Scanner) return Character is
      begin
         return Scanner.Input (Scanner.Cursor);
      end Current_Char;

      pragma Inline (Current_Char);

      procedure Skip_Spaces (Scanner : in out Basic_Scanner)
        with Post => (Scanner.At_EOF or else
                        not Ada.Strings.Maps.Is_In
                          (Current_Char (Scanner), Scanner.Whitespace));
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

         if Pos = 0 then
            Scanner.Skip_At_EOF;
         else
            Scanner.Cursor := Pos;
         end if;
      end Skip_Spaces;

      procedure Skip_Comment (Scanner : in out Basic_Scanner;
                              Skipped : out Boolean)
      is
         use Ada.Strings.Fixed;
         use Ada.Characters.Latin_1;
         use Ada.Strings.Maps;
         use Text_Scanners.Regexps;

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

         else
            Skipped := True;

            pragma Assert (Format (Scanner.Comment_Style) /= Void);

            case Non_Void_Comment_Style'(Format (Scanner.Comment_Style)) is
               when End_At_EOL =>
                  declare
                     Pos : Natural;
                  begin
                     --  Search for the beginning of a line delimiter: CR or
                     --  LF
                     Pos := Index (Source => Scanner.Input,
                                   Set    => To_Set (CR & LF),
                                   From   => Scanner.Cursor);

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
                  end;

               when End_At_Delimiter =>
                  pragma Assert (Format (Scanner.Comment_Style) = End_At_Delimiter);

                  declare
                     Pos  : Natural;
                     Stop : constant String := Comment_End (Scanner.Comment_Style);
                  begin
                     Pos := Index (Source  => Scanner.Input,
                                   Pattern => Stop,
                                   From    => Scanner.Cursor);

                     if Pos = 0 then
                        --  No closing delimiter found
                        raise Unexpected_EOF;
                     end if;

                     --  Move to the first character after the delimiter
                     Scanner.Cursor := Pos + Stop'Length;
                  end;
            end case;
         end if;
      end Skip_Comment;

      Skipped : Boolean;
   begin
      loop
         Skip_Spaces (Scanner);
         Skip_Comment (Scanner, Skipped);
         exit when not Skipped;
      end loop;

      if Scanner.At_EOF then
         if not Scanner.On_Eof_Valid then
            --  The user did not declare any "EOF" symbol
            raise Unexpected_EOF;
         else
            Scanner.Current_Token := Scanner.On_Eof;
            Scanner.String_Value := Null_Unbounded_String;
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
               --                 Scanner.Save_Current_Token_In_History;
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
               return;

            end if;
         end loop;
      end;

      raise Unrecognized_Token;
   end Next;
end Text_Scanners.Basic_Generic_Scanner;
