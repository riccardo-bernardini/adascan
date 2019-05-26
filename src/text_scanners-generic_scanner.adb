pragma SPARK_Mode (On);

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
--  with Ada.Text_IO;
--  pragma Warnings (Off, Ada.Text_IO);

package body Text_Scanners.Generic_Scanner is

   -----------------
   -- New_Scanner --
   -----------------

   function New_Scanner (Input          : String;
                         Regexps        : Token_Regexp_Array;
                         History_Size   : Positive := 1024;
                         Comment_Delim  : Comment_Specs := No_Comment;
                         Callbacks      : String_Preprocessor'Class := No_Processing;
                         Scan           : Boolean := True)
                         return Scanner_Type
     with SPARK_Mode => On
   is
      use Ada.Strings.Maps;
      use Ada.Characters.Latin_1;

      Tmp : Scanner_Access;
   begin



      Tmp := new True_Scanner_Type'(Ada.Finalization.Controlled with
                                      Size            => Input'Length,
                                    History_Size    => History_Size,
                                    Regexp_Table    => (others => null),
                                    Input           => Input,
                                    Cursor          => 1,
                                    On_Eof          => <>,
                                    On_EOF_Valid    => False,
                                    Current_Token   => <>,
                                    String_Value    => <>,
                                    Whitespace      =>
                                      To_Set (" " & CR & LF & VT & HT),
                                    History         => <>,
                                    History_Cursor  => 0,
                                    Callbacks       => Callback_Holder.To_Holder (Callbacks),
                                    Comment_Style   => Comment_Delim,
                                    First_Scan_Done => False);


      Initialize (Tmp.all, Regexps);
      return Result : Scanner_Type :=
        (Ada.Finalization.Limited_Controlled with S => Tmp)
      do
         pragma Warnings (Off, Result);

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

   -----------------
   -- Skip_At_EOF --
   -----------------

   procedure Skip_At_EOF (Scanner : Scanner_Type) is
   begin
      Scanner.S.Cursor := Scanner.S.Input'Last + 1;
   end Skip_At_EOF;

   -----------------------------------
   -- Save_Current_Token_In_History --
   -----------------------------------

   procedure Save_Current_Token_In_History (Scanner : Scanner_Type) is
   begin
      if Scanner.S.First_Scan_Done then
         Scanner.S.History (Scanner.S.History_Cursor) :=
           (Token => Scanner.S.Current_Token,
            Value => Scanner.S.String_Value);

         Scanner.S.History_Cursor :=
           (Scanner.S.History_Cursor + 1) mod Scanner.S.History_Size;
      end if;
   end Save_Current_Token_In_History;

   ----------
   -- Next --
   ----------

   procedure Next (Scanner : Scanner_Type) is
      use GNAT.Regpat;

      function Current_Char (Scanner : Scanner_Type) return Character is
      begin
         return Scanner.S.Input (Scanner.S.Cursor);
      end Current_Char;

      pragma Inline (Current_Char);

      procedure Skip_Spaces (Scanner : Scanner_Type);
      pragma Postcondition (not Ada.Strings.Maps.Is_In
                            (Current_Char (Scanner), Scanner.S.Whitespace));
      --  Skip spaces until a non-space char or EOF.

      procedure Skip_Spaces (Scanner : Scanner_Type) is
         use Ada.Strings.Maps;
         use Ada.Strings.Fixed;
         use Ada.Strings;

         Pos : Natural;
      begin
         Pos := Index (Source => Scanner.S.Input,
                       Set    => Scanner.S.Whitespace,
                       From   => Scanner.S.Cursor,
                       Test   => Outside);

         if Pos = 0 then
            Scanner.Skip_At_EOF;
         else
            Scanner.S.Cursor := Pos;
         end if;
      end Skip_Spaces;

      function Skip_Comment (Scanner : Scanner_Type) return Boolean is
         use Ada.Strings.Fixed;
         use Ada.Characters.Latin_1;
         use Ada.Strings.Maps;

         procedure Skip_EOL (Scanner : Scanner_Type) is
            pragma Precondition (not Scanner.At_EOF);
         begin
            if Current_Char (Scanner) = CR then
               Scanner.S.Cursor := Scanner.S.Cursor + 1;
            end if;

            if not Scanner.At_EOF and then Current_Char (Scanner) = LF then
               Scanner.S.Cursor := Scanner.S.Cursor + 1;
            end if;
         end Skip_EOL;



      begin
         if Scanner.S.Comment_Style.Style = Void then
            return False;
         end if;

         declare
            Start : constant String := To_String (Scanner.S.Comment_Style.Start);
            Last  : constant Natural := Scanner.S.Cursor + Start'Length - 1;
         begin
            if Last > Scanner.S.Input'Last then
               return False;
            end if;

            if Scanner.S.Input (Scanner.S.Cursor .. Last) /= Start then
               return False;
            end if;

            Scanner.S.Cursor := Last + 1;

            case Scanner.S.Comment_Style.Style is
            when Void =>
               raise Program_Error;

            when End_At_EOL =>
               declare
                  Pos : Natural;
               begin
                  --  Search for the beginning of a line delimiter: CR or
                  --  LF
                  Pos := Index (Source => Scanner.S.Input,
                                Set    => To_Set (CR & LF),
                                From   => Scanner.S.Cursor);

                  if Pos = 0 then
                     --  No line delimiter found, but this is OK: the
                     --  comment ends at EOF
                     Scanner.Skip_At_EOF;
                  else
                     --  Move to the beginning of the line delimiter
                     --  and skip it
                     Scanner.S.Cursor := Pos;
                     Skip_EOL (Scanner);
                  end if;

                  return True;
               end;
            when End_Delimeter =>
               pragma Assert (Scanner.S.Comment_Style.Style = End_Delimeter);

               declare
                  Pos  : Natural;
                  Stop : constant String := To_String (Scanner.S.Comment_Style.Stop);
               begin
                  Pos := Index (Source  => Scanner.S.Input,
                                Pattern => Stop,
                                From    => Scanner.S.Cursor);

                  if Pos = 0 then
                     --  No closing delimiter found
                     raise Unexpected_EOF;
                  end if;

                  --  Move to the first character after the delimiter
                  Scanner.S.Cursor := Pos + Stop'Length;
                  return True;
               end;
            end case;
         end;
      end Skip_Comment;


   begin
      loop
         Skip_Spaces (Scanner);
         exit when not Skip_Comment (Scanner);
      end loop;

      if Scanner.At_EOF then
         if not Scanner.S.On_Eof_Valid then
            --  The user did not declare any "EOF" symbol
            raise Unexpected_EOF;
         else
            Scanner.S.Current_Token := Scanner.S.On_Eof;
            Scanner.S.String_Value := Null_Unbounded_String;
            return;
         end if;
      end if;

      pragma Assert (not Scanner.At_EOF);

      declare
         Buffer : Match_Array (0 .. 0);
         Expr   : constant String :=
                    Scanner.S.Input (Scanner.S.Cursor .. Scanner.S.Input'Last);
      begin
         --           Ada.Text_IO.Put_Line ("Expr = [" & Expr & "]");

         for Token in Scanner.S.Regexp_Table'Range loop
            Match (Self    => Scanner.S.Regexp_Table (Token).all,
                   Data    => Expr,
                   Matches => Buffer);

            if Buffer (0) /= No_Match then
               Scanner.Save_Current_Token_In_History;
               Scanner.S.First_Scan_Done := True;

               Scanner.S.Current_Token := Token;

               declare
                  Value : constant String :=
                            Scanner.S.Input (Buffer (0).First .. Buffer (0).Last);
               begin
--                    if Scanner.S.Callbacks (Token) /= null then
--                       Scanner.S.String_Value :=
--                         To_Unbounded_String (Scanner.S.Callbacks (Token) (Value));
--                    else
--                       Scanner.S.String_Value :=
--                         To_Unbounded_String (Value);
--                    end if;
                  null;
               end;

               Scanner.S.Cursor := Buffer (0).Last + 1;
               return;

               end if;
         end loop;
      end;

      raise Unrecognized_Token;
   end Next;

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
      return To_String (Scanner.S.String_Value);
   end String_Value;

   ------------
   -- At_EOF --
   ------------

   function At_EOF (Scanner : Scanner_Type) return Boolean is
   begin
      return Scanner.S.Cursor > Scanner.S.Input'Last;
   end At_EOF;


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item   : in out True_Scanner_Type;
      Tokens : Token_Regexp_Array)
   is
      use GNAT.Regpat;

      function Anchored (X : Unbounded_String) return String is
         Tmp : constant String := To_String (X);
      begin
         if Tmp (Tmp'First) = '^' then
            return Tmp;
         else
            return '^' & Tmp;
         end if;
      end Anchored;
   begin
      Item.On_EOF_Valid := False;

      for I in Tokens'Range loop
         if Tokens (I) = null then
            if Item.On_EOF_Valid then
               raise Constraint_Error
                 with "Too many EOF symbols";
            end if;

            Item.On_EOF := I;
            Item.On_EOF_Valid := True;
            Item.Regexp_Table (I) := new Pattern_Matcher'(Never_Match);
         else
              Item.Regexp_Table (I) := Tokens(I);
--            new Pattern_Matche (
--                                  r'(Compile (Anchored (Tokens (I))));
         end if;
      end loop;
   end Initialize;



   -------------
   -- Destroy --
   -------------

   overriding procedure Finalize (Item : in out True_Scanner_Type) is
      use GNAT.Regpat;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Pattern_Matcher,
                                        Name   => Token_Regexp);
   begin
      --        Ada.Text_IO.Put_Line ("Finalize di TRUE Scanner");
      for I in Item.Regexp_Table'Range loop
         if Item.Regexp_Table (I) /= null then
              Free (Item.Regexp_Table (I));
         end if;
      end loop;
      --        Ada.Text_IO.Put_Line ("Finalize di TRUE Scanner: DONE");
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Scanner_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => True_Scanner_Type,
                                        Name   => Scanner_Access);
   begin
      --        Ada.Text_IO.Put_Line ("Finalize di Scanner");
      --        Destroy (Object.S.all);
      Free (Object.S);
      --        Ada.Text_IO.Put_Line ("Finalize di Scanner: DONE");
   end Finalize;

   -------------------------------
   -- Single_Delimeter_Comments --
   -------------------------------

   function Single_Delimeter_Comments (Start : String) return Comment_Specs is
   begin
      return Comment_Specs'(Style => End_At_EOL,
                            Start => To_Unbounded_String (Start));
   end Single_Delimeter_Comments;


   -------------------------------
   -- Double_Delimeter_Comments --
   -------------------------------

   function Double_Delimeter_Comments (Start, Stop : String) return Comment_Specs is
   begin
      return Comment_Specs'(Style => End_Delimeter,
                            Start => To_Unbounded_String (Start),
                            Stop  => To_Unbounded_String (Stop));
   end Double_Delimeter_Comments;

   function Comment_Like (Style : Comment_Style) return Comment_Specs is
   begin
      case Style is
         when Shell_Like =>
            return Single_Delimeter_Comments ("#");
         when Ada_Like =>
            return Single_Delimeter_Comments ("--");
         when LaTeX_Like =>
            return Single_Delimeter_Comments ("%");
         when C_Like =>
            return Double_Delimeter_Comments ("/*", "*/");
         when C_Plus_Plus_Like =>
            return Single_Delimeter_Comments ("//");
         when Asm_Like =>
            return Single_Delimeter_Comments (";");
      end case;
   end Comment_Like;

end Text_Scanners.Generic_Scanner;
















--     function Shell_Like_Comments return String
--     is
--     begin
--        return "#";
--     end Shell_Like_Comments;
--
--     function Ada_Like_Comments return String
--     is
--     begin
--        return "--";
--     end Ada_Like_Comments;
--
--     function C_Like_Comments return String
--     is
--     begin
--        return "/* */";
--     end C_Like_Comments;
--
--     function C_Plus_Plus_Like_Comments return String
--     is
--     begin
--        return "//";
--     end C_Plus_Plus_Like_Comments;
--
--     function Latex_Like_Comments return String
--     is
--     begin
--        return "%";
--     end Latex_Like_Comments;



--        procedure Parse_Comment_Delim (Specs : Comment_Specs;
--                                       Style : out Comment_Style_Type;
--                                       Start : out Unbounded_String;
--                                       Stop  : out Unbounded_String)
--        is
--           use Ada.Strings.Fixed;
--
--           Pos : Natural;
--        begin
--           if Specs = "" then
--              Style := Void;
--              Start := Null_Unbounded_String;
--              Stop := Null_Unbounded_String;
--           else
--              Pos := Index (Source  => String (Specs),
--                            Pattern => " ");
--
--              if Pos = 0 then
--                 Style := End_At_EOL;
--                 Start := To_Unbounded_String (String (Specs));
--                 Stop := Null_Unbounded_String;
--              else
--                 if
--                   Pos = Specs'Last or else
--                   Pos = Specs'First or else
--                   Index (String (Specs (Pos + 1 .. Specs'Last)), " ") /= 0
--                 then
--                    raise Constraint_Error;
--                 else
--                    Style := End_Delimeter;
--                    Start := To_Unbounded_String (String (Specs (Specs'First .. Pos - 1)));
--                    Stop := To_Unbounded_String (String (Specs (Pos + 1 .. Specs'Last)));
--                 end if;
--              end if;
--           end if;
--        end Parse_Comment_Delim;
