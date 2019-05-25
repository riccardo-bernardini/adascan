with Generic_Scanner;
with Test_Report;                 use Test_Report;

with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);


procedure Test_Scanner is
   use Ada.Characters;

   EOL : constant String := Latin_1.CR & Latin_1.LF;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   type Extended_Token_Type is
     (ID, Number, Text, Colon, Semicolon, Equal, EOF, Error);

   subtype Token_Type is Extended_Token_Type range ID .. EOF;

   package Scan is new Generic_Scanner (Token_Type);


   type Expected_Token is
      record
         Token : Extended_Token_Type;
         Value : Unbounded_String;
      end record;

   type Expected_Sequence is
     array (Positive range <>) of Expected_Token;

   type Expected_Sequence_Access is
     access Expected_Sequence;

--     type Comment_Style is (C, C_Plus_Plus, LaTeX, Ada_Style, Shell);
--     pragma Warnings (Off, C_Plus_Plus);
--     pragma Warnings (Off, LaTeX);
--     pragma Warnings (Off, Shell);

   type Test_Case is
      record
         Input       : Unbounded_String;
         Comment     : Scan.Comment_Style;
         Expected    : Expected_Sequence_Access;
         Extended_ID : Unbounded_String;
      end record;

   type Test_Case_Array is
     array (Positive range <>) of Test_Case;


   Test_Cases : constant Test_Case_Array :=
                  ((Input      => +"pip.po = 42; /* prova */ gigi : 'cane'; ",
                    Comment     => Scan.C_Like,
                    Extended_ID => +".",
                    Expected    =>
                    new Expected_Sequence'(
                      (ID, +"pip.po"),
                      (Equal, +"="),
                      (Number, +"42"),
                      (Semicolon, +";"),
                      (ID, +"gigi"),
                      (Colon, +":"),
                      (Text, +"'cane'"))),
                   (Input       => +"pippo = 42; /* prova */ gi.gi : 'cane'; ",
                    Comment     => Scan.C_Like,
                    Extended_ID => +"",
                    Expected    =>
                    new Expected_Sequence'(
                      (ID, +"pippo"),
                      (Equal, +"="),
                      (Number, +"42"),
                      (Semicolon, +";"),
                      (ID, +"gi"),
                      (Error, +""))),
                   (Input       =>
                    +"pippo = 42; -- Ignora #" & EOL
                    & EOL
                    & "   -- Riga vuota " & EOL
                    & " gigi : 'ca--n\'e''gatto'; ",
                    Comment     => Scan.Ada_Like,
                    Extended_ID => +"",
                    Expected    =>
                    new Expected_Sequence'(
                      (ID, +"pippo"),
                      (Equal, +"="),
                      (Number, +"42"),
                      (Semicolon, +";"),
                      (ID, +"gigi"),
                      (Colon, +":"),
                      (Text, +"'ca--n\'e'"),
                      (Text, +"'gatto'"))));




   function Check (Test : Test_Case) return Boolean is
      use Ada.Exceptions;

--        function To_String (Style : Comment_Style) return String is
--        begin
--           case Style is
--              when C =>
--                 return Scan.C_Like_Comments;
--              when C_Plus_Plus =>
--                 return Scan.C_Plus_Plus_Like_Comments;
--              when LaTeX =>
--                 return Scan.Latex_Like_Comments;
--              when  Ada_Style =>
--                 return Scan.Ada_Like_Comments;
--              when Shell =>
--                 return Scan.Shell_Like_Comments;
--           end case;
--        end To_String;


      Regexps : constant Scan.Token_Regexp_Array :=
                  (ID     => Scan.ID_Regexp (To_String (Test.Extended_ID)),
                   Number => Scan.Number_Regexp,
                   Text   => Scan.String_Regexp ('''),
                   Colon  => +":",
                   Semicolon => +";",
                   Equal     => +"=",
                   EOF       => +"");

      Scanner : Scan.Scanner_Type :=
                  Scan.New_Scanner
                    (Input         => To_String (Test.Input),
                     Regexps       => Regexps,
                     Comment_Delim => Scan.Comment_Like (Test.Comment),
                     Scan          => False);

      Expected       : Extended_Token_Type;
      Expected_Value : Unbounded_String;
      Token          : Token_Type;
   begin
--        Ada.Text_IO.Put_Line
--                ("*** TESTING *** <" & To_String (Test.Input) & ">");

      for I in Test.Expected'Range loop
         Expected := Test.Expected (I).Token;
--           Ada.Text_IO.Put_Line (">>>>  Expecting " & Expected'Img);

         Expected_Value := Test.Expected (I).Value;

         Token := Scanner.Next_Token;

         if Expected /=  Token or else
           To_String (Test.Expected (I).Value) /= Scanner.String_Value then
            Ada.Text_IO.Put_Line ("At " & Integer'Image (I) & "-th iteration:");

            Ada.Text_IO.Put_Line
              ("   Expected "
               & Extended_Token_Type'Image (Expected)
               & "<" & To_String (Expected_Value) & ">");

            Ada.Text_IO.Put_Line
              ("   Got "
               & Token_Type'Image (Token)
               & "<" & Scanner.String_Value & ">");

            Ada.Text_IO.Put_Line
              ("   Input = <" & To_String (Test.Input) & ">");
            return False;
         end if;

      end loop;

      return True;
   exception
      when E : Scan.Unrecognized_Token |
           Scan.Unmatched_Token |
           Scan.Unexpected_EOF =>

         if Expected = Error then
            return True;
         else
            Ada.Text_IO.Put_Line
              ("Unexpected error: " & Exception_Message (E));

            Ada.Text_IO.Put_Line
              ("   Expected "
               & Extended_Token_Type'Image (Expected)
               & "<" & To_String (Expected_Value) & ">");


            Ada.Text_IO.Put_Line
              ("   Input = <" & To_String (Test.Input) & ">");

            return False;
         end if;
   end Check;

   procedure Many_Tests is
     new Test_Report.Do_Suite (Test_Case       => Test_Case,
                               Test_Case_Array => Test_Case_Array,
                               Check           => Check);

   Reporter : Test_Report.Reporter_Type;
begin
   Many_Tests (Reporter, Test_Cases);
   Final (Reporter);
end Test_Scanner;








--     for I in Regexps'Range loop
--        Ada.Text_IO.Put_Line ("I=" & I'Img);
--        begin
--           Ada.Text_Io.Put_Line ("re: {" & To_String (Regexps (I)) & "}");
--        exception
--           when  E : others =>
--
--              Ada.Text_IO.Put_Line ("!!" & Ada.Exceptions.Exception_Message (E));
--        end;
--     end loop;

--     declare
--        Scanner : Scan.Scanner_Type :=
--                    Scan.New_Scanner
--                      (Input         => "pip.po = 42; /* prova */ gigi : 'cane'; ",
--                       Regexps       => Regexps,
--                       Comment_Delim => Scan.C_Like_Comments);
--     begin
--        while not Scanner.At_EOF loop
--           Ada.Text_IO.Put_Line (Token_Type'Image (Scanner.Current_Token)
--                                 & "-> [" & Scanner.String_Value & "]");
--           Scanner.Next;
--        end loop;
--     end;
