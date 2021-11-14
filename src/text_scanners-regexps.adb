pragma Ada_2012;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Text_Scanners.Regexps is
   use Ada.Strings.Unbounded;

   function To_Character_Set(X:Charset_Spec)
                             return Strings.Maps.Character_Set
   is
      use Ada.Strings.Maps;

      Result : Character_Set := Null_Set;
      Inverted : Boolean := False;

      Cursor : Positive := X'First;


      function End_Of_Input return Boolean
      is (Cursor > X'Last);

      function Current_Char return Character
        with Pre => not End_Of_Input;

      function Current_Char return Character
      is (X(Cursor));

      procedure Next
      is
      begin
         Cursor := Cursor+1;
      end Next;

      C : Character;
   begin
      if Current_Char = '^' then
         Inverted := True;
         Next;
      end if;

      while not End_Of_Input loop
         C := Current_Char;
         Next;

         if End_Of_Input or else  Current_Char /= '-' then
            Result := Result or To_Set(C);

         else
            pragma Assert(Current_Char = '-');
            Next;

            if End_Of_Input then
               --
               -- We found something like "a-"
               --

               Result := Result or To_Set(C) or To_Set('-');

            else
               -- Here we found something like "a-z"
               pragma Assert(not End_Of_Input);

               Result := Result or To_Set(Character_Range'(C, Current_Char));

               Next;
            end if;
         end if;
      end loop;

      if Inverted then
         Result := not Result;
      end if;

      return Result;
   end To_Character_Set;

   function Zero_Or_More (X:String) return String
   is ("(" & X & ")*");

   function One_Or_More (X:String) return String
   is ("(" & X & ")+");

   function Charset(X:Charset_Spec) return String
   is ("[" & String(X) & "]");

   Dec_Sequence  : constant String := "[0-9]+";
   Hex_Sequence  : constant String := "[0-9a-fA-F]+";
   Ada_Decimal   : constant String :=
     Dec_Sequence & Zero_Or_More("_" & Dec_Sequence);

   Ada_Based     : constant String :=
     Dec_Sequence
     & "#"
     & Hex_Sequence & Zero_Or_More("_" & Hex_Sequence)
     & "#";

   Optional_Sign : constant String := "(\+|-)?";

   function Anchor (X : String) return String
   is (if X (X'First) = '^' then X else '^' & X);

   function Is_Eof(X : Regexp) return Boolean
   is (X.Is_Eof_Regexp);

   function Compile(Regexp_Spec : String) return Regexp
   is

      Result : Regexp:=Regexp'(Is_Eof_Regexp => False,
                               Matcher => <>);
   begin
      Ada.Text_IO.Put_Line("<" & Regexp_Spec & ">");

      Gnat.Regpat.Compile(Matcher         => Result.Matcher,
                          Expression      => Anchor(Regexp_Spec));

      return Result;
   end Compile;

   function Sectioned_ID (Section_Delimiters : Charset_Spec;
                          Begin_ID_Chars : Charset_Spec := "a-zA-Z_";
                          Body_ID_Chars  : Charset_Spec := "a-zA-Z0-9_")
                          return Regexp
   is
      Head : constant String := Charset(Begin_ID_Chars);
      Core : constant String := Zero_Or_More(Charset(Body_ID_Chars));
   begin
      return Compile(Head
                     & Core
                     & Zero_Or_More(Charset(Section_Delimiters) & Core));
   end Sectioned_ID;

   function ID_Regexp (Begin_ID_Chars : Charset_Spec := "a-zA-Z_";
                       Body_ID_Chars  : Charset_Spec := "a-zA-Z0-9_")
                       return Regexp
   is
      --      use GNAT;

      Head : constant String := Charset(Begin_ID_Chars);
      Core : constant String := Zero_Or_More(Charset(Body_ID_Chars));
   begin
      return Compile (Head & Core);
   end ID_Regexp;

   function Number_Regexp return Unbounded_String
   is
   begin
      return To_Unbounded_String ("[1-9][0-9]*");
   end Number_Regexp;


   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp (Quote_Char : Character) return Unbounded_String
   is
      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;

      Pattern : constant String := "'([^\\']|\\.)*'";
   begin
      return To_Unbounded_String
        (Translate (Source  => Pattern,
                    Mapping => To_Mapping ("'", "" & Quote_Char)));
   end String_Regexp;


   ---------------
   -- ID_Regexp --
   ---------------

   function ID_Regexp (Style : Language_Style) return Regexp is
      Alfanum_Seq : constant String := Zero_Or_More(Charset("a-zA-Z0-9"));
   begin
      case Style is
      when Ada_Style =>
         return Sectioned_ID(Body_ID_Chars  => "a-zA-Z0-9",
                             Begin_ID_Chars => "a-zA-Z",
                             Section_Delimiters => "_");

      when C_Style=>
         return ID_Regexp(Body_ID_Chars  => "a-zA-Z0-9_",
                          Begin_ID_Chars => "a-zA-Z_");
      end case;
   end ID_Regexp;

   -------------------
   -- Number_Regexp --
   -------------------

   function Number_Regexp
     (Style : Language_Style := Ada_Style)
   return Regexp
   is
   begin
      return Compile
        (case Style is
            when Ada_Style =>
              Optional_Sign & "(" & Ada_Decimal & "|" & Ada_Based & ")",

            when C_Style =>
              Optional_Sign & "(" & Dec_Sequence & "|0[xX]" & Hex_Sequence & ")");
   end Number_Regexp;


   function Float_Regexp (Style : Language_Style := Ada_Style)  return Regexp is
   begin
      return Compile ("[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?");
   end Float_Regexp;

   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp (Quote_Char : Character) return Regexp is
      No_Quote_Seq : constant String := "[^""]*";
   begin
      return Compile('"'
                     & No_Quote_Seq
                     & Zero_Or_More (Quote_Char & """" & No_Quote_Seq)
                     & "'");
   end String_Regexp;

   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp
     (Style : Language_Style := Ada_Style)
   return Regexp
   is
   begin
      case Style is
      when Ada_Style =>
         return String_Regexp('"');

      when C_Style =>
         return String_Regexp('\');
      end case;
   end String_Regexp;

   ------------------
   -- Fixed_String --
   ------------------

   function Fixed_String (Str : String) return Regexp
   is (Compile(Gnat.Regpat.Quote(Str)));

   function Has_Matched(Item : Match_Data) return Boolean
   is
      use type GNAT.Regpat.Match_Location;
   begin
      return Item.Data(0) /= GNAT.Regpat.No_Match;
   end Has_Matched;

   function Get_Matched (Source : String;
                         Matching : Match_Data)
                      return String
   is (Source(Matching.Data(0).First ..  Matching.Data(0).Last));

   function Last_Matched (Matching: Match_Data) return Positive
   is (Matching.Data(0).Last);

   function First_Matched (Matching: Match_Data) return Positive
   is (Matching.Data(0).First);

   procedure Match(Matcher  : Regexp;
                   Matching : out Match_Data;
                   Input    : String)
   is
   begin
      if Matcher.Is_Eof_Regexp then
         Matching.Data(0) := GNAT.Regpat.No_Match;

      else
         GNAT.Regpat.Match(Self    => Matcher.Matcher,
                           Data    => Input,
                           Matches => Matching.Data);
      end if;
   end Match;


end Text_Scanners.Regexps;
