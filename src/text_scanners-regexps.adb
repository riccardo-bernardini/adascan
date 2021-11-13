pragma Ada_2012;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Text_Scanners.Regexps is
   use Ada.Strings.Unbounded;

   function Zero_Or_More (X:String) return String
   is ("(" & X & ")*");

   function One_Or_More (X:String) return String
   is ("(" & X & ")+");

   function Charset(X:String) return String
   is ("[" & X & "]");

   Dec_Sequence  : constant String := "[0-9]+";
   Hex_Sequence  : constant String := "[0-9a-fA-F]+";
   Ada_Decimal   : constant String :=
     Dec_Sequence & Zero_Or_More("_" & Dec_Sequence);

   Ada_Based     : constant String :=
     Dec_Sequence
     & "#"
     & Hex_Sequence & Zero_Or_More("_" & Hex_Sequence)
     & "#";

   Optional_Sign : constant String := "(+|-)?";

   function Anchor (X : String) return String
   is (if X (X'First) = '^' then X else '^' & X);

   function Is_Eof(X : Regexp) return Boolean
   is (X.Is_Eof_Regexp);

   function Compile(Regexp_Spec : String) return Regexp
   is

      Result : Regexp:=Regexp'(Is_Eof_Regexp => False,
                               Matcher => <>);
   begin
      Gnat.Regpat.Compile(Matcher         => Result.Matcher,
                          Expression      => Anchor(Regexp_Spec));

      return Result;
   end Compile;



   function ID_Regexp (Body_ID_Chars  : String := "a-zA-Z0-9";
                       Begin_ID_Chars : String := "a-zA-Z";
                       Section_Separator : String := "")
                       return Regexp
   is
      use GNAT;

      Head : constant String := Charset(Begin_ID_Chars);
      Core : constant String := Zero_Or_More(Charset(Body_ID_Chars));
   begin
      if Section_Separator = "" then
         return Compile (Head & Core);
      else
         return Compile(Head
                        & Core
                        & Zero_Or_More(Regpat.Quote(Section_Separator) & Core));
      end if;
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
            return ID_Regexp(Body_ID_Chars  => "a-zA-Z0-9",
                             Begin_ID_Chars => "a-zA-Z",
                             Section_Separator => "_");

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
      GNAT.Regpat.Match(Self    => Matcher.Matcher,
                        Data    => Input,
                        Matches => Matching.Data);
   end Match;


end Text_Scanners.Regexps;
