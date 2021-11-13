pragma Ada_2012;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Text_Scanners.Regexps is
   use Ada.Strings.Unbounded;

   Basic_Dec_Int : constant String := "[0-9]+";
   Basic_Hex_Int : constant String := "[0-9a-fA-F]+";
   Basic_Oct_Int : constant String := "[0-7]+";
   Ada_Decimal   : constant String := Basic_Dec_Int & "(_" & Basic_Dec_Int& ")+";
   Basic_Ada_Hex : constant String := Basic_Hex_Int & "(_" & Basic_Hex_Int & ")+";
   Ada_Based     : constant String := Basic_Dec_Int & "#" & Basic_Ada_Hex & "#";
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



   function ID_Regexp (Additional_ID_Chars : String := "_";
                       Basic_ID_Chars      : String := "a-zA-Z0-9";
                       Begin_ID_Chars      : String := "a-zA-Z")
                       return Regexp
   is
      Tmp : constant String := "[" & Begin_ID_Chars & "]"
        &
        "[" & Basic_ID_Chars & Additional_ID_Chars & "]*";
   begin
      return Compile (Tmp);
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
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "ID_Regexp unimplemented");
      return raise Program_Error with "Unimplemented function ID_Regexp";
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
              Optional_Sign & "(" & Basic_Dec_Int & "|0[xX]" & Basic_Hex_Int & ")");
   end Number_Regexp;


   function Float_Regexp (Style : Language_Style := Ada_Style)  return Regexp is
   begin
      return Compile ("[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?");
   end Float_Regexp;

   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp (Quote_Char : Character) return Regexp is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "String_Regexp unimplemented");
      return raise Program_Error with "Unimplemented function String_Regexp";
   end String_Regexp;

   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp
     (Style : Language_Style := Ada_Style)
      return Regexp
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "String_Regexp unimplemented");
      return raise Program_Error with "Unimplemented function String_Regexp";
   end String_Regexp;

   ------------------
   -- Fixed_String --
   ------------------

   function Fixed_String (Str : String) return Regexp
   is (Compile(Gnat.Regpat.Quote(Str)));

end Text_Scanners.Regexps;
