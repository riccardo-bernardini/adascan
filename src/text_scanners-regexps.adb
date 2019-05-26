pragma Ada_2012;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body Text_Scanners.Regexps is
   pragma SPARK_Mode (On);

   pragma Warnings (Off, "no Global Contract available");

   function Parse (Str : String) return Regexp
   is
      use Gnat.Regpat;
   begin
      return Regexp'(R => Regexp_Holders.To_Holder (Compile (Str)));
   end Parse;

   function ID_Regexp (Additional_ID_Chars : String := "_";
                       Basic_ID_Chars      : String := "a-zA-Z0-9";
                       Begin_ID_Chars      : String := "a-zA-Z")
                       return Regexp
   is
   begin
      return Parse ("[" & Begin_ID_Chars & "]" &
                      "[" & Basic_ID_Chars & Additional_ID_Chars & "]*");
   end ID_Regexp;



   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp (Quote_Char : Character) return Regexp
   is
      use Ada.Strings.Maps;
      use Ada.Strings.Fixed;

      Pattern : constant String := "'([^\\']|\\.)*'";
   begin
      return Parse
        (Translate (Source  => Pattern,
                    Mapping => To_Mapping ("'", "" & Quote_Char)));
   end String_Regexp;


   ---------------
   -- ID_Regexp --
   ---------------

   function ID_Regexp (Style : Language_Style) return Regexp is
      pragma SPARK_Mode (Off);
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
      return Parse ("[-+]?[0-9]?");
   end Number_Regexp;

   function Fixed_String (Str : String) return Regexp
   is
   begin
      return Parse (GNAT.Regpat.Quote (Str));
   end Fixed_String;

   function Float_Regexp (Style : Language_Style := Ada_Style) return Regexp is
   begin
      return Parse ("[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?");
   end Float_Regexp;

   -------------------
   -- String_Regexp --
   -------------------

   function String_Regexp
     (Style : Language_Style := Ada_Style)
      return Regexp
   is
      pragma SPARK_Mode (Off);
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "String_Regexp unimplemented");
      return raise Program_Error with "Unimplemented function String_Regexp";
   end String_Regexp;


   -------------------------------
   -- Single_Delimeter_Comments --
   -------------------------------

   function Single_Delimeter_Comments (Start : String) return Comment_Specs is
   begin
      return Comment_Specs'(Format => End_At_EOL,
                            Start  => To_Unbounded_String (Start));
   end Single_Delimeter_Comments;


   -------------------------------
   -- Double_Delimeter_Comments --
   -------------------------------

   function Double_Delimeter_Comments (Start, Stop : String) return Comment_Specs is
   begin
      return Comment_Specs'(Format => End_At_Delimiter,
                            Start  => To_Unbounded_String (Start),
                            Stop   => To_Unbounded_String (Stop));
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

   function Match (Self : Regexp;
                   Data : String)
                   return Match_Result
   is
      use type GNAT.Regpat.Match_Location;

      Matches : Gnat.Regpat.Match_Array (0 .. 0);
   begin
      if Is_Eof_Regexp (Self) then
         return No_Match;
      end if;

      Gnat.Regpat.Match (Self       => Regexp_Holders.Element (Self.R),
                         Data       => Data,
                         Matches    => Matches);

      if Matches (Matches'First) = Gnat.Regpat.No_Match then
         return No_Match;
      else
         return Match_Result'(First => Matches (Matches'First).First,
                              Last  => Matches (Matches'First).Last);
      end if;
   end Match;



end Text_Scanners.Regexps;
