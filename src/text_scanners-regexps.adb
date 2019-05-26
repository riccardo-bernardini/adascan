pragma Ada_2012;
package body Text_Scanners.Regexps is
   function ID_Regexp (Additional_ID_Chars : String := "";
                       Basic_ID_Chars      : String := "a-zA-Z0-9_";
                       Begin_ID_Chars      : String := "a-zA-Z")
                       return Unbounded_String
   is
      Tmp : constant String := "[" & Begin_ID_Chars & "]"
              &
              "[" & Basic_ID_Chars & Additional_ID_Chars & "]*";
   begin
      --        Ada.Text_IO.Put_Line ("<<" & Tmp & ">>");
      return To_Unbounded_String (Tmp);
      --          ("[" & Begin_ID_Chars & "]"
      --           &
      --           "[" & Basic_ID_Chars & Additional_ID_Chars & "]*");
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
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Number_Regexp unimplemented");
      return raise Program_Error with "Unimplemented function Number_Regexp";
   end Number_Regexp;

   function Regexp_Quote (Str : String) return Unbounded_String
   is
   begin
      return To_Unbounded_String (GNAT.Regpat.Quote (Str));
   end Regexp_Quote;

   function Float_Regexp return Unbounded_String is
   begin
      return To_Unbounded_String ("[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?");
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

   function Fixed_String (Str : String) return Regexp is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Fixed_String unimplemented");
      return raise Program_Error with "Unimplemented function Fixed_String";
   end Fixed_String;

end Text_Scanners.Regexps;
