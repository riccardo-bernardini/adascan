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



end Text_Scanners.Regexps;
