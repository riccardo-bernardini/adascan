pragma Ada_2012;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Ada.Strings.Fixed;

package body Text_Scanners.Post_Processors is
   pragma SPARK_Mode (On);
   pragma Warnings (Off, "no Global contract available for ""To_Unbounded_String""");
   pragma Warnings (Off, "no Global contract available for ""To_String""");
   pragma Warnings (Off, "no Global contract available for ""Last""");
   pragma Warnings (Off, "no Global contract available for ""Element""");
   pragma Warnings (Off, "no Global contract available for ""Previous""");
   pragma Warnings (Off, "no Global contract available for ""Append""");


   use Processor_Interfaces;


   type Case_Processor is new Processor_Interface
   with
      record
         Action : Case_Conversion;
      end record;

   overriding
   function Process (P : Case_Processor; What : String) return String
     with Global => null;

   function Process (P : Case_Processor; What : String)
                     return String
   is
   begin
      return (case P.Action is
                 when Lower =>
                   To_Lower (What),

                 when Upper =>
                    To_Upper (What));
   end Process;

   type Trimmer is new Processor_Interface
   with
      record
         Action : Trimming_Specs;
      end record;

   overriding
   function Process (P : Trimmer; What : String) return String
     with Global => null;

   function Process (P : Trimmer; What : String) return String
   is
      pragma Warnings (Off, "no Global contract available for ""Trim""");

      use Ada;
   begin
      return (case P.Action is
                 when Head =>
                    Strings.Fixed.Trim (What, Strings.Left),

                 when Tail =>
                    Strings.Fixed.Trim (What, Strings.Right),

                 when Both =>
                    Strings.Fixed.Trim (What, Strings.Both));
   end Process;

   -----------
   -- Apply --
   -----------

   function Apply
     (P    : Post_Processor;
      What : String)
      return String
   is
      use Processor_Lists;

      Tmp : Unbounded_String := To_Unbounded_String (What);
      Pos : Cursor := P.Process_Chain.Last;
   begin
      while Pos /= No_Element loop
         Tmp := To_Unbounded_String (Element (Pos).Process (To_String (Tmp)));
         Pos := Previous (Pos);
      end loop;

      return To_String (Tmp);
   end Apply;

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Post_Processor) return Post_Processor with SPARK_Mode => Off is
      use Processor_Lists;

      Pos : Cursor := Y.Process_Chain.Last;
      Result : Post_Processor := X;
   begin
      while Pos /= No_Element loop
         Result.Process_Chain.Append (Element(Pos));
      end loop;

      return Result;
   end "*";



   ----------------
   -- Force_Case --
   ----------------

   function Force_Case (To : Case_Conversion) return Post_Processor is
      pragma SPARK_Mode (Off);
   begin
      return Create (Case_Processor'(Action => To));
   end Force_Case;

   ----------
   -- Trim --
   ----------

   function Trim (Spec : Trimming_Specs) return Post_Processor is
      pragma SPARK_Mode (Off);
   begin
      return Create (Trimmer'(Action => Spec));
   end Trim;

   ------------
   -- Create --
   ------------

   function Create
     (P : Processor_Interface'Class)
      return Post_Processor
   is
      pragma SPARK_Mode (Off);

      Result : Post_Processor := (Process_Chain => Processor_Lists.Empty_List);
   begin
      Result.Process_Chain.Append (P);
      return Result;
   end Create;

end Text_Scanners.Post_Processors;
