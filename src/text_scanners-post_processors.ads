with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Text_Scanners.Post_Processors is
   pragma SPARK_Mode (On);

   type Post_Processor is private;

   function Apply (P    : Post_Processor;
                   What : String)
                   return String;

   function "*" (X, Y : Post_Processor) return Post_Processor;

   function No_Processing return Post_Processor;

   type Case_Conversion is (Lower, Upper, None);
   type Trimming_Specs is (Head, Tail, Both, None);

   function Force_Case (To : Case_Conversion) return Post_Processor;
   function Trim (Spec : Trimming_Specs) return Post_Processor;

   type Processor_Interface is interface;

   function Process (P    : Processor_Interface;
                     What : String)
                     return String
                     is abstract;

   function Create (P : Processor_Interface'Class)
                    return Post_Processor;
private
   pragma SPARK_Mode (Off);

   package Processor_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Processor_Interface'Class);

   type Post_Processor is
      record
         H : Processor_Lists.List;
      end record;
end Text_Scanners.Post_Processors;
