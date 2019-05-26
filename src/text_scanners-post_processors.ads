with Text_Scanners.Processor_Interfaces;


package Text_Scanners.Post_Processors is
   pragma SPARK_Mode (On);

   type Post_Processor is private;

   function Apply (P    : Post_Processor;
                   What : String)
                   return String;

   function "*" (X, Y : Post_Processor) return Post_Processor with SPARK_Mode => Off ;

   function No_Processing return Post_Processor;

   type Case_Conversion is (Lower, Upper);
   type Trimming_Specs is (Head, Tail, Both);

   function Force_Case (To : Case_Conversion) return Post_Processor;
   function Trim (Spec : Trimming_Specs) return Post_Processor;



   function Create (P : Processor_Interfaces.Processor_Interface'Class)
                    return Post_Processor;
private
   package Processor_Lists renames Processor_Interfaces.Processor_Lists;

   type Post_Processor is
      record
         Process_Chain : Processor_Lists.List;
      end record;

   function No_Processing return Post_Processor
   is (Post_Processor'(Process_Chain => Processor_Lists.Empty_List));
end Text_Scanners.Post_Processors;
