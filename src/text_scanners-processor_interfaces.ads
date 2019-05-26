with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Text_Scanners.Processor_Interfaces is
   type Processor_Interface is interface;

   function Process (P    : Processor_Interface;
                     What : String)
                     return String
                     is abstract;

   package Processor_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Processor_Interface'Class);

end Text_Scanners.Processor_Interfaces;
