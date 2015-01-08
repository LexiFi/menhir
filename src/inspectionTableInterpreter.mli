(* This functor is invoked inside the generated parser, in [--table] mode. It
   constructs the inspection API on top of the inspection tables described in
   [InspectionTableFormat]. *)

module Make (T : InspectionTableFormat.TABLES
             with type 'a lr1state = int)

: IncrementalEngine.INSPECTION
  with type 'a lr1state := 'a T.lr1state
   and type 'a symbol := 'a T.symbol
   and type xsymbol := T.xsymbol
   and type production := int

