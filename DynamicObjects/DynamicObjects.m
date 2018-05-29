(* ::Package:: *)

(* ::Chapter:: *)
(* Metadata*)


(* Mathematica Package *)

(* :Title: DynamicObjects *)
(* :Context: DynamicObjects` *)
(* :Author: Kuba Podkalicki (kuba.pod@gmail.com) *)
(* :Date: Tue 29 May 2018 23:56:43 *)

(* :Keywords: *)
(* :Discussion: *)

(*
	   Why it is not easy?
	    - we need a separate symbol for each needed part e.g. Obj["name", part_Integer] could be name$part, 
	      already tricky but we need to make that replacement in our interface
	      
	      - any kind of values for Obj will not help because its nature is to be deep inside holding functions (Dynamic/Button etc)
	      - custom typesetting for Obj will not help because stuff from Dynamic/ButtonFunction etc is not typeset	    
	      - so we need to create a 'lexical' environment which will replace _Obj with appropriate_Symbol , keep in mind we have to avoid evaluation etc
	      
	    - we need to add getters/setters which will pass values to DynamicModule variables via DynamicObject
	
	*)


  (*TODO: 
     + investigate: "state$108$$","state$109" removal maybe?
         OK, symbols created on DM lifecycle:
             DynObjDump`state$i      // Parsed version  \[Rule] Remove @ DynObjDump`*
             DynObjDump`state$i$nn   // Module \[Rule] Temp \[Rule] no problem
             DynObjDump`state$i$$    // Typeset version \[Rule] Remove @ DynObjDump`*
          FE`DynObjDump`state$i$$dmn // Initialized FE variable \[Rule] Remove @ FE`DynObjDump`*$$dmn
     
     - feature: DynamicObjectModule with InheritScope \[Rule] DynamicModuleNumber[] for dynamic content generation\
     - feature: the above needs to generate parent DynamicModule with ranges exeeding those found in the body
     
     + improvment: Deinitialization should remove underlying symbols. (partialy done)
     
     + improvment: DynamicObject setters should 'take' module number from a variable so it can be Blocked e.g. for asynchronous calls. It will help when setters are used multiple times during one evaluation
     
     - feature: Initial values for obj, probably one value threaded over symbols, for simplicty first.
     
     - feature: Assuming notebook context is set, one may want to not create scoped versions of those symbols, in cases where the total number is not known
       up front or the number is too big to matter but it is certain that on runtime only few will be accessed.
     
     - improvment: Pass DynamicModuleOptions via DynamicObjectModule
     
     - improvment: More extensive DynamicObject setters. Support for ranges etc.
     
     - feature: Support for multidimensional DynamicObject? 
   *)



(* ::Chapter:: *)
(* Begin package*)


BeginPackage["DynamicObjects`"];

  Unprotect["`*", "`*`*"]
  ClearAll["`*", "`*`*"]
  
  DynamicModuleNumber;
  $DynamicModuleNumber;
  
  DynamicObjectModule;
  DynamicObject;

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


$DynamicModuleNumber := DynamicModuleNumber[];


DynamicModuleNumber::noparent="DynamicModuleNumber not executed in DynamicModule";
DynamicModuleNumber::nokids="DynamicModuleNumber can not access any variable of parent DynamicModule";

DynamicModuleNumber[]:= DynamicModuleNumber[FrontEnd`Private`names]

DynamicModuleNumber[HoldPattern[FrontEnd`Private`names]]:=(Message[DynamicModuleNumber::noparent];$Failed);

DynamicModuleNumber[{}]:=(Message[DynamicModuleNumber::nokids];$Failed);

DynamicModuleNumber[{Hold[sym_Symbol],___}]:=First @ StringCases[SymbolName[Unevaluated[sym]],__~~"$$"~~dmn:DigitCharacter..~~EndOfString:>ToExpression[dmn]]


$objSymbolTemplate = StringTemplate["DynObjDump<*\"`\"*>``$``$$"];
$objFESymbolTemplate = StringTemplate["FE<*\"`\"*>DynObjDump<*\"`\"*>``$``$$``"];


DynamicObjectModule[expr_]:=Module[
  {wrap, objs}
, SetAttributes[wrap, HoldAll]

; objs = Join @@ (Union @ Cases[expr, DynamicObject[name_String, n_Integer] :> ToExpression[$objSymbolTemplate[name,n], StandardForm, wrap], \[Infinity]])

; DynamicObjectModule[
    Hold @@ objs
  , expr /. DynamicObject[name_String, n_Integer]:>RuleCondition@ToExpression[$objSymbolTemplate[name,n], StandardForm, wrap] /. wrap[x_]:>x
  ]
];


DynamicObjectModule[Hold[sym__Symbol],expr_]:=DynamicModule[
  {sym}
, expr
, UnsavedVariables :> {sym} (*let's not overheat the front end *)
, Initialization :> (Remove["DynObjDump`*"]) (*those symbols do not work anyway, were created during inital stages of DynamicModule generation*)
, Deinitialization :> (
     (*TODO: poor man's version of $DMN to not require user to Need DynamicObjects`*)
    Remove[#]& @ ("FE`DynObjDump`*$$"<>ToString[$DynamicModuleNumber])
  ) (* this will remove FE`objName$index$$dmn symbols only, should add something here *)
]


DynamicObject /: Set[DynamicObject[name_?StringQ, n_?IntegerQ], val_]:= Catch @ ToExpression[
  $objFESymbolTemplate[name, n, ($DynamicModuleNumber /. $Failed :> Throw @ $Failed)]
, StandardForm
, Function[symbol, symbol = val, HoldAll] 
]


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
