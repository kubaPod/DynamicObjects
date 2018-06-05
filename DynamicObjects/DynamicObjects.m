(* ::Package:: *)

(* ::Chapter:: *)
(* Metadata*)


(* Mathematica Package *)

(* :Title: DynamicObjects *)
(* :Context: DynamicObjects` *)
(* :Author: Kuba Podkalicki (kuba.pod@gmail.com) *)
(* :Date: Tue 29 May 2018 *)

(* :Keywords: *)
(* :Discussion: *)

(*
	   Why it is not easy?
	    - we need a separate symbol for each needed part e.g. Obj["name", part_Integer] could be name$part, 
	      already tricky but we need to make that replacement in our interface
	      
	      - any kind of values for Obj will not help because its nature is to be deep inside holding functions (Dynamic/Button etc)
	      - custom typesetting for Obj will not help because stuff from Dynamic/ButtonFunction etc is not typeset	    
	      - so we need to create a 'lexical' environment which will replace _Obj with appropriate_Symbol , keep in mind we have to avoid evaluation etc
	      
	    - we need to add getters/setters which will pass values to DynamicModule variables via FrontEndSymbol
	
	*)


  (*TODO:
  
     + investigate: "state$108$$","state$109" removal maybe?
         OK, symbols created on DM lifecycle:
             DynObjDump`state$i      // Parsed version  \[Rule] Remove @ DynObjDump`*
             DynObjDump`state$i$nn   // Module \[Rule] Temp \[Rule] no problem
             DynObjDump`state$i$$    // Typeset version \[Rule] Remove @ DynObjDump`*
          FE`DynObjDump`state$i$$dmn // Initialized FE variable \[Rule] Remove @ FE`DynObjDump`*$$dmn
     
     - feature: FrontEndModule with InheritScope \[Rule] DynamicModuleNumber[] for dynamic content generation\
     - feature: the above needs to generate parent DynamicModule with ranges exeeding those found in the body
     
     + improvment: Deinitialization should remove underlying symbols. (partialy done)
     
     + improvment: FrontEndSymbol setters should 'take' module number from a variable so it can be Blocked e.g. for asynchronous calls. It will help when setters are used multiple times during one evaluation
     
     - feature: Initial values for obj, probably one value threaded over symbols, for simplicty first.
     
     - feature: Assuming notebook context is set, one may want to not create scoped versions of those symbols, in cases where the total number is not known
       up front or the number is too big to matter but it is certain that on runtime only few will be accessed.
     
     - improvment: Pass DynamicModuleOptions via FrontEndModule
     
     - improvment: More extensive FrontEndSymbol setters. Support for ranges etc.
     
     - feature: Support for multidimensional FrontEndSymbol?
     
     - feature: utility function to convert _Dynamic to fe side version with FEPrivate` equivalents.
     
     - option: light locator env
     
     - option: replace colors
     
     - option: try to create fe side Dynamics
   *)



(* ::Chapter:: *)
(* Begin package*)


BeginPackage["DynamicObjects`"];

  Unprotect["`*", "`*`*"]
  ClearAll["`*", "`*`*"]
  
  
  $DynamicModuleNumber::usage = "Returns the number or $Failed + message." <> 
    "By default $DynamicModuleNumber:=DynamicModuleNumber[] but you can use it to block/inject the number e.g. when scheduling async tasks where DynamicModuleNumber[] would be out of luck."<>
    "It is better to use it in your functions in case someone will need to do something fancy with injected number.";
    
  DynamicModuleNumber::usage = "DynamicModuleNumber[] tries to determine the number based on the environment. Use $DynamicModuleNumber unlees you need this one";
  
  
  FrontEndModule;
  FrontEndSymbol;

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section:: *)
(*$DynamicModuleNumber *)


(*JF, y u no do dis?*)


$DynamicModuleNumber := DynamicModuleNumber[];


DynamicModuleNumber::noparent = "DynamicModuleNumber not executed in DynamicModule";
DynamicModuleNumber::nokids   = "DynamicModuleNumber can not access any variable of parent DynamicModule";


DynamicModuleNumber[]:= DynamicModuleNumber[FrontEnd`Private`names]


DynamicModuleNumber[HoldPattern[FrontEnd`Private`names]]:=(
  Message[DynamicModuleNumber::noparent]
; $Failed
);


DynamicModuleNumber[{}]:=(
  Message[DynamicModuleNumber::nokids]
; $Failed
);


DynamicModuleNumber[  {Hold[sym_Symbol],___}]:= First @ StringCases[
  SymbolName[Unevaluated[sym]]
, StringExpression[__, "$$", dmn:DigitCharacter.., EndOfString] :> ToExpression[dmn]
]


(* ::Section:: *)
(*FrontEndModule*)


(*$objSymbolTemplate = TemplateWith[
  {"context" \[Rule] "DynObjDump`"
  ,"symbolName" \[Rule] StringTemplate["`1`$`2`$$"]}
, StringTemplate["`context``symbolName`"]
];                 (* DynObjDump`name$index$$ *)
$objFESymbolTemplate = TemplateWith[
  { "context1" \[Rule] "FE`"
  , "context2" \[Rule] "DynObjDump`"
  , "symbolName" \[Rule] StringTemplate["`1`$`2`$$`3`"]}
, StringTemplate["`context1``context2``symbolName`"]
]
StringTemplate["FE<*\"`\"*>DynObjDump<*\"`\"*>``$``$$``"];  (* FE`DynObjDump`name$index$$dynamicModuleNumber *)*)


ObjectSymbolString[name_String, spec:(_String|_Integer)..]:= StringRiffle[
  {name, spec}
, {"DynObjDump`", "$", "$$"}];


ObjectFrontEndSymbolString[name_String, spec:(_String|_Integer).., dynModNumber_Integer]:=StringRiffle[
  {name, spec}
, {"FE`DynObjDump`", "$", "$$" <> ToString @ dynModNumber}
];


FrontEndModule // Options = Options @ DynamicModule;


FrontEndModule[expr_, patt: OptionsPattern[]]:=Module[
  {wrap, objs}
, SetAttributes[wrap, HoldAll]

; objs = Join @@ (Union @ Cases[expr, FrontEndSymbol[name_String, spec:(_String|_Integer)..] :> ToExpression[ObjectSymbolString[name, spec], StandardForm, Hold], \[Infinity]])

; FrontEndModule @@ (
    {
      objs (* Hold[sym1, sym2, ...] *)
    , expr (*body with DynamicObjects inside*)
    , patt (*options*)
    } /. 
      FrontEndSymbol[name_String, spec:(_String|_Integer)..]:>RuleCondition@ToExpression[ObjectSymbolString[name, spec], StandardForm, wrap] /.
        wrap[x_] :> x 
  )
];


FrontEndModule[Hold[sym__Symbol],expr_, patt: OptionsPattern[]]:=DynamicModule[
  {sym}
, expr

    (*let's not overheat the front end *)
, UnsavedVariables :> {sym}                  

    (*those symbols do not work anyway, were created during inital stages of DynamicModule generation*)
, Evaluate @ OptionValue[Automatic, Automatic, Initialization, 
    Function[init, Initialization :> (init; Remove["DynObjDump`*"] ), HoldAll]
  ]
    (*TODO: poor man's version of $DMN to not require user to Need DynamicObjects`*)
    (* this will remove FE`objName$index$$dmn symbols only, should add something here *)
, Evaluate @ OptionValue[Automatic, Automatic, Deinitialization, 
    Function[deinit, Deinitialization :> (deinit; Remove[#]& @ ("FE`DynObjDump`*$$"<>ToString[$DynamicModuleNumber]) ), HoldAll]
  ]
 
]


(* ::Section:: *)
(*FrontEndSymbol setter*)


(*TODO: can this be cached per $DynamicModuleNumber? *)


FrontEndSymbol /: Set[FrontEndSymbol[name_?StringQ, spec: (_?IntegerQ|_?StringQ)..], val_]:= Catch @ ToExpression[
  ObjectFrontEndSymbolString[name, spec, ($DynamicModuleNumber /. $Failed :> Throw @ $Failed)]
, StandardForm
, Function[symbol, symbol = val, HoldAll] 
]


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
