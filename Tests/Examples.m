(* ::Package:: *)

(* ::Title:: *)
(*Examples*)


(* ::Section:: *)
(**)


Needs @ "DynamicObjects`"


Dynamic[
 Length/@{ Names["DynObjDump`*"], Names["*`DynObjDump`*"] }
,BaseStyle->"Chapter"
, UpdateInterval->5]


(* ::Section:: *)
(*Graph*)


(* ::Subsection::Closed:: *)
(*graph examples init*)


n=150;
names=Range[n];
pts=AssociationThread[names->N@CirclePoints[n]];
edges=RandomSample[Subsets[names,{2}],1000];


(* ::Subsection::Closed:: *)
(*simple example*)


DynamicModule[{i=1},
FrontEndModule[ Graphics[
    { { 
        Dynamic@If[TrueQ[ FrontEndSymbol["state",#1] || FrontEndSymbol["state", #2] ],{Thickness@.01,Red, Opacity[1]},{Black, Opacity[.1]}]
      , Line[{pts[#1],pts[#2]}]
      }& @@@ edges    
    , {
          AbsolutePointSize@7
        , Dynamic@If[TrueQ[FrontEndSymbol["state",#1]],Red,Black]
        , EventHandler[Point@pts[#],{"MouseEntered":>(FrontEndSymbol["state",#1]=True),"MouseExited":>(FrontEndSymbol["state",#1]=False)}]
        
       }& /@ names
    }
  , ImageSize -> Medium
  , Background->White
  ],
  Initialization :> ( Set[FrontEndSymbol["state", #], False] & /@ Range[n])
  ]
]


(* ::Subsection::Closed:: *)
(*scheduled tasks with local variables*)


Panel@DynamicModule[{i=1},
FrontEndModule @ Column[{
  Graphics[
    { {Dynamic@If[TrueQ[FrontEndSymbol["state",#1]||FrontEndSymbol["state", #2]],{Thickness@.01,Red},Black],Line[{pts[#1],pts[#2]}]}& @@@ edges
    , PointSize[0.025]
    , {Dynamic@If[TrueQ[FrontEndSymbol["state",#1]],Red,Black],EventHandler[Point@pts[#],{"MouseEntered":>(FrontEndSymbol["state",#1]=True),"MouseExited":>(FrontEndSymbol["state",#1]=False)}]}&/@names
    }
  , ImageSize->Large
  ]
, Button["easy", FrontEndSymbol["state", 1] = True]
, Button["tricky", Module[{n=RandomInteger[n]}, FrontEndSymbol["state",n] = True]]
, Button["tricky and neat async",With[{dm = DynamicModuleNumber[]}, RunScheduledTask[Block[{$DynamicModuleNumber=dm},(FrontEndSymbol["state",#]=True)&[i++]],{.05,Length@names}]], Method->"Queued"]

}]
]


RemoveScheduledTask/@ScheduledTasks[]


(* ::Subsection:: *)
(*FE side example*)


DynamicModule[
  {i=1}
, FrontEndModule[
      Graphics[
      { 
        { DynamicBox[
            FEPrivate`If[
              FEPrivate`SameQ[
                FEPrivate`Or[ FrontEndSymbol["state",#1], FrontEndSymbol["state", #2]]
              , True
              ]
            , RGBColor[1,0,1]
            , RGBColor[0. ,0. ,0. , .1]
            
            ]
          ]
        , Line[{pts[#1],pts[#2]}]
        }& @@@ edges
      , PointSize[0.025]
      , { 
          DynamicBox[FEPrivate`If[SameQ[FrontEndSymbol["state",#1],True],RGBColor[1,0,1],RGBColor[0,0,0]]]
        , EventHandler[Point@pts[#]
          , { "MouseEntered" :> FEPrivate`Set[FrontEndSymbol["state",#1],True]
            , "MouseExited":>FEPrivate`Set[FrontEndSymbol["state",#1],False]
            }
          ]
        }& /@ names
      }
    , ImageSize->Large
    ]
  , Initialization :> ( Set[FrontEndSymbol["state", #],False] & /@ Range[n])
  ]
, SaveDefinitions->True (*For FES upvalues and `n`*)
]


Export[
  "test.cdf"
, %
]
(*can't use CDFDeploy due to a bug: https://mathematica.stackexchange.com/q/89473/5478 *)
