# DynamicObjects

Mini framework helping to scale Dynamics up. 

There are many features and enhancements in the pipeline. 

## Installation
 
### Manual
 
   Go to 'releases' tab and download appropriate .paclet file.
    
   Run `PacletInstall @ path/to/the.paclet` file
   
### Via ``MPM` ``
   
If you don't have ``MPM` `` yet, run:
   
    Import["https://raw.githubusercontent.com/kubapod/mpm/master/install.m"]
   
and then:
   
    Needs @ "MPM`"    
    MPM`MPMInstall["kubapod", "dynamicobjects"]

# Background 

The problem with scaling of Dynamic is as explained here: [Allow multiple GUI elements to react dynamically to interaction with a single element](https://mathematica.stackexchange.com/q/128344/5478).

The solution to linked problem is not readable / approachable unless you are a power user:


```Mathematica
DynamicModule[{}
, Graphics[
    {
      (
        ToExpression[
          "{sA:=state" <> ToString[#] <> ", sB:=state" <> ToString[#2] <> "}"
        , StandardForm
        , Hold
        ] /. Hold[spec_] :> With[
          spec, {  Dynamic @ If[TrueQ[sA || sB], Red, Black], Line[{pts[#1], pts[#2]}] }
        ]
      ) & @@@ edges
    , PointSize[0.025]
    , (
        ToExpression[
          "{sA:=state" <> ToString[#] <> "}"
        , StandardForm
        , Hold
        ] /. Hold[spec_] :> With[
          spec
        , { Dynamic @ If[TrueQ[sA], Red, Black]
          , EventHandler[ Point @ pts[#], {"MouseEntered" :> (sA = True), "MouseExited" :> (sA = False)}  ]
          }
        ]
      ) & /@ names
    }
  , ImageSize -> Large
  ]
]
```

Using this package it is way more handy, additionally it scopes all generated symbols unlike the code above (that would make it even worse):

```Mathematica
FrontEndModule[ 
  Graphics[
    { 
      { 
        Dynamic @ If[ TrueQ[ FrontEndSymbol["state",#1] || FrontEndSymbol["state", #2] ], Red, Black]
      , Line[{pts[#1],pts[#2]}]
      }& @@@ edges    
    , {
        AbsolutePointSize@7
      , Dynamic @ If[ TrueQ[FrontEndSymbol["state",#1]], Red, Black]
      , EventHandler[Point@pts[#],{"MouseEntered":>(FrontEndSymbol["state",#1]=True),"MouseExited":>(FrontEndSymbol["state",#1]=False)}]
      }& /@ names
    }
  , ImageSize -> Medium
  ]
]
```

## Bonus

If you know what you are doing you can even create a completely FrontEnd side interaction:

```Mathematica
<< DynamicObjects`

n = 150;
names = Range[n];
pts = AssociationThread[names -> N@CirclePoints[n]];
edges = RandomSample[Subsets[names, {2}], 2000];

FrontEndModule[
 Graphics[
  { 
    { RawBoxes @ DynamicBox[
        FEPrivate`If[
          FEPrivate`SameQ[ FEPrivate`Or[ FrontEndSymbol["state",#1], FrontEndSymbol["state", #2]], True ]
        , RGBColor[1,0,1]
        , RGBColor[0. ,0. ,0. , .1]
        
        ]
      ]
    , Line[{pts[#1],pts[#2]}]
    }& @@@ edges
  , PointSize[0.025]
  , { 
      RawBoxes @ DynamicBox[
        FEPrivate`If[ 
          SameQ[FrontEndSymbol["state",#1],True]
        , RGBColor[1,0,1]
        , RGBColor[0,0,0]
        ]
      ]
    , EventHandler[Point@pts[#]
      , { "MouseEntered" :> FEPrivate`Set[FrontEndSymbol["state",#1],True]
        , "MouseExited":>FEPrivate`Set[FrontEndSymbol["state",#1],False]
        }
      ]
    }& /@ names
  }
, ImageSize->Large
],
Initialization :> ( Set[FrontEndSymbol["state", #],False] & /@ Range[n])
]
```


![Alt text](src/example-fe-side.gif?raw=true "v-manipulate")  