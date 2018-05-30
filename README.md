# DynamicObjects (in production...)

Mini framework helping to scale Dynamics up

# Background 

Will elaborate later, basically the problem with scaling of Dynamic is as explained here: [Allow multiple GUI elements to react dynamically to interaction with a single element](https://mathematica.stackexchange.com/q/128344/5478).

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
DynamicObjectModule[ 
  Graphics[
    { 
      { 
        Dynamic @ If[ TrueQ[ DynamicObject["state",#1] || DynamicObject["state", #2] ], Red, Black]
      , Line[{pts[#1],pts[#2]}]
      }& @@@ edges    
    , {
        AbsolutePointSize@7
      , Dynamic @ If[ TrueQ[DynamicObject["state",#1]], Red, Black]
      , EventHandler[Point@pts[#],{"MouseEntered":>(DynamicObject["state",#1]=True),"MouseExited":>(DynamicObject["state",#1]=False)}]
      }& /@ names
    }
  , ImageSize -> Medium
  ]
]
```