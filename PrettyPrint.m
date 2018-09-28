(* ::Package:: *)

BeginPackage["PrettyPrint`"];
RemoveTrailingPoint;
RemoveTrailingPoint::usage="Suppressing a trailing \[OpenCurlyDoubleQuote].\[CloseCurlyDoubleQuote] in output from Mathematica. Example: '5.' -> '5'
In: S=1.22522;PrettyPrint`Variable[\"\!\(\*SubscriptBox[\(S\), \(var\)]\)\", S, 3];
Out: \!\(\*SubscriptBox[\(S\), \(var\)]\)=1.23";
PrintV;
PrintV::usage="Print variable with precision and name";

Begin["Private`"];
PrintV[var_,precision_?IntegerQ:2] := Print[
PrettyPrint`RemoveTrailingPoint[NumberForm[N@var,precision]//ToString//ToExpression]
];
PrintV[name_?StringQ,var_,precision_?IntegerQ:2] := Print[
name,"=", PrettyPrint`RemoveTrailingPoint[NumberForm[N@var,precision]//ToString//ToExpression]
];
PrintV[name_?StringQ,var_?MatrixQ,precision_?IntegerQ:2] := Print[
name,"=", PrettyPrint`RemoveTrailingPoint[NumberForm[N@var,precision]//ToString//ToExpression]//MatrixForm
];

RemoveTrailingPoint[x_]:=StringReplace[ToString@x,RegularExpression@"\\.$"->""]//ToExpression
RemoveTrailingPoint[x_?ListQ]:=Map[RemoveTrailingPoint[#]&, x, 1];
RemoveTrailingPoint[x_?MatrixQ]:=Map[RemoveTrailingPoint[#]&,x,2];
End[];

EndPackage[];



