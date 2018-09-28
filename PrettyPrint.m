(* ::Package:: *)

BeginPackage["PrettyPrint`"];
RemoveTrailingPoint;
RemoveTrailingPoint::usage="Suppressing a trailing \[OpenCurlyDoubleQuote].\[CloseCurlyDoubleQuote] in output from Mathematica. Example: '5.' -> '5'
In: S=1.22522;PrettyPrint`Variable[\"\!\(\*SubscriptBox[\(S\), \(var\)]\)\", S, 3];
Out: \!\(\*SubscriptBox[\(S\), \(var\)]\)=1.23";
PrintV;
PrintV::usage="Print variable with precision and name";

Begin["Private`"];
(*4 all*)
PrintV[var_,precision_Integer:2] := Print[
RemoveTrailingPoint[NumberForm[N@var,precision]//ToString]
];
PrintV[name_?StringQ,var_,precision_:2] := Print[
name,"=", RemoveTrailingPoint[NumberForm[N@var,precision]//ToString]
];
(*4 Matrix*)
PrintV[var_?MatrixQ,precision_Integer:2] := Print[
PrettyPrint`RemoveTrailingPoint[NumberForm[N@var,precision]//ToString//ToExpression]//MatrixForm
];
PrintV[name_?StringQ,var_?MatrixQ,precision_Integer:2] := Print[
name,"=", PrettyPrint`RemoveTrailingPoint[NumberForm[N@var,precision]//ToString//ToExpression]//MatrixForm
];


RemoveTrailingPoint[x_]:=StringReplace[ToString@x,RegularExpression@"\\.$"->""]//ToExpression;
RemoveTrailingPoint[x_?ListQ]:=Map[RemoveTrailingPoint[#]&, x, 1];
RemoveTrailingPoint[x_?MatrixQ]:=Map[RemoveTrailingPoint[#]&,x,2];
End[];

EndPackage[];
