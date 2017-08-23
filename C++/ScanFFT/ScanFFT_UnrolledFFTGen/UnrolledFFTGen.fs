module Program

open System

//-------------------------------------------------------------------------------
let notImpl (message : string) : 'T =
    raise (NotImplementedException message)

//-------------------------------------------------------------------------------
type Complex = System.Numerics.Complex

type FloatVRegister = { Name : string; Dimension : int }
type FloatPtrVariable = { Name : string; Readonly : bool }

and FloatVExpression = 
    | FV_Literal of Values : List<double>
    | FV_InterleavedLow of Operand0 : FloatVRegister * Operand1 : FloatVRegister * SubDimension : int
    | FV_InterleavedHi of Operand0 : FloatVRegister * Operand1 : FloatVRegister * SubDimension : int
    | FV_Minus of Operand : FloatVRegister
    | FV_Add of Operand0 : FloatVRegister * Operand1 : FloatVRegister
    | FV_Subtract of Operand0 : FloatVRegister * Operand1 : FloatVRegister
    | FV_Multiply of Operand0 : FloatVRegister * Operand1 : FloatVRegister

type Statement = 
    | S_EvaluateFloatV of Register : FloatVRegister * Expression : FloatVExpression
    | S_LoadFloatV of Register : FloatVRegister * Ptr : FloatPtrVariable * Index : int * Dimension : int
    | S_StoreFloatV of Ptr : FloatPtrVariable * Index : int * Register : FloatVRegister

type Function = { Name : string; Parameters : List<FloatPtrVariable>; Body : List<Statement> }

//-------------------------------------------------------------------------------
let DimensionOf (exp : FloatVExpression) : int = 
    match exp with
    | FV_Literal values -> values.Length
    | FV_InterleavedLow (op0, op1, subD) when op0.Dimension = op1.Dimension -> op0.Dimension
    | FV_InterleavedHi (op0, op1, subD) when op0.Dimension = op1.Dimension -> op0.Dimension
    | FV_Minus op -> op.Dimension
    | FV_Add (op0, op1) when op0.Dimension = op1.Dimension -> op0.Dimension
    | FV_Subtract (op0, op1) when op0.Dimension = op1.Dimension -> op0.Dimension
    | FV_Multiply (op0, op1) when op0.Dimension = op1.Dimension -> op0.Dimension
    | _ -> notImpl ""

let EliminateDeadCode (stats : List<Statement>) : List<Statement> = 
    let TrackUsedRegisters 
        (exp : FloatVExpression) 
        (usedRegisters : Set<FloatVRegister>) 
        : Set<FloatVRegister>= 
        match exp with 
        | FV_Literal _ -> usedRegisters
        | FV_InterleavedLow (op0, op1, sd) -> usedRegisters.Add(op0).Add(op1)
        | FV_InterleavedHi (op0, op1, sd) -> usedRegisters.Add(op0).Add(op1)
        | FV_Minus op -> usedRegisters.Add(op)
        | FV_Add (op0, op1) -> usedRegisters.Add(op0).Add(op1)
        | FV_Subtract (op0, op1) -> usedRegisters.Add(op0).Add(op1)
        | FV_Multiply (op0, op1) -> usedRegisters.Add(op0).Add(op1)

    let rec BackwardProcess 
        (reversedStats : List<Statement>) 
        (usedRegisters : Set<FloatVRegister>) 
        (newStats : List<Statement>)
        : List<Statement> =
        match reversedStats with
        | [] -> newStats
        | (S_EvaluateFloatV (reg, exp)) as stat :: restStats when usedRegisters.Contains reg -> 
            BackwardProcess restStats (TrackUsedRegisters exp usedRegisters) (stat :: newStats)
        | (S_EvaluateFloatV (reg, exp)) :: restStats -> 
            BackwardProcess restStats usedRegisters newStats
        | (S_LoadFloatV (reg, _, _, _)) as stat :: restStats when usedRegisters.Contains reg -> 
            BackwardProcess restStats usedRegisters (stat :: newStats)
        | (S_LoadFloatV (reg, _, _, _)) :: restStats -> 
            BackwardProcess restStats usedRegisters newStats
        | (S_StoreFloatV (_, _, reg)) as stat :: restStats -> 
            BackwardProcess restStats (usedRegisters.Add reg) (stat :: newStats)

    BackwardProcess (List.rev stats) Set.empty []

//-------------------------------------------------------------------------------
module Utils = 
    let FloatEquals (f0 : double) (f1 : double) : bool =
        Math.Abs(f0 - f1) < 0.000001

    let ComplexEquals (c0 : Complex) (c1 : Complex) : bool = 
        FloatEquals c0.Real c1.Real && FloatEquals c0.Imaginary c1.Imaginary

    let TwiddleFactor (e : int) (n : int) : Complex = 
        Complex.FromPolarCoordinates(1.0, 2.0 * Math.PI * (double e) / (double n))

//-------------------------------------------------------------------------------
module Constant =
    let kFFTFuncPrefix = "FFT_"
    let kInverseFFTFuncPrefix = "IFFT_"
    let kInplaceFFTFuncPrefix = "InplaceFFT_"
    let kInverseInplaceFFTFuncPrefix = "InplaceIFFT_"

//-------------------------------------------------------------------------------
[< AbstractClass >]
type FFTFunctionGenerator() = class
    let mutable statements : List<Statement> = []
    let mutable exp2Register : Map<FloatVExpression, FloatVRegister> = Map.empty
    let mutable register2Exp : Map<FloatVRegister, FloatVExpression> = Map.empty
    let mutable nextRegisterId : int = 0

    member this.Reset() = 
        statements <- []
        exp2Register <- Map.empty
        register2Exp <- Map.empty
        nextRegisterId <- 0

    member private this.NewRegisterName() : string = 
        let id = nextRegisterId
        nextRegisterId <- nextRegisterId + 1
        sprintf "temp_%d" id

    member private this.EvaluateFloatV (exp : FloatVExpression) : FloatVRegister =
        let reg = exp2Register.TryFind exp
        if reg.IsSome then
            reg.Value
        else
            let reg : FloatVRegister = { Name=this.NewRegisterName(); Dimension=DimensionOf exp }
            exp2Register <- exp2Register.Add(exp, reg)
            register2Exp <- register2Exp.Add(reg, exp)
            statements <- (S_EvaluateFloatV (reg, exp)) :: statements
            reg

    member private this.MinusFloatV (op : FloatVRegister) : FloatVRegister = 
        this.EvaluateFloatV (FV_Minus op)

    member private this.AddFloatV (op0 : FloatVRegister) (op1 : FloatVRegister) : FloatVRegister = 
        if op0.Name > op1.Name then
            this.AddFloatV op1 op0
        else
            match (register2Exp.TryFind op0, register2Exp.TryFind op1) with
            | Some (FV_Minus n0), Some (FV_Minus n1) -> this.MinusFloatV (this.AddFloatV n0 n1)
            | Some (FV_Minus n0), _ -> this.SubtractFloatV op1 n0
            | _, Some (FV_Minus n1) -> this.SubtractFloatV op0 n1
            | _, _ -> this.EvaluateFloatV (FV_Add (op0, op1))

    member private this.SubtractFloatV (op0 : FloatVRegister) (op1 : FloatVRegister) : FloatVRegister = 
        match (register2Exp.TryFind op0, register2Exp.TryFind op1) with
            | Some (FV_Minus n0), Some (FV_Minus n1) -> this.SubtractFloatV n1 n0
            | Some (FV_Minus n0), _ -> this.MinusFloatV (this.AddFloatV n0 op1)
            | _, Some (FV_Minus n1) -> this.AddFloatV op0 n1
            | _, _ -> this.EvaluateFloatV (FV_Subtract (op0, op1))

    member this.InterleavedLowComplexV
        (c0 : FloatVRegister * FloatVRegister) 
        (c1 : FloatVRegister * FloatVRegister) 
        (subDimension : int) : 
        FloatVRegister * FloatVRegister =
        let realReg = this.EvaluateFloatV (FV_InterleavedLow ((fst c0), (fst c1), subDimension))
        let imagReg = this.EvaluateFloatV (FV_InterleavedLow ((snd c0), (snd c1), subDimension))
        (realReg, imagReg)

    member this.InterleavedHiComplexV
        (c0 : FloatVRegister * FloatVRegister) 
        (c1 : FloatVRegister * FloatVRegister) 
        (subDimension : int) : 
        FloatVRegister * FloatVRegister =
        let realReg = this.EvaluateFloatV (FV_InterleavedHi ((fst c0), (fst c1), subDimension))
        let imagReg = this.EvaluateFloatV (FV_InterleavedHi ((snd c0), (snd c1), subDimension))
        (realReg, imagReg)

    member this.LoadComplexV
        (realPtr : FloatPtrVariable, imagPtr : FloatPtrVariable) 
        (index : int)
        (dimension : int)
        : FloatVRegister * FloatVRegister =
        let realReg : FloatVRegister = { Name=this.NewRegisterName(); Dimension=dimension }
        let imagReg : FloatVRegister = { Name=this.NewRegisterName(); Dimension=dimension }
        statements <- (S_LoadFloatV (realReg, realPtr, index, dimension)) :: statements
        statements <- (S_LoadFloatV (imagReg, imagPtr, index, dimension)) :: statements
        (realReg, imagReg)

    member this.StoreComplexV 
        (realPtr : FloatPtrVariable, imagPtr : FloatPtrVariable)
        (index : int)
        (reg : FloatVRegister * FloatVRegister) = 
        statements <- (S_StoreFloatV (realPtr, index, fst reg)) :: statements
        statements <- (S_StoreFloatV (imagPtr, index, snd reg)) :: statements

    member this.ScaleComplexV 
        (realReg : FloatVRegister, imagReg : FloatVRegister) 
        (scale : double) : 
        FloatVRegister * FloatVRegister =
        let scaler = this.EvaluateFloatV (FV_Literal (List.replicate realReg.Dimension scale))
        (this.EvaluateFloatV (FV_Multiply (realReg, scaler)), this.EvaluateFloatV (FV_Multiply (imagReg, scaler)))

    member this.AddComplexV 
        (op0Real : FloatVRegister, op0Imag : FloatVRegister) 
        (op1Real : FloatVRegister, op1Imag : FloatVRegister) 
        : FloatVRegister * FloatVRegister =  
        this.AddFloatV op0Real op1Real, this.AddFloatV op0Imag op1Imag

    member this.SubtractComplexV 
        (op0Real : FloatVRegister, op0Imag : FloatVRegister) 
        (op1Real : FloatVRegister, op1Imag : FloatVRegister) 
        : FloatVRegister * FloatVRegister = 
        this.SubtractFloatV op0Real op1Real, this.SubtractFloatV op0Imag op1Imag

    member private this.MultiplyComplexV 
        (op0Real : FloatVRegister, op0Imag : FloatVRegister) 
        (op1Real : FloatVRegister, op1Imag : FloatVRegister) 
        : FloatVRegister * FloatVRegister = 

        let realTimesReal = this.EvaluateFloatV (FV_Multiply (op0Real, op1Real))
        let realTimesImag = this.EvaluateFloatV (FV_Multiply (op0Real, op1Imag))
        let imagTimesReal = this.EvaluateFloatV (FV_Multiply (op0Imag, op1Real))
        let imagTimesImag = this.EvaluateFloatV (FV_Multiply (op0Imag, op1Imag))

        this.SubtractFloatV realTimesReal imagTimesImag, this.AddFloatV realTimesImag imagTimesReal

    member private this.ParallelMultiplyLiteralComplexV
        (op0Real: FloatVRegister, op0Imag : FloatVRegister) 
        (cs : List<Complex>) :
        FloatVRegister * FloatVRegister =

        let subDimension = op0Real.Dimension / cs.Length
        let expandedCs = cs |> List.collect (fun c -> List.replicate subDimension c)
        let csRealReg = this.EvaluateFloatV (FV_Literal [for c in expandedCs -> c.Real])
        let csImagReg = this.EvaluateFloatV (FV_Literal [for c in expandedCs -> c.Imaginary])

        this.MultiplyComplexV (op0Real, op0Imag) (csRealReg, csImagReg)

    member private this.ParallelMultiplyLiteralComplex
        (op0Real: FloatVRegister, op0Imag : FloatVRegister) 
        (c : Complex) :
        FloatVRegister * FloatVRegister =

        if Utils.ComplexEquals c Complex.One then
            (op0Real, op0Imag)
        else if Utils.ComplexEquals -c Complex.One then
            this.MinusFloatV op0Real, this.MinusFloatV op0Imag
        else if Utils.ComplexEquals c Complex.ImaginaryOne then
            this.MinusFloatV op0Imag, op0Real
        else if Utils.ComplexEquals -c Complex.ImaginaryOne then
            op0Imag, this.MinusFloatV op0Real
        else if Utils.FloatEquals c.Real c.Imaginary then
            let realMinusImag = this.SubtractFloatV op0Real op0Imag
            let realPlusImag = this.AddFloatV op0Real op0Imag
            this.ScaleComplexV (realMinusImag, realPlusImag) c.Real
        else if Utils.FloatEquals c.Real -c.Imaginary then
            let realPlusImag = this.AddFloatV op0Real op0Imag
            let imagMinusReal = this.SubtractFloatV op0Imag op0Real
            this.ScaleComplexV (realPlusImag, imagMinusReal) c.Real
        else
            this.ParallelMultiplyLiteralComplexV (op0Real, op0Imag) [c]

    member this.MultiplyTwiddleFactors 
        (op0 : FloatVRegister * FloatVRegister) 
        (factors : List<Complex>) :
        FloatVRegister * FloatVRegister =
        if factors.Length = 1 then
            this.ParallelMultiplyLiteralComplex op0 (factors.Item 0)
        else
            this.ParallelMultiplyLiteralComplexV op0 factors

    member this.Statements() = EliminateDeadCode (List.rev statements)

end

type Radix2FFTFunctionGenerator() = class
    inherit FFTFunctionGenerator()

    member this.GenInplaceFFT (bits : int) (dlp : int) (direction : int) : Function = 
        this.Reset()

        let size = 1 <<< bits
        let destRealPtr : FloatPtrVariable = { Name="destReals"; Readonly=false }
        let destImagPtr : FloatPtrVariable = { Name="destImags"; Readonly=false }
        let funcName = (if direction > 0 then Constant.kInplaceFFTFuncPrefix else Constant.kInverseInplaceFFTFuncPrefix) 
                        + bits.ToString()

        let regs = this.RecursiveFFT (destRealPtr, destImagPtr) [0..size-1] 1 dlp direction 1.0
        let regDimension = (fst (regs.Item 0)).Dimension
        for i, reg in List.zip [0..regDimension..size-1] regs do
            this.StoreComplexV (destRealPtr, destImagPtr) i reg

        { Name=funcName; Parameters=[ destRealPtr; destImagPtr; ]; Body=this.Statements() }

    member this.GenFFT (bits : int) (dlp : int) (direction : int) : Function = 
        this.Reset()

        let size = 1 <<< bits
        let srcRealPtr : FloatPtrVariable = { Name="srcReals"; Readonly=true }
        let srcImagPtr : FloatPtrVariable = { Name="srcImags"; Readonly=true }
        let destRealPtr : FloatPtrVariable = { Name="destReals"; Readonly=false }
        let destImagPtr : FloatPtrVariable = { Name="destImags"; Readonly=false }
        let funcName = (if direction > 0 then Constant.kFFTFuncPrefix else Constant.kInverseFFTFuncPrefix) + bits.ToString()
        let scale = if direction > 0 then 1.0 else 1.0 / (double size)

        let regs = this.RecursiveFFT (srcRealPtr, srcImagPtr) [0..size-1] 1 dlp direction scale
        let regDimension = (fst (regs.Item 0)).Dimension
        for i, reg in List.zip [0..regDimension..size-1] regs do
            this.StoreComplexV (destRealPtr, destImagPtr) i reg

        { Name=funcName; Parameters=[destRealPtr; destImagPtr; srcRealPtr; srcImagPtr; ]; Body=this.Statements() }

    member private this.RecursiveFFT 
        (srcPtr : FloatPtrVariable * FloatPtrVariable)
        (srcIndices : List<int>)
        (srcDimension : int)
        (dlp : int)
        (direction : int)
        (scale : double)
        : List<FloatVRegister * FloatVRegister> =

        if srcIndices.Length = 1 then
            let srcValue = this.LoadComplexV srcPtr (srcIndices.Item 0) srcDimension
            if Utils.FloatEquals scale 1.0 then
                [ srcValue ]
            else
                [ this.ScaleComplexV srcValue scale ]
        else if srcIndices.Length > 2 && srcDimension < dlp then
            let src0, _ = List.foldBack (fun v (l0, l1) -> (v::l1, l0)) srcIndices ([],[])
            let regs = this.RecursiveFFT srcPtr src0 (srcDimension * 2) dlp direction scale |> List.chunkBySize 2
            let factors = [for i in 0..src0.Length - 1 -> Utils.TwiddleFactor (i * direction) srcIndices.Length]
                            |> List.chunkBySize (src0.Length / regs.Length)

            let mutable o0, o1 = [], []

            for f, reg01 in List.zip factors regs do 
                let r0, r1 = reg01.Item 0, reg01.Item 1
                let c1 = this.InterleavedHiComplexV r0 r1 srcDimension
                let c1TimesF = this.MultiplyTwiddleFactors c1 f
                let c0 = this.InterleavedLowComplexV r0 r1 srcDimension
                o0 <- (this.AddComplexV c0 c1TimesF) :: o0
                o1 <- (this.SubtractComplexV c0 c1TimesF) :: o1

            List.concat [List.rev o0; List.rev o1]
        else 
            let evenIndices, oddIndices = List.foldBack (fun v (l0, l1) -> (v::l1, l0)) srcIndices ([],[])

            let regs0 = this.RecursiveFFT srcPtr evenIndices srcDimension dlp direction scale
            let regs1 = this.RecursiveFFT srcPtr oddIndices srcDimension dlp direction scale
            let factors = [for i in 0..evenIndices.Length - 1 -> Utils.TwiddleFactor (i * direction) srcIndices.Length]
                            |> List.chunkBySize (evenIndices.Length / regs0.Length)

            let mutable o0, o1 = [], []

            for c0, c1, f in List.zip3 regs0 regs1 factors do 
                let c1TimesF = this.MultiplyTwiddleFactors c1 f
                o0 <- (this.AddComplexV c0 c1TimesF) :: o0
                o1 <- (this.SubtractComplexV c0 c1TimesF) :: o1

            List.concat [List.rev o0; List.rev o1]

end

//-------------------------------------------------------------------------------
type CppTranslator(floatType : string) = class 

    let mutable headerGenerated = false

    member private this.GenHeader() : string = 
        if headerGenerated then
            ""
        else
            headerGenerated <- true
            ""

    member private this.TranslateVariable (v : FloatPtrVariable) : string =
        match v with 
        | { Name=name; Readonly=true } -> sprintf "%s const *%s" floatType name
        | { Name=name; Readonly=false } -> sprintf "%s *%s" floatType name

    member private this.TranslateFloatVExpression (exp : FloatVExpression) : string = 
        match exp with
        | FV_Literal values -> 
            let literalSuffix = if floatType = "float" then "f" else ""
            sprintf "make_tuple(%s)" ([for v in values -> sprintf "%.20f%s" v literalSuffix] |> String.concat ",")
        | FV_InterleavedLow (op0, op1, subD) when op0.Dimension = op1.Dimension -> 
            let subCount = op0.Dimension / subD
            let indices = ([for i in 0..subCount-1 do if i % 2 = 0 then yield! [for j in 0..subD-1 -> i * subD + j] ])
            let values0 = [for i in indices -> sprintf "get<%d>(%s)" i op0.Name]
            let values1 = [for i in indices -> sprintf "get<%d>(%s)" i op1.Name]
            sprintf "make_tuple(%s)" (List.concat [values0; values1] |> String.concat ",")
        | FV_InterleavedHi (op0, op1, subD) when op0.Dimension = op1.Dimension -> 
            let subCount = op0.Dimension / subD
            let indices = ([for i in 0..subCount-1 do if i % 2 = 1 then yield! [for j in 0..subD-1 -> i * subD + j] ])
            let values0 = [for i in indices -> sprintf "get<%d>(%s)" i op0.Name]
            let values1 = [for i in indices -> sprintf "get<%d>(%s)" i op1.Name]
            sprintf "make_tuple(%s)" (List.concat [values0; values1] |> String.concat ",")
        | FV_Minus op -> 
            sprintf "make_tuple(%s)" (([for i in 0..op.Dimension-1 -> sprintf "-get<%d>(%s)" i op.Name] |> String.concat ","))
        | FV_Add (op0, op1) when op0.Dimension = op1.Dimension -> 
            sprintf "make_tuple(%s)" 
                    ([for i in 0..op0.Dimension-1 -> sprintf "get<%d>(%s)+get<%d>(%s)" i op0.Name i op1.Name] |> String.concat ",")
        | FV_Subtract (op0, op1) when op0.Dimension = op1.Dimension -> 
            sprintf "make_tuple(%s)" 
                    ([for i in 0..op0.Dimension-1 -> sprintf "get<%d>(%s)-get<%d>(%s)" i op0.Name i op1.Name] |> String.concat ",")
        | FV_Multiply (op0, op1) when op0.Dimension = op1.Dimension -> 
            sprintf "make_tuple(%s)" 
                    ([for i in 0..op0.Dimension-1 -> sprintf "get<%d>(%s)*get<%d>(%s)" i op0.Name i op1.Name] |> String.concat ",")
        | _ -> notImpl ""

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateFloatV (r, e) -> sprintf "%sauto %s=%s;" ident r.Name (this.TranslateFloatVExpression e)
        | S_LoadFloatV (r, p, i, d) -> 
            sprintf "%sauto %s=make_tuple(%s);" 
                    ident 
                    r.Name 
                    ([for j in 0..d-1 -> 
                        sprintf "%s[%d]" p.Name (i+j)] 
                    |> String.concat ",")
        | S_StoreFloatV (p, i, r) -> 
            sprintf "%s%s;" ident ([for j in 0..r.Dimension-1 do
                                        yield sprintf "%s[%d]=get<%d>(%s)" p.Name (i+j) j r.Name ]
                                   |> String.concat ";")

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "void %s(%s) {\n%s\n}" 
            name 
            (List.map this.TranslateVariable parameters |> String.concat ", ") 
            (this.TranslateBody body)

end

type CppWithSIMDTranslator() = class 

    let mutable headerGenerated = false

    member private this.GenHeader() : string = 
        if headerGenerated then
            ""
        else
            headerGenerated <- true
            ""

    member private this.TranslateVariable (v : FloatPtrVariable) : string =
        match v with 
        | { Name=name; Readonly=true } -> sprintf "double const *%s" name
        | { Name=name; Readonly=false } -> sprintf "double *%s" name

    member private this.TranslateFloatVExpression (exp : FloatVExpression) : string = 
        match exp with
        | FV_Literal [v] -> sprintf "%.20f" v
        | FV_Literal [v0; v1] when v0 = v1 -> sprintf "_mm_set1_pd(%.20f)" v0
        | FV_Literal [v0; v1] -> sprintf "_mm_set_pd(%.20f, %.20f)" v1 v0
        | FV_Literal [v0; v1; v2; v3] when v0 = v1 && v0 = v2 && v0 = v3 -> sprintf "_mm256_set1_pd(%.20f)" v0
        | FV_Literal [v0; v1; v2; v3] -> sprintf "_mm256_set_pd(%.20f, %.20f, %.20f, %.20f)" v3 v2 v1 v0
        | FV_InterleavedLow ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }, 1) ->
            sprintf "_mm_unpacklo_pd(%s, %s)" name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 2) ->
            sprintf "_mm256_permute2f128_pd(%s, %s, 0x20)"  name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 1) ->
            sprintf "_mm256_permute4x64_pd(_mm256_unpacklo_pd(%s, %s),0xd8)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }, 1) ->
            sprintf "_mm_unpackhi_pd(%s, %s)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 2) ->
            sprintf "_mm256_permute2f128_pd(%s, %s, 0x31)"  name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 1) ->
            sprintf "_mm256_permute4x64_pd(_mm256_unpackhi_pd(%s, %s),0xd8)" name0 name1
        | FV_Minus { Name=name; Dimension=1 } -> sprintf "-%s" name
        | FV_Minus { Name=name; Dimension=2 } -> sprintf "_mm_sub_pd(_mm_setzero_pd(), %s)" name
        | FV_Minus { Name=name; Dimension=4 } -> sprintf "_mm256_sub_pd(_mm256_setzero_pd(), %s)" name
        | FV_Add ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s+%s" name0 name1
        | FV_Add ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_add_pd(%s, %s)" name0 name1
        | FV_Add ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm256_add_pd(%s, %s)" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s-%s" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_sub_pd(%s, %s)" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm256_sub_pd(%s, %s)" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s*%s" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_mul_pd(%s, %s)" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm256_mul_pd(%s, %s)" name0 name1
        | _ -> notImpl ""

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateFloatV (r, e) -> sprintf "%sauto %s=%s;" ident r.Name (this.TranslateFloatVExpression e)
        | S_LoadFloatV ({ Name=rname; Dimension=1 }, p, i, 1) -> 
            sprintf "%sauto %s=%s[%d];" ident rname p.Name i
        | S_LoadFloatV ({ Name=rname; Dimension=2 }, p, i, 2) -> 
            sprintf "%sauto %s=_mm_load_pd(&%s[%d]);" ident rname p.Name i
        | S_LoadFloatV ({ Name=rname; Dimension=4 }, p, i, 4) -> 
            sprintf "%sauto %s=_mm256_load_pd(&%s[%d]);" ident rname p.Name i
        | S_StoreFloatV (p, i, { Name=rname; Dimension=1 }) ->
            sprintf "%s%s[%d]=%s;" ident p.Name i rname
        | S_StoreFloatV (p, i, { Name=rname; Dimension=2 }) ->
            sprintf "%s_mm_store_pd(&%s[%d], %s);" ident p.Name i rname
        | S_StoreFloatV (p, i, { Name=rname; Dimension=4 }) ->
            sprintf "%s_mm256_store_pd(&%s[%d], %s);" ident p.Name i rname
        | _ -> notImpl ""

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "void %s(%s) {\n%s\n}" 
            name 
            (List.map this.TranslateVariable parameters |> String.concat ", ") 
            (this.TranslateBody body)

end

type CppWithSIMDTranslatorF() = class 

    let mutable headerGenerated = false

    member private this.GenHeader() : string = 
        if headerGenerated then
            ""
        else
            headerGenerated <- true
            "
static auto gF32Q2Q3SwapIdx = _mm256_set_epi32(7, 6, 3, 2, 5, 4, 1, 0);
"

    member private this.TranslateVariable (v : FloatPtrVariable) : string =
        match v with 
        | { Name=name; Readonly=true } -> sprintf "float const *%s" name
        | { Name=name; Readonly=false } -> sprintf "float *%s" name

    member private this.TranslateFloatVExpression (exp : FloatVExpression) : string = 
        match exp with
        | FV_Literal [v] -> sprintf "%.20ff" v
        | FV_Literal [v0; v1] when v0 = v1 -> sprintf "_mm_set1_ps(%.20ff)" v0
        | FV_Literal [v0; v1] -> sprintf "_mm_set_ps(0, 0, %.20ff, %.20ff)" v1 v0
        | FV_Literal [v0; v1; v2; v3] when v0 = v1 && v0 = v2 && v0 = v3 -> sprintf "_mm_set1_ps(%.20ff)" v0
        | FV_Literal [v0; v1; v2; v3] -> sprintf "_mm_set_ps(%.20ff, %.20ff, %.20ff, %.20ff)" v3 v2 v1 v0
        | FV_Literal [v0; v1; v2; v3; v4; v5; v6; v7] when v0 = v1 && v0 = v2 && v0 = v3 && v0 = v4 && v0 = v5 && v0 = v6 && v0 = v7 -> 
            sprintf "_mm256_set1_ps(%.20ff)" v0
        | FV_Literal [v0; v1; v2; v3; v4; v5; v6; v7] -> sprintf "_mm256_set_ps(%.20ff, %.20ff, %.20ff, %.20ff, %.20ff, %.20ff, %.20ff, %.20ff)" v7 v6 v5 v4 v3 v2 v1 v0
        | FV_InterleavedLow ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }, 1) ->
            sprintf "_mm_unpacklo_ps(%s, %s)" name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 2) ->
            sprintf "_mm_shuffle_ps(%s, %s, 0x44)" name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 1) ->
            sprintf "_mm_unpacklo_ps(_mm_unpacklo_ps(%s, %s),_mm_unpackhi_ps(%s, %s))" name0 name1 name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 4) ->
            sprintf "_mm256_permute2f128_ps(%s, %s, 0x20)"  name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 2) ->
            sprintf "_mm256_permutevar8x32_ps(_mm256_shuffle_ps(%s, %s, 0x44), gF32Q2Q3SwapIdx)" name0 name1
        | FV_InterleavedLow ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 1) ->
            sprintf "_mm256_permutevar8x32_ps(_mm256_shuffle_ps(%s, %s, 0x88), gF32Q2Q3SwapIdx)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }, 1) ->
            sprintf "_mm_permute_ps(_mm_unpacklo_ps(%s, %s), 0xe)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 2) ->
            sprintf "_mm_shuffle_ps(%s, %s, 0xee)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }, 1) ->
            sprintf "_mm_unpackhi_ps(_mm_unpacklo_ps(%s, %s),_mm_unpackhi_ps(%s, %s))" name0 name1 name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 4) ->
            sprintf "_mm256_permute2f128_ps(%s, %s, 0x31)"  name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 2) ->
            sprintf "_mm256_permutevar8x32_ps(_mm256_shuffle_ps(%s, %s, 0xee), gF32Q2Q3SwapIdx)" name0 name1
        | FV_InterleavedHi ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }, 1) ->
            sprintf "_mm256_permutevar8x32_ps(_mm256_shuffle_ps(%s, %s, 0xdd), gF32Q2Q3SwapIdx)" name0 name1
        | FV_Minus { Name=name; Dimension=1 } -> sprintf "-%s" name
        | FV_Minus { Name=name; Dimension=2 } -> sprintf "_mm_sub_ps(_mm_setzero_ps(), %s)" name
        | FV_Minus { Name=name; Dimension=4 } -> sprintf "_mm_sub_ps(_mm_setzero_ps(), %s)" name
        | FV_Minus { Name=name; Dimension=8 } -> sprintf "_mm256_sub_ps(_mm256_setzero_ps(), %s)" name
        | FV_Add ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s+%s" name0 name1
        | FV_Add ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_add_ps(%s, %s)" name0 name1
        | FV_Add ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm_add_ps(%s, %s)" name0 name1
        | FV_Add ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }) -> sprintf "_mm256_add_ps(%s, %s)" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s-%s" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_sub_ps(%s, %s)" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm_sub_ps(%s, %s)" name0 name1
        | FV_Subtract ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }) -> sprintf "_mm256_sub_ps(%s, %s)" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "%s*%s" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_mul_ps(%s, %s)" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm_mul_ps(%s, %s)" name0 name1
        | FV_Multiply ({ Name=name0; Dimension=8 }, { Name=name1; Dimension=8 }) -> sprintf "_mm256_mul_ps(%s, %s)" name0 name1
        | _ -> notImpl ""

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateFloatV (r, e) -> sprintf "%sauto %s=%s;" ident r.Name (this.TranslateFloatVExpression e)
        | S_LoadFloatV ({ Name=rname; Dimension=1 }, p, i, 1) -> 
            sprintf "%sauto %s=%s[%d];" ident rname p.Name i
        | S_LoadFloatV ({ Name=rname; Dimension=2 }, p, i, 2) -> 
            sprintf "%sauto %s=_mm_loadl_pi(_mm_setzero_ps(), reinterpret_cast<__m64 const*>(&%s[%d]));" ident rname p.Name i
        | S_LoadFloatV ({ Name=rname; Dimension=4 }, p, i, 4) -> 
            sprintf "%sauto %s=_mm_load_ps(&%s[%d]);" ident rname p.Name i
        | S_LoadFloatV ({ Name=rname; Dimension=8 }, p, i, 8) -> 
            sprintf "%sauto %s=_mm256_load_ps(&%s[%d]);" ident rname p.Name i
        | S_StoreFloatV (p, i, { Name=rname; Dimension=1 }) ->
            sprintf "%s%s[%d]=%s;" ident p.Name i rname
        | S_StoreFloatV (p, i, { Name=rname; Dimension=2 }) ->
            sprintf "%s_mm_storel_pi(reinterpret_cast<__m64*>(&%s[%d]), %s);" ident p.Name i rname
        | S_StoreFloatV (p, i, { Name=rname; Dimension=4 }) ->
            sprintf "%s_mm_store_ps(&%s[%d], %s);" ident p.Name i rname
        | S_StoreFloatV (p, i, { Name=rname; Dimension=8 }) ->
            sprintf "%s_mm256_store_ps(&%s[%d], %s);" ident p.Name i rname
        | _ -> notImpl ""

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "void %s(%s) {\n%s\n}" 
            name 
            (List.map this.TranslateVariable parameters |> String.concat ", ") 
            (this.TranslateBody body)

end

//-------------------------------------------------------------------------------
let GenerateFFTCode (path : string) (bitsList : List<int>) (dlp : int) (translator : Function -> string) =
    let fftGen = new Radix2FFTFunctionGenerator()
    System.IO.File.WriteAllLines(path,
        List.concat [
                [for bits in bitsList do 
                    yield fftGen.GenFFT bits dlp 1
                    yield fftGen.GenFFT bits dlp -1];
                [for bits in (List.skip (bitsList.Length - 1) bitsList) do 
                    yield fftGen.GenInplaceFFT bits dlp 1
                    yield fftGen.GenInplaceFFT bits dlp -1] ] 
            |> List.map translator);

//-------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =

    let maxBits = 8
    let outputDir = "..\..\..\ScanFFT\include\ScanFFTImpl\Generated"

    GenerateFFTCode 
        (outputDir + "\UnrolledFFT_Double.h")
        [ 0..maxBits ]
        8
        ((new CppTranslator("double")).Translate);

    GenerateFFTCode 
        (outputDir + "\UnrolledFFT_Single.h")
        [ 0..maxBits ]
        8
        ((new CppTranslator("float")).Translate);

    GenerateFFTCode 
        (outputDir + "\UnrolledFFT_DoubleSIMD.h")
        [ 0..maxBits ]
        4
        ((new CppWithSIMDTranslator()).Translate);

    GenerateFFTCode 
        (outputDir + "\UnrolledFFT_SingleSIMD.h")
        [ 0..maxBits ]
        8
        ((new CppWithSIMDTranslatorF()).Translate);

    0
