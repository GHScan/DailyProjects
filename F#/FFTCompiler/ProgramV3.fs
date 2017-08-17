module Program


open System

//-------------------------------------------------------------------------------
let notImpl (message : string) : 'T = 
    raise (NotImplementedException message)

//-------------------------------------------------------------------------------
type IntVariable = { Name: string }
type Complex = System.Numerics.Complex
type ComplexPtrVariable = { Name : string; Readonly : bool }
type Variable = 
    | V_Int of Variable : IntVariable
    | V_Ptr of Variable : ComplexPtrVariable
type ComplexVRegister = { Name : string; Dimension : int }

type IntExpression = 
    | I_Literal of Value : int
    | I_Variable of Variable : IntVariable
    | I_Add of Operand0 : IntExpression * Operand1 : IntExpression
    | I_Substract of Operand0 : IntExpression * Operand1 : IntExpression
    | I_Multiply of Operand0 : IntExpression * Operand1 : IntExpression
    | I_And of Operand0 : IntExpression * Operand1 : IntExpression
    | I_Or of Operand0 : IntExpression * Operand1 : IntExpression
    | I_LShift of Operand0 : IntExpression * Operand1 : IntExpression
    | I_RShift of Operand0 : IntExpression * Operand1 : IntExpression
    | I_Invoke of FuncName : string * Arguments : List<ArgumentExpression>
and FloatExpression = 
    | F_Literal of Value : double
    | F_Minus of Operand : FloatExpression
    | F_Multiply of Operand0 : FloatExpression * Operand1 : FloatExpression
    | F_ReadReal of Operand : ComplexVRegister * Index : int
    | F_ReadImag of Operand : ComplexVRegister * Index : int
and ComplexVExpression = 
    | CV_New of Values : List<FloatExpression * FloatExpression>
    | CV_DupReals of Operand : ComplexVRegister
    | CV_DupImags of Operand : ComplexVRegister
    | CV_Merge of Variables : List<ComplexVRegister>
    | CV_Add of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_Substract of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_ElementMultiply of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_CrossElementAddSub of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_Invoke of FuncName : string * Arguments : List<ArgumentExpression> * Dimension : int
and ArgumentExpression = 
    | A_Int of Exprssion : IntExpression
    | A_Ptr of Expression : ComplexPtrVariable

type Statement = 
    | S_EvaluateComplexV of Register : ComplexVRegister * Expression : ComplexVExpression
    | S_LoadComplex of Register : ComplexVRegister * Ptr : ComplexPtrVariable * Index : IntExpression * Dimension : int
    | S_StoreComplexV of Ptrs : List<ComplexPtrVariable * IntExpression * int> * Register : ComplexVRegister
    | S_RangeFor of Variable : IntVariable * Start : int * Stop : int * Step : int * Body : List<Statement>
    | S_Invoke of Name : string * Arguments : List<ArgumentExpression>
    | S_Return of Expression : IntExpression

type Function = { Name : string; Parameters : List<Variable>; Ret : Option<IntVariable>; Body : List<Statement> }


let DimensionOf (exp : ComplexVExpression) : int = 
    match exp with
        | CV_New nums -> nums.Length
        | CV_DupReals op -> op.Dimension
        | CV_DupImags op -> op.Dimension
        | CV_Merge rs -> List.sumBy (fun r -> r.Dimension) rs
        | CV_Add (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_Substract (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_ElementMultiply (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_CrossElementAddSub (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_Invoke (name, args, d) -> d


let ReduceBinaryIOp 
    (termReducer : IntExpression * IntExpression -> IntExpression) 
    (literalReducer : int -> int -> int)
    (unit : int)
    (operands : List<IntExpression>) 
    : IntExpression =
    let terms, accum = List.foldBack 
                            (fun v (terms, accum) -> match v with 
                                                        | I_Literal i -> (terms, literalReducer i accum)
                                                        | _ -> (v :: terms, accum)) 
                            operands 
                            ([], unit)
    if terms.Length = 0 then
        I_Literal accum
    else
        let reducedTerms = List.reduce (fun a b->termReducer(a, b)) terms
        if accum = unit then
            reducedTerms
        else 
            termReducer (reducedTerms, I_Literal accum)

let rec ExpandIAdd (exp : IntExpression) : List<IntExpression> = 
    match exp with
    | I_Add (op0, op1) -> List.concat [ExpandIAdd op0; ExpandIAdd op1]
    | _ -> [ exp ]

let rec ExpandIMultiply (exp : IntExpression) : List<IntExpression> = 
    match exp with
    | I_Multiply (op0, op1) -> List.concat [ExpandIMultiply op0; ExpandIMultiply op1]
    | _ -> [ exp ]

let rec FoldLiteralInt (exp : IntExpression) : IntExpression = 
    match exp with
    | I_Literal _ -> exp
    | I_Variable _ -> exp
    | I_Add (op0, op1) -> ExpandIAdd exp |> List.map FoldLiteralInt |> ReduceBinaryIOp I_Add (fun a b->a+b) 0
    | I_Substract (op0, op1) -> match (FoldLiteralInt op0, FoldLiteralInt op1) with 
                                | (I_Literal a, I_Literal b) -> I_Literal (a-b)
                                | (a, b) -> I_Substract (a, b)
    | I_Multiply (op0, op1) -> ExpandIMultiply exp |> List.map FoldLiteralInt |> ReduceBinaryIOp I_Multiply (fun a b->a*b) 1
    | _ -> notImpl ""

//-------------------------------------------------------------------------------
module Util = 
    let ILog2 (x : int) : int = 
        int (Math.Log((double x), 2.0))

    let ReverseBits (index : int) (bitCount : int) : int = 
        let mutable x = uint32 index
        x <- (((x &&& 0xaaaaaaaau) >>> 1) ||| ((x &&& 0x55555555u) <<< 1))
        x <- (((x &&& 0xccccccccu) >>> 2) ||| ((x &&& 0x33333333u) <<< 2))
        x <- (((x &&& 0xf0f0f0f0u) >>> 4) ||| ((x &&& 0x0f0f0f0fu) <<< 4))
        x <- (((x &&& 0xff00ff00u) >>> 8) ||| ((x &&& 0x00ff00ffu) <<< 8))
        int ((((x >>> 16) ||| (x <<< 16))) >>> (32 - bitCount))

    let ReversedBytes = [for i in 0..255 -> ReverseBits i 8] 

    let FloatEquals (f0 : double) (f1 : double) : bool =
        Math.Abs(f0 - f1) < 0.000001

    let ComplexEquals (c0 : Complex) (c1 : Complex) : bool = 
        FloatEquals c0.Real c1.Real && FloatEquals c0.Imaginary c1.Imaginary

    let TwiddleFactor (e : int) (n : int) : Complex = 
        Complex.FromPolarCoordinates(1.0, 2.0 * Math.PI * (double e) / (double n))

//-------------------------------------------------------------------------------
module Constant =
    let kBitReverseCopyFuncPrefix = "BitReverseCopy_"
    let kInverseBitReverseCopyFuncPrefix = "IBitReverseCopy_"
    let kFFTFuncPrefix = "FFT_"
    let kInverseFFTFuncPrefix = "IFFT_"
    let kKernelFuncPrefix = "Kernel_"
    let kInverseKernelFuncPrefix = "IKernel_"
    let kTwiddleFactorFuncName = "TwiddleFactor"
    let kReverseBitsFuncPrefix = "ReverseBits_"
    let kReverseByteFuncName = "ReverseByte"
    let kSrcParamName = "src"
    let kDestParamName = "dest"
    let kOffsetParamName = "off"
    let kDefaultParamName = "param"
    let kScalerVariableName = "scaler"
    let kDefaultLoopVariableName = "i"
    let kComplexVRegisterPrefix = "temp_"
    let kUnrollingBits = 8
    let kUnrollingSize = 1 <<< kUnrollingBits
    let kRollingBits = 3
    let kRollingSize = 1 <<< kRollingBits

type BitReverseCopyFunctionGenerator() = class
    
    member private this.GenUnrolledForwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let statements = [0..size-1] |> List.chunkBySize 4 |> List.collect (fun ints -> 
                let temps = [for i in ints ->{ Name=Constant.kComplexVRegisterPrefix+i.ToString(); Dimension=1 }]
                List.concat [
                    [for i, temp in List.zip ints temps -> S_LoadComplex (temp, srcParam, I_Literal i, 1)];
                    [for i, temp in List.zip ints temps -> S_StoreComplexV ([destParam, I_Literal (Util.ReverseBits i (Util.ILog2 size)), 1], temp)]])
        {   Name=Constant.kBitReverseCopyFuncPrefix+size.ToString();
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None;
            Body=statements }

    member private this.GenUnrolledBackwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let scalerVar = { Name=Constant.kScalerVariableName; Dimension=1 }
        let defScaler = S_EvaluateComplexV (scalerVar, CV_New [F_Literal (1.0/(double size)), F_Literal (1.0/(double size))])
        let bitRevCopy = [0..size-1] |> List.chunkBySize 4 |> List.collect (fun ints -> 
                let temps = [for i in ints ->{ Name=Constant.kComplexVRegisterPrefix+i.ToString(); Dimension=1 }]
                let scaledTemps = [for i in ints ->{ Name=Constant.kComplexVRegisterPrefix+(i+size).ToString(); Dimension=1 }]
                List.concat [
                    [for i, temp in List.zip ints temps -> S_LoadComplex (temp, srcParam, I_Literal i, 1)];
                    [for temp, scaledTemp in List.zip temps scaledTemps -> S_EvaluateComplexV (scaledTemp, CV_ElementMultiply (temp, scalerVar))]
                    [for i, scaledTemp in List.zip ints scaledTemps -> S_StoreComplexV ([destParam, I_Literal (Util.ReverseBits i (Util.ILog2 size)), 1], scaledTemp)]])
        {   Name=Constant.kInverseBitReverseCopyFuncPrefix+size.ToString();
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None;
            Body=defScaler :: bitRevCopy }

    member private this.GenReverseBitsFunction (size : int) : Function = 
        let param : IntVariable = { Name=Constant.kDefaultParamName }
        let bits = Util.ILog2 size
        let reversed = 
            [for i in 0..8..bits-1 do
                let b = I_And ((I_RShift (I_Variable param, I_Literal i)), I_Literal 0xff)
                let reversed = I_Invoke (Constant.kReverseByteFuncName, [A_Int b])
                if i + 8 > bits then
                    yield I_RShift (reversed, I_Literal (i+8-bits))
                else
                    yield I_LShift (reversed, I_Literal (bits-i-8))]
            |> List.reduce (fun a b ->I_Or (a,b))
        {   Name=Constant.kReverseBitsFuncPrefix+size.ToString(); 
            Parameters=[V_Int param]; Ret=Some param; 
            Body=[S_Return reversed]}

    member private this.GenRolledForwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let loopV : IntVariable = { Name=Constant.kDefaultLoopVariableName }
        let temp = { Name=Constant.kComplexVRegisterPrefix+"0"; Dimension=1 }
        let load = S_LoadComplex (temp, srcParam, I_Variable loopV, 1)
        let store = S_StoreComplexV (
                        [destParam, (I_Invoke (Constant.kReverseBitsFuncPrefix+size.ToString(), [A_Int (I_Variable loopV)])), 1], 
                        temp)
        let stat = S_RangeFor (loopV, 0, size, 1, [load; store])
        {   Name=Constant.kBitReverseCopyFuncPrefix+size.ToString(); 
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None; 
            Body=[stat] }

    member private this.GenRolledBackwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let loopV : IntVariable = { Name=Constant.kDefaultLoopVariableName }
        let scalerVar = { Name=Constant.kScalerVariableName; Dimension=1 }
        let temp = { Name=Constant.kComplexVRegisterPrefix+"0"; Dimension=1 }
        let scaledTemp = { Name=Constant.kComplexVRegisterPrefix+"1"; Dimension=1 }
        let defScaler = S_EvaluateComplexV (scalerVar, CV_New [F_Literal (1.0/(double size)), F_Literal (1.0/(double size))])
        let load = S_LoadComplex (temp, srcParam, I_Variable loopV, 1)
        let scale = S_EvaluateComplexV (scaledTemp, CV_ElementMultiply (temp, scalerVar))
        let store = S_StoreComplexV (
                        [destParam, (I_Invoke (Constant.kReverseBitsFuncPrefix+size.ToString(), [A_Int (I_Variable loopV)])), 1], 
                        scaledTemp)
        let stat = S_RangeFor (loopV, 0, size, 1, [load; scale; store])
        {   Name=Constant.kInverseBitReverseCopyFuncPrefix+size.ToString(); 
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None; 
            Body=[defScaler; stat] }
    
    member this.ForwardGen (size : int) : List<Function> = 
        if size <= Constant.kUnrollingSize then
            [ this.GenUnrolledForwardFunction size ]
        else
            [ this.GenReverseBitsFunction size; this.GenRolledForwardFunction size ] 

    member this.BackwardGen (size : int) : List<Function> = 
        if size <= Constant.kUnrollingSize then
            [ this.GenUnrolledBackwardFunction size ]
        else
            [ this.GenRolledBackwardFunction size ] 

end

[< AbstractClass >]
type FFTKernelGenerator() = class
    let mutable statements : List<Statement> = List.empty
    let mutable exp2Register : Map<ComplexVExpression, ComplexVRegister> = Map.empty
    let mutable registerId = 0

    member private this.Reset() =
        statements <- List.empty
        exp2Register <- Map.empty
        registerId <- 0

    member private this.NewRegisterName () : string = 
        registerId <- registerId + 1
        Constant.kComplexVRegisterPrefix + registerId.ToString()

    member private this.EvaluateComplexV (exp : ComplexVExpression) : ComplexVRegister = 
        let reg = exp2Register |> Map.tryFind exp
        if Option.isSome reg then
            Option.get reg
        else
            let reg : ComplexVRegister = { Name = this.NewRegisterName(); Dimension = DimensionOf exp }
            exp2Register <- exp2Register.Add(exp, reg)
            statements <- S_EvaluateComplexV (reg, exp) :: statements
            reg

    member this.StoreComplexV (ptrs : List<ComplexPtrVariable * IntExpression * int>) (r : ComplexVRegister) =
        assert (List.forall (fun (p, i, d)-> not p.Readonly) ptrs)
        statements <- (S_StoreComplexV (ptrs, r)) :: statements

    member this.LoadComplex (ptr : ComplexPtrVariable) (index : IntExpression) (dimension : int) : ComplexVRegister =
        let reg : ComplexVRegister = { Name = this.NewRegisterName(); Dimension = dimension }
        statements <- (S_LoadComplex (reg, ptr, index, dimension)) :: statements
        reg

    member private this.MultiplyLiteralComplex (r : ComplexVRegister) (c : Complex) : ComplexVRegister = 
        assert (r.Dimension = 1)
        if Util.ComplexEquals c Complex.One then
            r
        else if Util.ComplexEquals -c Complex.One then
            let zero = this.EvaluateComplexV (CV_New [(F_Literal 0.0, F_Literal 0.0)])
            this.EvaluateComplexV (CV_Substract (zero, r))
        else if Util.ComplexEquals c Complex.ImaginaryOne then
            this.EvaluateComplexV (CV_New [(F_Minus (F_ReadImag (r, 0)), F_ReadReal (r, 0))])
        else if Util.ComplexEquals -c Complex.ImaginaryOne then
            this.EvaluateComplexV (CV_New [(F_ReadImag (r, 0), F_Minus (F_ReadReal (r, 0)))])
        else if Util.FloatEquals c.Imaginary 0.0 then
            let scaler = this.EvaluateComplexV (CV_New [(F_Literal c.Real, F_Literal c.Real)])
            this.EvaluateComplexV (CV_ElementMultiply (r, scaler))
        else if Util.FloatEquals c.Real 0.0 then
            let scaler = this.EvaluateComplexV (CV_New [(F_Literal c.Imaginary, F_Literal c.Imaginary)])
            let scaled = this.EvaluateComplexV (CV_ElementMultiply (r, scaler))
            this.EvaluateComplexV (CV_New [(F_Minus (F_ReadImag (scaled, 0)), F_ReadReal (scaled, 0))])
        else
            this.MultiplyLiteralComplexV r [c]

    member private this.MultiplyLiteralComplexV (r : ComplexVRegister) (cs : List<Complex>) : ComplexVRegister = 
        let reals = this.EvaluateComplexV (CV_New [for c in cs -> (F_Literal c.Real, F_Literal c.Real)])
        let imags = this.EvaluateComplexV (CV_New [for c in cs -> (F_Literal c.Imaginary, F_Literal c.Imaginary)])
        let rTimesReal = this.EvaluateComplexV (CV_ElementMultiply (r, reals))
        let rTimesImags = this.EvaluateComplexV (CV_ElementMultiply (r, imags))
        this.EvaluateComplexV (CV_CrossElementAddSub (rTimesReal, rTimesImags))

    member private this.MultiplyComplexV (r : ComplexVRegister) (cs : List<ComplexVRegister>) : ComplexVRegister = 
        assert (List.forall (fun c->c.Dimension = 1) cs)
        let csr = if cs.Length = 1 then cs.Item 0 else this.EvaluateComplexV (CV_Merge cs)
        let reals = this.EvaluateComplexV (CV_DupReals csr)
        let imags = this.EvaluateComplexV (CV_DupImags csr)
        let rTimesReal = this.EvaluateComplexV (CV_ElementMultiply (r, reals))
        let rTimesImags = this.EvaluateComplexV (CV_ElementMultiply (r, imags))
        this.EvaluateComplexV (CV_CrossElementAddSub (rTimesReal, rTimesImags))

    member this.Multiply (r : ComplexVRegister) (cs : List<IntExpression * int>) (direction : int) : ComplexVRegister = 
        assert (r.Dimension = cs.Length)
        let folded = [for e, n in cs -> (FoldLiteralInt e, n)]
        if List.forall (function |(I_Literal _,n) -> true|_ -> false) folded then
            let literals = List.map (function |(I_Literal e),n -> Util.TwiddleFactor (e * direction) n|_ -> Complex.Zero) folded
            if literals.Length = 1 then
                this.MultiplyLiteralComplex r (literals.Item 0)
            else
                this.MultiplyLiteralComplexV r literals
        else
            let crs = [for e, n in folded do
                        let e2 = FoldLiteralInt (if direction > 0 then e else I_Substract (I_Literal n, e))
                        let bitCount = Util.ILog2 n
                        yield this.EvaluateComplexV (CV_Invoke (Constant.kTwiddleFactorFuncName, [A_Int e2; A_Int (I_Literal bitCount)], 1))]
            this.MultiplyComplexV r crs

    member this.Add (op0 : ComplexVRegister) (op1 : ComplexVRegister) : ComplexVRegister =
        if op0.Name < op1.Name then
            this.EvaluateComplexV (CV_Add (op0, op1))
        else
            this.Add op1 op0

    member this.Substract (op0 : ComplexVRegister) (op1 : ComplexVRegister) : ComplexVRegister = 
        this.EvaluateComplexV (CV_Substract (op0, op1))

    member this.MergeRegisters (dlp : int) (l : List<ComplexVRegister>) : List<ComplexVRegister> =
        let chunk = dlp / (l.Item 0).Dimension
        l |> List.chunkBySize chunk 
          |> List.collect (fun rs -> if rs.Length > 1 then [this.EvaluateComplexV (CV_Merge rs)] else rs)
    
    member this.Gen (a : List<ComplexPtrVariable * IntExpression * int>) (initialFactor : List<IntExpression * int>) (dlp : int) (direction : int) : List<Statement> = 
        this.Reset()

        let rs = this.RecursiveFFT a initialFactor dlp direction
        for ptrs, r in List.zip (List.chunkBySize (a.Length / rs.Length) a) rs do
            this.StoreComplexV ptrs r

        List.rev statements

    abstract member RecursiveFFT : List<ComplexPtrVariable * IntExpression * int> -> List<IntExpression * int> -> int -> int -> List<ComplexVRegister>
end

type Radix2FFTKernelGenerator() = class
    inherit FFTKernelGenerator()

    override this.RecursiveFFT 
            (a : List<ComplexPtrVariable * IntExpression * int>) (initialFactors : List<IntExpression * int>) (dlp : int) (direction : int) : List<ComplexVRegister> =

        if a.Length = 1 then
            let (p, i, d) = a.Item 0
            [ this.LoadComplex p i d]
        else
            let a0, a1 = List.splitAt (a.Length / 2) a
            let nextInitialFactors = [for e, n in initialFactors -> (I_Multiply (e, I_Literal 2), n)]

            let b0 = this.RecursiveFFT a0 nextInitialFactors dlp direction |> this.MergeRegisters dlp
            let b1 = this.RecursiveFFT a1 nextInitialFactors dlp direction |> this.MergeRegisters dlp

            let factors = [for i in 0..a0.Length-1 ->
                            [for e, n in initialFactors -> (I_Add (e, I_Literal (n / a.Length * i)), n)]]
                            |> List.chunkBySize (a0.Length / b0.Length)
                            |> List.map List.concat

            let mutable o0, o1 = List.empty, List.empty

            for c0, c1, f in List.zip3 b0 b1 factors do
                let c1f = this.Multiply c1 f direction
                o0 <- (this.Add c0 c1f) :: o0
                o1 <- (this.Substract c0 c1f) :: o1

            List.concat [List.rev o0; List.rev o1]
end

type FFTFunctionGenerator() = class

    member private this.InvokeKernel (size : int) (destParam : ComplexPtrVariable) (offset : IntExpression) (direction : int) : List<Statement> = 
        if size = 1 then
            List.empty
        else
            let prefix = if direction > 0 then Constant.kKernelFuncPrefix else Constant.kInverseKernelFuncPrefix
            [ S_Invoke (prefix + size.ToString(), [A_Ptr destParam; A_Int offset]) ]

    member private this.RangeFor (start : int) (stop : int) (step : int) (bodyGen : IntExpression -> List<Statement>) : List<Statement> =
        if start + 1 > stop then
            List.empty
        else if start + 1 = stop then
            bodyGen (I_Literal start)
        else
            let var : IntVariable = { Name=Constant.kDefaultLoopVariableName }
            [ S_RangeFor (var, start, stop, step, bodyGen (I_Variable var)) ]

    member private this.GenKernelFunction (size : int) (dlp : int) (direction : int) : Function = 
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let offsetParam : IntVariable = { Name=Constant.kOffsetParamName }
        let prevKernelSize = 
            if size <= Constant.kUnrollingSize then
                1
            else 
                size / Constant.kRollingSize
        let prevKernelCount = size / prevKernelSize
        let invokeKernels = List.concat [for i in 0..prevKernelCount-1 ->
                                            this.InvokeKernel 
                                                prevKernelSize 
                                                destParam 
                                                (I_Add (I_Variable offsetParam, I_Literal (i * prevKernelSize))) 
                                                direction ]
        let step = min dlp prevKernelSize
        let loop = this.RangeFor 0 prevKernelSize step (fun iv -> 
                let recFFTGen = new Radix2FFTKernelGenerator()
                let a = [for i in 0..prevKernelCount-1 -> 
                            (destParam, FoldLiteralInt (I_Add (I_Variable offsetParam,(I_Add (iv, I_Literal (i * prevKernelSize))))), step)]
                let initialFactors = [for i in 0..step-1 -> (I_Add (iv, I_Literal i), size)]
                recFFTGen.Gen a initialFactors dlp direction)
        {   Name=(if direction > 0 then Constant.kKernelFuncPrefix else Constant.kInverseKernelFuncPrefix)+size.ToString();
            Parameters=[V_Ptr destParam; V_Int offsetParam ]; Ret=None;
            Body=List.concat [invokeKernels; loop] }

    member private this.GenFFTFunction (size : int) (direction : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        if direction > 0 then
            let invokeBitReverseCopy = S_Invoke (Constant.kBitReverseCopyFuncPrefix+size.ToString(), [A_Ptr destParam; A_Ptr srcParam])
            let invokeKernel = S_Invoke (Constant.kKernelFuncPrefix+size.ToString(), [A_Ptr destParam; A_Int (I_Literal 0)])
            {   Name=Constant.kFFTFuncPrefix+size.ToString();
                Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None;
                Body=[invokeBitReverseCopy; invokeKernel] }
        else
            let invokeBitReverseCopy = S_Invoke (Constant.kInverseBitReverseCopyFuncPrefix+size.ToString(), [A_Ptr destParam; A_Ptr srcParam])
            let invokeKernel = S_Invoke (Constant.kInverseKernelFuncPrefix+size.ToString(), [A_Ptr destParam; A_Int (I_Literal 0)])
            {   Name=Constant.kInverseFFTFuncPrefix+size.ToString();
                Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None;
                Body=[invokeBitReverseCopy; invokeKernel] }

    member this.ForwardGen (size : int) (dlp : int) : List<Function> = 
        assert (dlp = 1 || dlp = 2 || dlp = 4)
        [ this.GenKernelFunction size  dlp 1; this.GenFFTFunction size 1]

    member this.BackwardGen (size : int) (dlp : int) : List<Function> = 
        assert (dlp = 1 || dlp = 2 || dlp = 4)
        [ this.GenKernelFunction size  dlp -1; this.GenFFTFunction size -1]

end

//-------------------------------------------------------------------------------
type PythonTranslator() = class 
    let mutable headerGenerated = false

    member private this.GenHeader() : string =
        if headerGenerated then 
            ""
        else
            headerGenerated <- true
            let twiddleFactorFunc = "
import math
def TwiddleFactor(i, bitCount, factorMatrix=[[]]*32):
    factors = factorMatrix[bitCount]
    if len(factors) == 0:
        size = 1 << bitCount
        factors = [0] * (size + 1)
        factorMatrix[bitCount] = factors
        fi, f = 1, math.e**(2j*math.pi/size);
        for j in range(size):
            factors[j], fi = fi, fi * f
        factors[size] = 1
    return factors[i]
                                "
            let reverseByteFunc = 
                sprintf "def %s(i,revBytes=[%s]):\n    return revBytes[i];\n" 
                    Constant.kReverseByteFuncName ([for b in Util.ReversedBytes -> b.ToString()] |> String.concat ",")
            [twiddleFactorFunc; reverseByteFunc] |> String.concat "\n"

    member private this.TranslateComplexVRegister (r : ComplexVRegister) : List<string> = 
        [for i in 0..r.Dimension-1 -> sprintf "%s_%d" r.Name i]

    member private this.TranslateComplexPtr (p : ComplexPtrVariable) (i : IntExpression) (dimension : int) : List<string> = 
        let is = this.TranslateIntExpression i
        [for i in 0..dimension-1 -> sprintf "%s[%s+%d]" p.Name is i]

    member private this.TranslateVariable (v : Variable) : string =
        match v with 
        | V_Int { Name=name; } -> name
        | V_Ptr { Name=name; } -> name

    member private this.TranslateIntExpression (exp : IntExpression) : string = 
        match exp with
        | I_Literal i -> i.ToString()
        | I_Variable {Name=name} -> name
        | I_Add (op0, op1) -> sprintf "(%s)+(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Substract (op0, op1) -> sprintf "(%s)-(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Multiply (op0, op1) -> sprintf "(%s)*(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_And (op0, op1) -> sprintf "(%s)&(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Or (op0, op1) -> sprintf "(%s)|(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_LShift (op0, op1) -> sprintf "(%s)<<(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_RShift (op0, op1) -> sprintf "(%s)>>(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Invoke (name, args) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")

    member private this.TranslateFloatExpression (exp : FloatExpression) : string = 
        match exp with
        | F_Literal v -> sprintf "%.20f" v
        | F_Minus v -> sprintf "-%s" (this.TranslateFloatExpression v)
        | F_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateFloatExpression op0) (this.TranslateFloatExpression op1)
        | F_ReadReal (r, i) -> sprintf "%s.real" ((this.TranslateComplexVRegister r).Item i)
        | F_ReadImag (r, i) -> sprintf "%s.imag" ((this.TranslateComplexVRegister r).Item i)

    member private this.TranslateComplexVExpression (exp : ComplexVExpression) : List<string> = 
        match exp with
        | CV_New values -> [for (r, i) in values -> sprintf "complex(%s,%s)" (this.TranslateFloatExpression r) (this.TranslateFloatExpression i)]
        | CV_DupReals r -> this.TranslateComplexVRegister r |> List.map (fun s -> sprintf "complex(%s.real,%s.real)" s s)
        | CV_DupImags r -> this.TranslateComplexVRegister r |> List.map (fun s -> sprintf "complex(%s.imag,%s.imag)" s s)
        | CV_Merge vars -> List.collect this.TranslateComplexVRegister vars
        | CV_Add (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s+%s" l r]
        | CV_Substract (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s-%s" l r]
        | CV_ElementMultiply (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "complex(%s.real*%s.real,%s.imag*%s.imag)" l r l r]
        | CV_CrossElementAddSub (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "complex(%s.real-%s.imag,%s.imag+%s.real)" l r l r]
        | CV_Invoke (name, args, d) -> [sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")]

    member private this.TranslateArgument (arg : ArgumentExpression) : string = 
        match arg with
        | A_Int i -> this.TranslateIntExpression i
        | A_Ptr {Name=name} -> name

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateComplexV (r, e) -> 
            sprintf "%s%s" ident 
                (List.zip (this.TranslateComplexVRegister r) (this.TranslateComplexVExpression e) 
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_LoadComplex (r, p, i, d) -> 
            sprintf "%s%s" ident 
                (List.zip (this.TranslateComplexVRegister r) (this.TranslateComplexPtr p i d) 
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_StoreComplexV (ptrs, r) -> 
            sprintf "%s%s" ident 
                (List.zip (List.concat [for p, i, d in ptrs -> this.TranslateComplexPtr p i d]) (this.TranslateComplexVRegister r)
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_RangeFor ({Name=name}, start, stop, step, stats) -> 
            sprintf "%sfor %s in range(%d,%d,%d):\n%s" 
                ident name start stop step 
                ([for stat in stats -> this.TranslateStatement (ident + "    ") stat] |> String.concat "\n")
        | S_Invoke (name, args) -> sprintf "%s%s(%s)" ident name ([for arg in args -> this.TranslateArgument arg] |> String.concat ",")
        | S_Return e -> sprintf "%sreturn %s" ident (this.TranslateIntExpression e)

    member private this.TranslateBody (body : List<Statement>) : string = 
        if body.Length = 0 then
            "    pass"
        else 
            body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Ret=ret; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "def %s(%s):\n%s\n" name 
            (List.map this.TranslateVariable parameters |> String.concat ", ") 
            (this.TranslateBody body)

end

type CppTranslator(floatType : string) = class 
    do 
        assert (floatType = "float" || floatType = "double")

    let mutable headerGenerated = false

    member private this.GenHeader() : string =
        if headerGenerated then 
            ""
        else
            headerGenerated <- true
            let twiddleFactorFunc = 
                sprintf "
#include <vector>
#include <complex>
static std::complex<%s> %s(size_t i, size_t bitCount) {
    static std::vector<std::vector<std::complex<%s>>> sFactorMatrix(32);
    auto &factors = sFactorMatrix[bitCount];
    if (factors.size() == 0) {
        auto size = 1UL << bitCount;
        factors.reserve(size + 1);
        std::complex<%s> f = std::polar(1.0, 2.0 * acos(-1) / size), fi = 1;
        for (size_t j = 0; j < size; ++j, fi *= f) 
            factors.push_back(fi);
        factors.push_back(1);
    }
    return factors[i];
}
                                " floatType Constant.kTwiddleFactorFuncName floatType floatType
            let reverseByteFunc = 
                sprintf "static size_t %s(size_t i){\n    static size_t sReversedBytes[]={%s};\n    return sReversedBytes[i];\n}\n" 
                    Constant.kReverseByteFuncName ([for b in Util.ReversedBytes -> b.ToString()] |> String.concat ",")
            [twiddleFactorFunc; reverseByteFunc] |> String.concat "\n"

    member private this.TranslateComplexVRegister (r : ComplexVRegister) : List<string> = 
        [for i in 0..r.Dimension-1 -> sprintf "%s_%d" r.Name i]

    member private this.TranslateComplexPtr (p : ComplexPtrVariable) (i : IntExpression) (dimension : int) : List<string> = 
        let is = this.TranslateIntExpression i
        [for i in 0..dimension-1 -> sprintf "%s[%s+%d]" p.Name is i]

    member private this.TranslateVariable (v : Variable) : string =
        match v with 
        | V_Int { Name=name; } -> sprintf "size_t %s" name
        | V_Ptr { Name=name; Readonly=true } -> sprintf "std::complex<%s> const *%s" floatType name
        | V_Ptr { Name=name; Readonly=false } -> sprintf "std::complex<%s> *%s" floatType name

    member private this.TranslateRetType (v : Option<IntVariable>) : string = 
        match v with 
        | None -> "void"
        | Some _ -> "size_t"

    member private this.TranslateIntExpression (exp : IntExpression) : string = 
        match exp with
        | I_Literal i -> i.ToString()
        | I_Variable {Name=name} -> name
        | I_Add (op0, op1) -> sprintf "(%s)+(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Substract (op0, op1) -> sprintf "(%s)-(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Multiply (op0, op1) -> sprintf "(%s)*(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_And (op0, op1) -> sprintf "(%s)&(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Or (op0, op1) -> sprintf "(%s)|(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_LShift (op0, op1) -> sprintf "(%s)<<(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_RShift (op0, op1) -> sprintf "(%s)>>(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Invoke (name, args) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")

    member private this.TranslateFloatExpression (exp : FloatExpression) : string = 
        match exp with
        | F_Literal v -> sprintf "%.20f%s" v (if floatType = "float" then "f" else "")
        | F_Minus v -> sprintf "-%s" (this.TranslateFloatExpression v)
        | F_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateFloatExpression op0) (this.TranslateFloatExpression op1)
        | F_ReadReal (r, i) -> sprintf "%s.real()" ((this.TranslateComplexVRegister r).Item i)
        | F_ReadImag (r, i) -> sprintf "%s.imag()" ((this.TranslateComplexVRegister r).Item i)

    member private this.TranslateComplexVExpression (exp : ComplexVExpression) : List<string> = 
        match exp with
        | CV_New values -> [for (r, i) in values -> sprintf "std::complex<%s>(%s,%s)" floatType (this.TranslateFloatExpression r) (this.TranslateFloatExpression i)]
        | CV_DupReals r -> 
            this.TranslateComplexVRegister r |> List.map (fun s -> sprintf "std::complex<%s>(%s.real(),%s.real())" floatType s s)
        | CV_DupImags r -> 
            this.TranslateComplexVRegister r |> List.map (fun s -> sprintf "std::complex<%s>(%s.imag(),%s.imag())" floatType s s)
        | CV_Merge vars -> List.collect this.TranslateComplexVRegister vars
        | CV_Add (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s+%s" l r]
        | CV_Substract (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s-%s" l r]
        | CV_ElementMultiply (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "std::complex<%s>(%s.real()*%s.real(),%s.imag()*%s.imag())" floatType l r l r]
        | CV_CrossElementAddSub (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "std::complex<%s>(%s.real()-%s.imag(),%s.imag()+%s.real())" floatType l r l r]
        | CV_Invoke (name, args, d) -> [sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")]

    member private this.TranslateArgument (arg : ArgumentExpression) : string = 
        match arg with
        | A_Int i -> this.TranslateIntExpression i
        | A_Ptr {Name=name} -> name

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateComplexV (r, e) -> 
            sprintf "%s%s;" ident 
                (List.zip (this.TranslateComplexVRegister r) (this.TranslateComplexVExpression e) 
                |> List.map (fun (a,b)-> sprintf "auto %s=%s" a b) |> String.concat ";")
        | S_LoadComplex (r, p, i, d) -> 
            sprintf "%s%s;" ident 
                (List.zip (this.TranslateComplexVRegister r) (this.TranslateComplexPtr p i d) 
                |> List.map (fun (a,b)-> sprintf "auto %s=%s" a b) |> String.concat ";")
        | S_StoreComplexV (ptrs, r) -> 
            sprintf "%s%s;" ident 
                (List.zip (List.concat [for p, i, d in ptrs -> this.TranslateComplexPtr p i d]) (this.TranslateComplexVRegister r)
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_RangeFor ({Name=name}, start, stop, step, stats) -> 
            sprintf "%sfor (size_t %s = %d; %s < %d; %s += %d) {\n%s\n%s}" 
                ident name start name stop name step
                ([for stat in stats -> this.TranslateStatement (ident + "    ") stat] |> String.concat "\n") ident
        | S_Invoke (name, args) -> sprintf "%s%s(%s);" ident name ([for arg in args -> this.TranslateArgument arg] |> String.concat ",")
        | S_Return e -> sprintf "%sreturn %s;" ident (this.TranslateIntExpression e)

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Ret=ret; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "static %s %s(%s) {\n%s\n}" 
            (this.TranslateRetType ret)
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
            let twiddleFactorFunc = 
                sprintf "
#include <emmintrin.h>
#include <pmmintrin.h>
#include <vector>
#include <complex>
static class TwiddleFactorMatrix {
public:
    TwiddleFactorMatrix (): mMatrix(32){
        for (size_t bitCount = 0; bitCount <= 20; ++bitCount) {
            auto &factors = mMatrix[bitCount];
            auto size = 1UL << bitCount;
            factors.reserve(size + 1);
            std::complex<double> f = std::polar(1.0, 2.0 * acos(-1) / size), fi = 1;
            for (size_t j = 0; j < size; ++j, fi *= f)
                factors.push_back(fi);
            factors.push_back(1);
        }
    }
    __m128d operator () (size_t i, size_t bitCount) const {
        return _mm_load_pd(reinterpret_cast<double const*>(&mMatrix[bitCount][i]));
    }
    std::vector<std::vector<std::complex<double>>> mMatrix;
} gFactorMatrix;
static __m128d %s(size_t i, size_t bitCount) {
    return gFactorMatrix(i, bitCount);
}
                                " Constant.kTwiddleFactorFuncName
            let reverseByteFunc = 
                sprintf "static size_t %s(size_t i){\n    static size_t sReversedBytes[]={%s};\n    return sReversedBytes[i];\n}\n" 
                    Constant.kReverseByteFuncName ([for b in Util.ReversedBytes -> b.ToString()] |> String.concat ",")
            [twiddleFactorFunc; reverseByteFunc] |> String.concat "\n"

    member private this.TranslateVariable (v : Variable) : string =
        match v with 
        | V_Int { Name=name; } -> sprintf "size_t %s" name
        | V_Ptr { Name=name; Readonly=true } -> sprintf "std::complex<double> const *%s" name
        | V_Ptr { Name=name; Readonly=false } -> sprintf "std::complex<double> *%s" name

    member private this.TranslateRetType (v : Option<IntVariable>) : string = 
        match v with 
        | None -> "void"
        | Some _ -> "size_t"

    member private this.TranslateIntExpression (exp : IntExpression) : string = 
        match exp with
        | I_Literal i -> i.ToString()
        | I_Variable {Name=name} -> name
        | I_Add (op0, op1) -> sprintf "(%s)+(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Substract (op0, op1) -> sprintf "(%s)-(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Multiply (op0, op1) -> sprintf "(%s)*(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_And (op0, op1) -> sprintf "(%s)&(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Or (op0, op1) -> sprintf "(%s)|(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_LShift (op0, op1) -> sprintf "(%s)<<(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_RShift (op0, op1) -> sprintf "(%s)>>(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Invoke (name, args) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")

    member private this.TranslateFloatExpression (exp : FloatExpression) : string = 
        match exp with
        | F_Literal v -> sprintf "%.20f" v
        | F_Minus v -> sprintf "-%s" (this.TranslateFloatExpression v)
        | F_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateFloatExpression op0) (this.TranslateFloatExpression op1)
        | F_ReadReal ({ Name=name; Dimension=1 }, 0) -> sprintf "%s.m128d_f64[0]" name
        | F_ReadReal ({ Name=name; Dimension=2 }, i) -> sprintf "%s.m256d_f64[%d]" name (i * 2)
        | F_ReadImag ({ Name=name; Dimension=1 }, 0) -> sprintf "%s.m128d_f64[1]" name
        | F_ReadImag ({ Name=name; Dimension=2 }, i) -> sprintf "%s.m256d_f64[%d+1]" name (i * 2)
        | _ -> notImpl ""

    member private this.TranslateComplexVExpression (exp : ComplexVExpression) : string = 
        match exp with
        | CV_New [(r, i)] when r = i -> sprintf "_mm_set1_pd(%s)" (this.TranslateFloatExpression r)
        | CV_New [(r, i)] -> sprintf "_mm_set_pd(%s,%s)" (this.TranslateFloatExpression i) (this.TranslateFloatExpression r)
        | CV_New [(r0, i0); (r1, i1)] when r0 = i0 && r1 = i1 && r0 = r1 -> sprintf "_mm256_set1_pd(%s)" (this.TranslateFloatExpression r0)
        | CV_New [(r0, i0); (r1, i1)] -> 
            sprintf "_mm256_set_pd(%s,%s,%s,%s)" 
                (this.TranslateFloatExpression i1) (this.TranslateFloatExpression r1) 
                (this.TranslateFloatExpression i0) (this.TranslateFloatExpression r0)
        | CV_DupReals { Name=name; Dimension=1 } -> sprintf "_mm_shuffle_pd(%s,%s,0)" name name
        | CV_DupReals { Name=name; Dimension=2 } -> sprintf "_mm256_shuffle_pd(%s,%s,0)" name name
        | CV_DupImags { Name=name; Dimension=1 } -> sprintf "_mm_shuffle_pd(%s,%s,3)" name name
        | CV_DupImags { Name=name; Dimension=2 } -> sprintf "_mm256_shuffle_pd(%s,%s,15)" name name
        | CV_Merge [{ Name=name0; Dimension=1 }; { Name=name1; Dimension=1 }] -> sprintf "_mm256_set_m128d(%s,%s)" name1 name0
        | CV_Add ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "_mm_add_pd(%s,%s)" name0 name1
        | CV_Add ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm256_add_pd(%s,%s)" name0 name1
        | CV_Substract ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_sub_pd(%s,%s)" name0 name1
        | CV_Substract ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm256_sub_pd(%s,%s)" name0 name1
        | CV_ElementMultiply ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_mul_pd(%s,%s)" name0 name1
        | CV_ElementMultiply ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm256_mul_pd(%s,%s)" name0 name1
        | CV_CrossElementAddSub ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_addsub_pd(%s, _mm_shuffle_pd(%s,%s,1))" name0 name1 name1
        | CV_CrossElementAddSub ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm256_addsub_pd(%s, _mm256_shuffle_pd(%s,%s,5))" name0 name1 name1
        | CV_Invoke (name, args, d) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")
        | _ -> notImpl ""

    member private this.TranslateArgument (arg : ArgumentExpression) : string = 
        match arg with
        | A_Int i -> this.TranslateIntExpression i
        | A_Ptr {Name=name} -> name

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateComplexV ({ Name=name; }, e) -> sprintf "%sauto %s=%s;" ident name (this.TranslateComplexVExpression e)
        | S_LoadComplex ({ Name=rname; Dimension=1 }, { Name=pname; }, i, 1) -> 
            sprintf "%sauto %s=_mm_load_pd(reinterpret_cast<double const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_LoadComplex ({ Name=rname; Dimension=2 }, { Name=pname; }, i, 2) -> 
            sprintf "%sauto %s=_mm256_load_pd(reinterpret_cast<double const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_StoreComplexV ([{ Name=pname }, i, 1 ], { Name=rname; Dimension=1 }) -> 
            sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname }, i, 2 ], { Name=rname; Dimension=2 }) -> 
            sprintf "%s_mm256_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname0 }, i0, 1; { Name=pname1 }, i1, 1], { Name=rname; Dimension=2 }) when pname0 = pname1 -> 
            if FoldLiteralInt (I_Add (i0, I_Literal 1)) = i1 then
                let si0 = this.TranslateIntExpression i0
                sprintf "%s_mm256_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname0 si0 rname
            else
                [ sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),_mm256_castpd256_pd128(%s));" ident pname0 (this.TranslateIntExpression i0) rname
                  sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),_mm256_extractf128_pd(%s,1));" ident pname1 (this.TranslateIntExpression i1) rname
                  ] |> String.concat "" 
        | S_RangeFor ({Name=name}, start, stop, step, stats) -> 
            sprintf "%sfor (size_t %s = %d; %s < %d; %s += %d) {\n%s\n%s}" 
                ident name start name stop name step
                ([for stat in stats -> this.TranslateStatement (ident + "    ") stat] |> String.concat "\n") ident
        | S_Invoke (name, args) -> sprintf "%s%s(%s);" ident name ([for arg in args -> this.TranslateArgument arg] |> String.concat ",")
        | S_Return e -> sprintf "%sreturn %s;" ident (this.TranslateIntExpression e)
        | _ -> notImpl ""

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Ret=ret; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "static %s %s(%s) {\n%s\n}" 
            (this.TranslateRetType ret)
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
            let twiddleFactorFunc = 
                sprintf "
#include <emmintrin.h>
#include <pmmintrin.h>
#include <vector>
#include <complex>
static const __m128 gZero128(_mm_setzero_ps());
static class TwiddleFactorMatrix {
public:
    TwiddleFactorMatrix() : mMatrix(32) {
        for (size_t bitCount = 0; bitCount <= 20; ++bitCount) {
            auto &factors = mMatrix[bitCount];
            auto size = 1UL << bitCount;
            factors.reserve(size + 1);
            std::complex<float> f = std::polar(1.0, 2.0 * acos(-1) / size), fi = 1;
            for (size_t j = 0; j < size; ++j, fi *= f)
                factors.push_back(fi);
            factors.push_back(1);
        }
    }
    __m128 operator () (size_t i, size_t bitCount) const {
        return _mm_loadl_pi(gZero128, reinterpret_cast<__m64 const*>(&mMatrix[bitCount][i]));
    }
    std::vector<std::vector<std::complex<float>>> mMatrix;
} gFactorMatrix;
static __m128 %s(size_t i, size_t bitCount) {
    return gFactorMatrix(i, bitCount);
}
                                " Constant.kTwiddleFactorFuncName
            let reverseByteFunc = 
                sprintf "static size_t %s(size_t i){\n    static size_t sReversedBytes[]={%s};\n    return sReversedBytes[i];\n}\n" 
                    Constant.kReverseByteFuncName ([for b in Util.ReversedBytes -> b.ToString()] |> String.concat ",")
            [twiddleFactorFunc; reverseByteFunc] |> String.concat "\n"

    member private this.TranslateVariable (v : Variable) : string =
        match v with 
        | V_Int { Name=name; } -> sprintf "size_t %s" name
        | V_Ptr { Name=name; Readonly=true } -> sprintf "std::complex<float> const *%s" name
        | V_Ptr { Name=name; Readonly=false } -> sprintf "std::complex<float> *%s" name

    member private this.TranslateRetType (v : Option<IntVariable>) : string = 
        match v with 
        | None -> "void"
        | Some _ -> "size_t"

    member private this.TranslateIntExpression (exp : IntExpression) : string = 
        match exp with
        | I_Literal i -> i.ToString()
        | I_Variable {Name=name} -> name
        | I_Add (op0, op1) -> sprintf "(%s)+(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Substract (op0, op1) -> sprintf "(%s)-(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Multiply (op0, op1) -> sprintf "(%s)*(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_And (op0, op1) -> sprintf "(%s)&(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Or (op0, op1) -> sprintf "(%s)|(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_LShift (op0, op1) -> sprintf "(%s)<<(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_RShift (op0, op1) -> sprintf "(%s)>>(%s)" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
        | I_Invoke (name, args) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")

    member private this.TranslateFloatExpression (exp : FloatExpression) : string = 
        match exp with
        | F_Literal v -> sprintf "%.20ff" v
        | F_Minus v -> sprintf "-%s" (this.TranslateFloatExpression v)
        | F_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateFloatExpression op0) (this.TranslateFloatExpression op1)
        | F_ReadReal ({ Name=name; Dimension=1 }, 0) -> sprintf "%s.m128_f32[0]" name
        | F_ReadReal ({ Name=name; Dimension=2 }, i) -> sprintf "%s.m128_f32[%d]" name (i * 2)
        | F_ReadReal ({ Name=name; Dimension=4 }, i) -> sprintf "%s.m256_f32[%d]" name (i * 2)
        | F_ReadImag ({ Name=name; Dimension=1 }, 0) -> sprintf "%s.m128_f32[1]" name
        | F_ReadImag ({ Name=name; Dimension=2 }, i) -> sprintf "%s.m128_f32[%d+1]" name (i * 2)
        | F_ReadImag ({ Name=name; Dimension=4 }, i) -> sprintf "%s.m256_f32[%d+1]" name (i * 2)
        | _ -> notImpl ""

    member private this.TranslateComplexVExpression (exp : ComplexVExpression) : string = 
        match exp with
        | CV_New [(r, i)] when r = i -> sprintf "_mm_set1_ps(%s)" (this.TranslateFloatExpression r)
        | CV_New [(r, i)] -> sprintf "_mm_set_ps(0, 0, %s, %s)" (this.TranslateFloatExpression i) (this.TranslateFloatExpression r)
        | CV_New [(r0, i0); (r1, i1)] when r0 = i0 && r1 = i1 && r0 = r1 -> sprintf "_mm_set1_ps(%s)" (this.TranslateFloatExpression r0)
        | CV_New [(r0, i0); (r1, i1)] -> 
            sprintf "_mm_set_ps(%s,%s,%s,%s)" 
                (this.TranslateFloatExpression i1) (this.TranslateFloatExpression r1) 
                (this.TranslateFloatExpression i0) (this.TranslateFloatExpression r0)
        | CV_New nums when nums.Length = 4 -> 
            let h = List.head nums
            if List.forall (fun v->v = h) nums && fst h = snd h then
                sprintf "_mm256_set1_ps(%s)" (this.TranslateFloatExpression (fst h))
            else
                sprintf "_mm256_set_ps(%s)" 
                    ([for r, i in List.rev nums -> sprintf "%s,%s" (this.TranslateFloatExpression i) (this.TranslateFloatExpression r)] 
                    |> String.concat ",")
        | CV_DupReals { Name=name; Dimension=1 } -> sprintf "_mm_shuffle_ps(%s,%s,0)" name name
        | CV_DupReals { Name=name; Dimension=2 } -> sprintf "_mm_shuffle_ps(%s,%s,0xa0)" name name
        | CV_DupReals { Name=name; Dimension=4 } -> sprintf "_mm256_shuffle_ps(%s,%s,0xa0)" name name
        | CV_DupImags { Name=name; Dimension=1 } -> sprintf "_mm_shuffle_ps(%s,%s,5)" name name
        | CV_DupImags { Name=name; Dimension=2 } -> sprintf "_mm_shuffle_ps(%s,%s,0xf5)" name name
        | CV_DupImags { Name=name; Dimension=4 } -> sprintf "_mm256_shuffle_ps(%s,%s,0xf5)" name name
        | CV_Merge [{ Name=name0; Dimension=1 }; { Name=name1; Dimension=1 }] -> sprintf "_mm_shuffle_ps(%s,%s,0x44)" name0 name1
        | CV_Merge [{ Name=name0; Dimension=1 }; { Name=name1; Dimension=1 }; { Name=name2; Dimension=1 };{ Name=name3; Dimension=1 };] -> 
            sprintf "_mm256_set_m128(_mm_shuffle_ps(%s,%s,0x44),_mm_shuffle_ps(%s,%s,0x44))" name2 name3 name0 name1
        | CV_Merge [{ Name=name0; Dimension=2 }; { Name=name1; Dimension=2 }] -> sprintf "_mm256_set_m128(%s,%s)" name1 name0
        | CV_Add ({ Name=name0; Dimension=1 }, { Name=name1; Dimension=1 }) -> sprintf "_mm_add_ps(%s,%s)" name0 name1
        | CV_Add ({ Name=name0; Dimension=2 }, { Name=name1; Dimension=2 }) -> sprintf "_mm_add_ps(%s,%s)" name0 name1
        | CV_Add ({ Name=name0; Dimension=4 }, { Name=name1; Dimension=4 }) -> sprintf "_mm256_add_ps(%s,%s)" name0 name1
        | CV_Substract ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_sub_ps(%s,%s)" name0 name1
        | CV_Substract ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm_sub_ps(%s,%s)" name0 name1
        | CV_Substract ({ Name=name0; Dimension=4 },{ Name=name1; Dimension=4 }) -> sprintf "_mm256_sub_ps(%s,%s)" name0 name1
        | CV_ElementMultiply ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_mul_ps(%s,%s)" name0 name1
        | CV_ElementMultiply ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm_mul_ps(%s,%s)" name0 name1
        | CV_ElementMultiply ({ Name=name0; Dimension=4 },{ Name=name1; Dimension=4 }) -> sprintf "_mm256_mul_ps(%s,%s)" name0 name1
        | CV_CrossElementAddSub ({ Name=name0; Dimension=1 },{ Name=name1; Dimension=1 }) -> sprintf "_mm_addsub_ps(%s, _mm_shuffle_ps(%s,%s,0xb1))" name0 name1 name1
        | CV_CrossElementAddSub ({ Name=name0; Dimension=2 },{ Name=name1; Dimension=2 }) -> sprintf "_mm_addsub_ps(%s, _mm_shuffle_ps(%s,%s,0xb1))" name0 name1 name1
        | CV_CrossElementAddSub ({ Name=name0; Dimension=4 },{ Name=name1; Dimension=4 }) -> sprintf "_mm256_addsub_ps(%s, _mm256_shuffle_ps(%s,%s,0xb1))" name0 name1 name1
        | CV_Invoke (name, args, d) -> sprintf "%s(%s)" name ([for arg in args -> this.TranslateArgument arg ] |> String.concat ",")
        | _ -> notImpl ""

    member private this.TranslateArgument (arg : ArgumentExpression) : string = 
        match arg with
        | A_Int i -> this.TranslateIntExpression i
        | A_Ptr {Name=name} -> name

    member private this.TranslateStatement (ident : string) (stat : Statement) : string = 
        match stat with 
        | S_EvaluateComplexV ({ Name=name; }, e) -> sprintf "%sauto %s=%s;" ident name (this.TranslateComplexVExpression e)
        | S_LoadComplex ({ Name=rname; Dimension=1 }, { Name=pname; }, i, 1) -> 
            sprintf "%sauto %s=_mm_loadl_pi(gZero128, reinterpret_cast<__m64 const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_LoadComplex ({ Name=rname; Dimension=2 }, { Name=pname; }, i, 2) -> 
            sprintf "%sauto %s=_mm_load_ps(reinterpret_cast<float const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_LoadComplex ({ Name=rname; Dimension=4 }, { Name=pname; }, i, 4) -> 
            sprintf "%sauto %s=_mm256_load_ps(reinterpret_cast<float const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_StoreComplexV ([{ Name=pname }, i, 1 ], { Name=rname; Dimension=1 }) -> 
            sprintf "%s_mm_storel_pi(reinterpret_cast<__m64*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname }, i, 2 ], { Name=rname; Dimension=2 }) -> 
            sprintf "%s_mm_store_ps(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname }, i, 4 ], { Name=rname; Dimension=4 }) -> 
            sprintf "%s_mm256_store_ps(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname0 }, i0, 1; { Name=pname1 }, i1, 1], { Name=rname; Dimension=2 }) when pname0 = pname1 -> 
            if FoldLiteralInt (I_Add (i0, I_Literal 1)) = i1 then
                let si0 = this.TranslateIntExpression i0
                sprintf "%s_mm_store_ps(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname0 si0 rname
            else
                [ sprintf "%s_mm_storel_pi(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname0 (this.TranslateIntExpression i0) rname
                  sprintf "%s_mm_storeh_pi(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname1 (this.TranslateIntExpression i1) rname
                  ] |> String.concat ""
        | S_StoreComplexV ([{ Name=pname0 }, i0, 1; { Name=pname1 }, i1, 1; { Name=pname2 }, i2, 1; { Name=pname3 }, i3, 1], { Name=rname; Dimension=4 }) 
            when pname0 = pname1 && pname0 = pname2 && pname0 = pname3 -> 
                let i0Plus1, i1Plus1, i2Plus1 = FoldLiteralInt (I_Add (i0, I_Literal 1)), FoldLiteralInt (I_Add (i1, I_Literal 1)), FoldLiteralInt (I_Add (i2, I_Literal 1))
                if i0Plus1 = i1 && i1Plus1 = i2 && i2Plus1 = i3 then
                    let si0 = this.TranslateIntExpression i0
                    sprintf "%s_mm256_store_ps(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname0 si0 rname
                else
                    sprintf 
                        "%s%s;" 
                        ident 
                        ([
                            sprintf "auto %s_lo=_mm256_castps256_ps128(%s)" rname rname;
                            sprintf "auto %s_hi=_mm256_extractf128_ps(%s,1)" rname rname;
                            sprintf "_mm_storel_pi(reinterpret_cast<float*>(&%s[%s]),%s_lo);" pname0 (this.TranslateIntExpression i0) rname
                            sprintf "_mm_storeh_pi(reinterpret_cast<float*>(&%s[%s]),%s_lo);" pname1 (this.TranslateIntExpression i1) rname
                            sprintf "_mm_storel_pi(reinterpret_cast<float*>(&%s[%s]),%s_hi);" pname2 (this.TranslateIntExpression i2) rname
                            sprintf "_mm_storeh_pi(reinterpret_cast<float*>(&%s[%s]),%s_hi);" pname3 (this.TranslateIntExpression i3) rname
                        ] |> String.concat ";")
        | S_StoreComplexV ([{ Name=pname0 }, i0, 2; { Name=pname1 }, i1, 2], { Name=rname; Dimension=4 }) when pname0 = pname1 -> 
            if FoldLiteralInt (I_Add (i0, I_Literal 1)) = i1 then
                let si0 = this.TranslateIntExpression i0
                sprintf "%s_mm256_store_ps(reinterpret_cast<float*>(&%s[%s]),%s);" ident pname0 si0 rname
            else
                [ sprintf "%s_mm_store_ps(reinterpret_cast<float*>(&%s[%s]),_mm256_castps256_ps128(%s));" ident pname0 (this.TranslateIntExpression i0) rname
                  sprintf "%s_mm_store_ps(reinterpret_cast<float*>(&%s[%s]),_mm256_extractf128_ps(%s,1));" ident pname1 (this.TranslateIntExpression i1) rname
                  ] |> String.concat ""
        | S_RangeFor ({Name=name}, start, stop, step, stats) -> 
            sprintf "%sfor (size_t %s = %d; %s < %d; %s += %d) {\n%s\n%s}" 
                ident name start name stop name step
                ([for stat in stats -> this.TranslateStatement (ident + "    ") stat] |> String.concat "\n") ident
        | S_Invoke (name, args) -> sprintf "%s%s(%s);" ident name ([for arg in args -> this.TranslateArgument arg] |> String.concat ",")
        | S_Return e -> sprintf "%sreturn %s;" ident (this.TranslateIntExpression e)
        | _ -> notImpl ""

    member private this.TranslateBody (body : List<Statement>) : string = 
        body |> List.map (this.TranslateStatement "    ") |> String.concat "\n"

    member this.Translate ({Name=name; Parameters=parameters; Ret=ret; Body=body; } : Function) : string = 
        this.GenHeader() + sprintf "static %s %s(%s) {\n%s\n}" 
            (this.TranslateRetType ret)
            name 
            (List.map this.TranslateVariable parameters |> String.concat ", ") 
            (this.TranslateBody body)

end
//-------------------------------------------------------------------------------
let GenerateFFTCode (path : string) (sizes : List<int>) (dlp : int) (translator : Function -> string) =
    let brcGen = new BitReverseCopyFunctionGenerator()
    let fftGen = new FFTFunctionGenerator()
    System.IO.File.WriteAllLines(path,
        List.concat [
                [for size in sizes do 
                    yield! brcGen.ForwardGen size
                    yield! brcGen.BackwardGen size];
                [for size in sizes do 
                    yield! fftGen.ForwardGen size dlp; 
                    yield! fftGen.BackwardGen size dlp] ] 
            |> List.map translator);


[<EntryPoint>]
let main argv =

//    GenerateFFTCode 
//        "C:\Programming\Projects\Github\DailyProjects\Python\BigInteger\FFT_gen.py"
//        [for i in 0..20 -> 1 <<< i ]
//        2
//        ((new PythonTranslator()).Translate);

//    GenerateFFTCode 
//        "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h"
//        [for i in 0..20 -> 1 <<< i ]
//        1
//        ((new CppTranslator("float")).Translate);

//    GenerateFFTCode 
//        "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h"
//        [for i in 0..20 -> 1 <<< i ]
//        2
//        ((new CppTranslator("double")).Translate);

    GenerateFFTCode 
        "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h"
        [for i in 0..20 -> 1 <<< i ]
        4
        ((new CppWithSIMDTranslatorF()).Translate);

//    GenerateFFTCode 
//        "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h"
//        [for i in 0..20 -> 1 <<< i ]
//        2
//        ((new CppWithSIMDTranslator()).Translate);

    0
