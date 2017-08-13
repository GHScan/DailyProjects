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
    | S_LoadComplex of Register : ComplexVRegister * Ptr : ComplexPtrVariable * Index : IntExpression
    | S_StoreComplexV of Ptrs : List<ComplexPtrVariable * IntExpression> * Register : ComplexVRegister
    | S_RangeFor of Variable : IntVariable * Start : IntExpression * Stop : IntExpression * Body : List<Statement>
    | S_Invoke of Name : string * Arguments : List<ArgumentExpression>
    | S_Return of Expression : IntExpression

type Function = { Name : string; Parameters : List<Variable>; Ret : Option<IntVariable>; Body : List<Statement> }


let DimensionOf (exp : ComplexVExpression) : int = 
    match exp with
        | CV_New nums -> nums.Length
        | CV_Merge rs -> List.sumBy (fun r -> r.Dimension) rs
        | CV_Add (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_Substract (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_ElementMultiply (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_CrossElementAddSub (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_Invoke (name, args, d) -> d

let rec FoldLiteralInt (exp : IntExpression) : IntExpression = 
    match exp with
    | I_Literal _ -> exp
    | I_Variable _ -> exp
    | I_Add (op0, op1) -> match (FoldLiteralInt op0, FoldLiteralInt op1) with 
                          | (I_Literal a, I_Literal b) -> I_Literal (a+b)
                          | (a, b) -> I_Add (a, b)
    | I_Substract (op0, op1) -> match (FoldLiteralInt op0, FoldLiteralInt op1) with 
                                | (I_Literal a, I_Literal b) -> I_Literal (a-b)
                                | (a, b) -> I_Substract (a, b)
    | I_Multiply (op0, op1) -> match (FoldLiteralInt op0, FoldLiteralInt op1) with 
                                | (I_Literal a, I_Literal b) -> I_Literal (a*b)
                                | (a, b) -> I_Multiply (a, b)
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
    let kSmallSizeBits = 6
    let kSmallSize = 1 <<< kSmallSizeBits

type BitReverseCopyCodeGenerator() = class
    
    member private this.GenUnrolledForwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let statements = [0..size-1] |> List.chunkBySize 4 |> List.collect (fun ints -> 
                let temps = [for i in ints ->{ Name=Constant.kComplexVRegisterPrefix+i.ToString(); Dimension=1 }]
                List.concat [
                    [for i, temp in List.zip ints temps -> S_LoadComplex (temp, srcParam, I_Literal i)];
                    [for i, temp in List.zip ints temps -> S_StoreComplexV ([destParam, I_Literal (Util.ReverseBits i (Util.ILog2 size))], temp)]])
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
                    [for i, temp in List.zip ints temps -> S_LoadComplex (temp, srcParam, I_Literal i)];
                    [for temp, scaledTemp in List.zip temps scaledTemps -> S_EvaluateComplexV (scaledTemp, CV_ElementMultiply (temp, scalerVar))]
                    [for i, scaledTemp in List.zip ints scaledTemps -> S_StoreComplexV ([destParam, I_Literal (Util.ReverseBits i (Util.ILog2 size))], scaledTemp)]])
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

    member private this.GenLoopForwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let loopV : IntVariable = { Name=Constant.kDefaultLoopVariableName }
        let temp = { Name=Constant.kComplexVRegisterPrefix+"0"; Dimension=1 }
        let load = S_LoadComplex (temp, srcParam, I_Variable loopV)
        let store = S_StoreComplexV (
                        [destParam, (I_Invoke (Constant.kReverseBitsFuncPrefix+size.ToString(), [A_Int (I_Variable loopV)]))], 
                        temp)
        let stat = S_RangeFor (loopV, (I_Literal 0), (I_Literal size), [load; store])
        {   Name=Constant.kBitReverseCopyFuncPrefix+size.ToString(); 
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None; 
            Body=[stat] }

    member private this.GenLoopBackwardFunction (size : int) : Function = 
        let srcParam : ComplexPtrVariable = { Name=Constant.kSrcParamName; Readonly=true }
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let loopV : IntVariable = { Name=Constant.kDefaultLoopVariableName }
        let scalerVar = { Name=Constant.kScalerVariableName; Dimension=1 }
        let temp = { Name=Constant.kComplexVRegisterPrefix+"0"; Dimension=1 }
        let scaledTemp = { Name=Constant.kComplexVRegisterPrefix+"1"; Dimension=1 }
        let defScaler = S_EvaluateComplexV (scalerVar, CV_New [F_Literal (1.0/(double size)), F_Literal (1.0/(double size))])
        let load = S_LoadComplex (temp, srcParam, I_Variable loopV)
        let scale = S_EvaluateComplexV (scaledTemp, CV_ElementMultiply (temp, scalerVar))
        let store = S_StoreComplexV (
                        [destParam, (I_Invoke (Constant.kReverseBitsFuncPrefix+size.ToString(), [A_Int (I_Variable loopV)]))], 
                        scaledTemp)
        let stat = S_RangeFor (loopV, (I_Literal 0), (I_Literal size), [load; scale; store])
        {   Name=Constant.kInverseBitReverseCopyFuncPrefix+size.ToString(); 
            Parameters=[V_Ptr destParam; V_Ptr srcParam]; Ret=None; 
            Body=[defScaler; stat] }
    
    member this.ForwardGen (size : int) : List<Function> = 
        if size <= Constant.kSmallSize then
            [ this.GenUnrolledForwardFunction size ]
        else
            [ this.GenReverseBitsFunction size; this.GenLoopForwardFunction size ] 

    member this.BackwardGen (size : int) : List<Function> = 
        if size <= Constant.kSmallSize then
            [ this.GenUnrolledBackwardFunction size ]
        else
            [ this.GenLoopBackwardFunction size ] 

end

type RecursiveFFTCodeGenerator() = class
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

    member private this.StoreComplexV (ptrs : List<ComplexPtrVariable * IntExpression>) (r : ComplexVRegister) =
        assert (List.forall (fun (p, i)-> not p.Readonly) ptrs)
        statements <- (S_StoreComplexV (ptrs, r)) :: statements

    member private this.LoadComplex (ptr : ComplexPtrVariable) (index : IntExpression) : ComplexVRegister =
        let reg : ComplexVRegister = { Name = this.NewRegisterName(); Dimension = 1 }
        statements <- (S_LoadComplex (reg, ptr, index)) :: statements
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
        let reals = this.EvaluateComplexV (CV_New [for c in cs -> (F_ReadReal (c, 0), F_ReadReal (c, 0))])
        let imags = this.EvaluateComplexV (CV_New [for c in cs -> (F_ReadImag (c, 0), F_ReadImag (c, 0))])
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

    member this.RecursiveFFT 
            (a : List<ComplexPtrVariable * IntExpression>) (initialE : IntExpression, initialN : int) (dlp : int) (direction : int) 
            : List<ComplexVRegister> =

        if a.Length = 1 then
            let (p, i) = a.Item 0
            [ this.LoadComplex p i]
        else
            let a0, a1 = List.splitAt (a.Length / 2) a
            let nextInitialFactor = (I_Multiply (initialE, I_Literal 2), initialN)

            let b0 = this.RecursiveFFT a0 nextInitialFactor dlp direction |> this.MergeRegisters dlp
            let b1 = this.RecursiveFFT a1 nextInitialFactor dlp direction |> this.MergeRegisters dlp

            let factors = [for i in 0..a0.Length-1 ->
                            (I_Add (initialE, I_Literal (initialN / a.Length * i)), initialN)] 
                            |> List.chunkBySize (a0.Length / b0.Length)

            let mutable o0, o1 = List.empty, List.empty

            for c0, c1, f in List.zip3 b0 b1 factors do
                let c1f = this.Multiply c1 f direction
                o0 <- (this.Add c0 c1f) :: o0
                o1 <- (this.Substract c0 c1f) :: o1

            List.concat [List.rev o0; List.rev o1]
    
    member this.Gen (a : List<ComplexPtrVariable * IntExpression>) (initialFactor : IntExpression * int) (dlp : int) (direction : int) : List<Statement> = 
        this.Reset()

        let rs = this.RecursiveFFT a initialFactor dlp direction
        for ptrs, r in List.zip (List.chunkBySize (a.Length/rs.Length) a) rs do
            this.StoreComplexV ptrs r

        List.rev statements
end

type FFTRadix2CodeGenerator() = class

    member private this.InvokeKernel (size : int) (destParam : ComplexPtrVariable) (offset : IntExpression) (direction : int) : List<Statement> = 
        if size = 1 then
            List.empty
        else
            let prefix = if direction > 0 then Constant.kKernelFuncPrefix else Constant.kInverseKernelFuncPrefix
            [ S_Invoke (prefix + size.ToString(), [A_Ptr destParam; A_Int offset]) ]

    member private this.RangeFor (start : int) (stop : int) (bodyGen : IntExpression -> List<Statement>) : List<Statement> =
        if start + 1 > stop then
            List.empty
        else if start + 1 = stop then
            bodyGen (I_Literal start)
        else
            let var : IntVariable = { Name=Constant.kDefaultLoopVariableName }
            [ S_RangeFor (var, I_Literal start, I_Literal stop, bodyGen (I_Variable var)) ]

    member private this.GenKernelFunction (size : int) (dlp : int) (direction : int) : Function = 
        let destParam : ComplexPtrVariable = { Name=Constant.kDestParamName; Readonly=false }
        let offsetParam : IntVariable = { Name=Constant.kOffsetParamName }
        let prevKernelSize = 
            if size = 1 then 0
            else 1 <<< (((Util.ILog2 size) - 1) / Constant.kSmallSizeBits * Constant.kSmallSizeBits)
        let prevKernelCount = 
            if size = 1 then 0
            else size / prevKernelSize
        let invokeKernels = List.concat [for i in 0..prevKernelCount-1 ->
                                            this.InvokeKernel 
                                                prevKernelSize 
                                                destParam 
                                                (I_Add (I_Variable offsetParam, I_Literal (i * prevKernelSize))) 
                                                direction
                                        ] 
        let loop = this.RangeFor 0 prevKernelSize (fun iv -> 
                let recFFTGen = new RecursiveFFTCodeGenerator()
                let a = [for i in 0..prevKernelCount-1 -> (destParam, FoldLiteralInt (I_Add (I_Variable offsetParam,(I_Add (iv, I_Literal (i * prevKernelSize))))))]
                recFFTGen.Gen a (iv, size) dlp direction)
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
        assert (dlp = 1 || dlp = 2)
        [ this.GenKernelFunction size  dlp 1; this.GenFFTFunction size 1]

    member this.BackwardGen (size : int) (dlp : int) : List<Function> = 
        assert (dlp = 1 || dlp = 2)
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

    member private this.TranslateComplexPtr (p : ComplexPtrVariable) (i : IntExpression) : string = 
        sprintf "%s[%s]" p.Name (this.TranslateIntExpression i)

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
        | I_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
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
        | S_LoadComplex (r, p, i) -> 
            assert (r.Dimension = 1)
            sprintf "%s%s=%s" ident ((this.TranslateComplexVRegister r).Item 0) (this.TranslateComplexPtr p i)
        | S_StoreComplexV (ptrs, r) -> 
            sprintf "%s%s" ident 
                (List.zip [for p, i in ptrs -> this.TranslateComplexPtr p i] (this.TranslateComplexVRegister r)
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_RangeFor ({Name=name}, s, e, stats) -> 
            sprintf "%sfor %s in range(%s,%s):\n%s" 
                ident name (this.TranslateIntExpression s) (this.TranslateIntExpression e) 
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

type CppTranslator() = class 
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
static std::complex<double> %s(size_t i, size_t bitCount) {
    static std::vector<std::vector<std::complex<double>>> sFactorMatrix(32);
    auto &factors = sFactorMatrix[bitCount];
    if (factors.size() == 0) {
        auto size = 1UL << bitCount;
        factors.reserve(size + 1);
        std::complex<double> f = std::polar(1.0, 2.0 * acos(-1) / size), fi = 1;
        for (size_t j = 0; j < size; ++j, fi *= f) 
            factors.push_back(fi);
        factors.push_back(1);
    }
    return factors[i];
}
                                " Constant.kTwiddleFactorFuncName
            let reverseByteFunc = 
                sprintf "static size_t %s(size_t i){\n    static size_t sReversedBytes[]={%s};\n    return sReversedBytes[i];\n}\n" 
                    Constant.kReverseByteFuncName ([for b in Util.ReversedBytes -> b.ToString()] |> String.concat ",")
            [twiddleFactorFunc; reverseByteFunc] |> String.concat "\n"

    member private this.TranslateComplexVRegister (r : ComplexVRegister) : List<string> = 
        [for i in 0..r.Dimension-1 -> sprintf "%s_%d" r.Name i]

    member private this.TranslateComplexPtr (p : ComplexPtrVariable) (i : IntExpression) : string = 
        sprintf "%s[%s]" p.Name (this.TranslateIntExpression i)

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
        | I_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
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
        | F_ReadReal (r, i) -> sprintf "%s.real()" ((this.TranslateComplexVRegister r).Item i)
        | F_ReadImag (r, i) -> sprintf "%s.imag()" ((this.TranslateComplexVRegister r).Item i)

    member private this.TranslateComplexVExpression (exp : ComplexVExpression) : List<string> = 
        match exp with
        | CV_New values -> [for (r, i) in values -> sprintf "std::complex<double>(%s,%s)" (this.TranslateFloatExpression r) (this.TranslateFloatExpression i)]
        | CV_Merge vars -> List.collect this.TranslateComplexVRegister vars
        | CV_Add (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s+%s" l r]
        | CV_Substract (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "%s-%s" l r]
        | CV_ElementMultiply (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "std::complex<double>(%s.real()*%s.real(),%s.imag()*%s.imag())" l r l r]
        | CV_CrossElementAddSub (op0, op1) -> [for l, r in List.zip (this.TranslateComplexVRegister op0) (this.TranslateComplexVRegister op1) 
                                -> sprintf "std::complex<double>(%s.real()-%s.imag(),%s.imag()+%s.real())" l r l r]
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
        | S_LoadComplex (r, p, i) -> 
            assert (r.Dimension = 1)
            sprintf "%sauto %s=%s;" ident ((this.TranslateComplexVRegister r).Item 0) (this.TranslateComplexPtr p i)
        | S_StoreComplexV (ptrs, r) -> 
            sprintf "%s%s;" ident 
                (List.zip [for p, i in ptrs -> this.TranslateComplexPtr p i] (this.TranslateComplexVRegister r)
                |> List.map (fun (a,b)-> sprintf "%s=%s" a b) |> String.concat ";")
        | S_RangeFor ({Name=name}, s, e, stats) -> 
            sprintf "%sfor (size_t %s = %s; %s < %s; ++%s) {\n%s\n%s}" 
                ident name (this.TranslateIntExpression s) name (this.TranslateIntExpression e) name
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
    __m128d operator () (size_t i, size_t bitCount) {
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
        | I_Multiply (op0, op1) -> sprintf "%s*%s" (this.TranslateIntExpression op0) (this.TranslateIntExpression op1)
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
        | S_LoadComplex ({ Name=rname; Dimension=1 }, { Name=pname; }, i) -> 
            sprintf "%sauto %s=_mm_load_pd(reinterpret_cast<double const*>(&%s[%s]));" ident rname pname (this.TranslateIntExpression i)
        | S_StoreComplexV ([{ Name=pname }, i], { Name=rname; Dimension=1 }) -> 
            sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname (this.TranslateIntExpression i) rname
        | S_StoreComplexV ([{ Name=pname0 }, i0; { Name=pname1 }, i1], { Name=rname; Dimension=2 }) when pname0 = pname1 -> 
            let foldedI0, foldedI1 = FoldLiteralInt i0, FoldLiteralInt i1
            if I_Add (i0, I_Literal 1) = i1 then
                let si0 = this.TranslateIntExpression i0
                sprintf "%s_mm256_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname0 si0 rname
            else if I_Add (i1, I_Literal 1) = i0 then
                let si1 = this.TranslateIntExpression i1
                sprintf "%s_mm256_store_pd(reinterpret_cast<double*>(&%s[%s]),%s);" ident pname0 si1 rname
            else
                [ sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),_mm256_castpd256_pd128(%s));" ident pname0 (this.TranslateIntExpression i0) rname
                  sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%s]),_mm256_extractf128_pd(%s,1));" ident pname1 (this.TranslateIntExpression i1) rname
                  ] |> String.concat "" 
        | S_RangeFor ({Name=name}, s, e, stats) -> 
            sprintf "%sfor (size_t %s = %s; %s < %s; ++%s) {\n%s\n%s}" 
                ident name (this.TranslateIntExpression s) name (this.TranslateIntExpression e) name
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
    let brcGen = new BitReverseCopyCodeGenerator()
    let fftGen = new FFTRadix2CodeGenerator()
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
//        2
//        ((new CppTranslator()).Translate);

    GenerateFFTCode 
        "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h"
        [for i in 0..20 -> 1 <<< i ]
        2
        ((new CppWithSIMDTranslator()).Translate);

    0