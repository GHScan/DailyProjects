module Program


open System


//-------------------------------------------------------------------------------
type Address = { Base : string; Offset : int }
type Complex = System.Numerics.Complex
type ComplexVRegister = { Name : string; Dimension : int }
type FloatExpression =
    | F_Literal of Value : double
    | F_Minus of Operand : FloatExpression
    | F_Multiply of Operand0 : FloatExpression * Operand1 : FloatExpression
    | F_ReadReal of Register : ComplexVRegister * Index : int
    | F_ReadImag of Register : ComplexVRegister * Index : int
type ComplexVExpression = 
    | CV_New of Numbers : List<FloatExpression * FloatExpression>
    | CV_Variable of Address : Address * Dimension : int
    | CV_Merge of Registers : List<ComplexVRegister>
    | CV_Minus of Operand : ComplexVRegister
    | CV_Add of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_Substract of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_ElementMultiply of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
    | CV_ElementCrossAddSub of Operand0 : ComplexVRegister * Operand1 : ComplexVRegister
type Instruction = 
    | I_Load of Register : ComplexVRegister * Expression : ComplexVExpression
    | I_Store of Address : Address * Register : ComplexVRegister
type Function = { Name : string; Parameters : List<string>; Body : List<Instruction> }


let DimensionOf (exp : ComplexVExpression) : int = 
    match exp with
        | CV_New nums -> nums.Length
        | CV_Variable (a, d) -> d
        | CV_Merge rs -> List.sumBy (fun r -> r.Dimension) rs
        | CV_Minus op -> op.Dimension
        | CV_Add (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_Substract (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_ElementMultiply (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension
        | CV_ElementCrossAddSub (op1, op2) -> assert (op1.Dimension = op2.Dimension); op1.Dimension

//-------------------------------------------------------------------------------
module Utility =

    let FloatEquals (f0 : double) (f1 : double) : bool =
        Math.Abs(f0 - f1) < 0.000001

    let ComplexEquals (c0 : Complex) (c1 : Complex) : bool = 
        FloatEquals c0.Real c1.Real && FloatEquals c0.Imaginary c1.Imaginary

    let TwiddleFactor (n : int) (e : int) : Complex = 
        Complex.FromPolarCoordinates(1.0, 2.0 * Math.PI * (double e) / (double n))

//-------------------------------------------------------------------------------
[< AbstractClass >]
type FFTCodeGenerator() = class
    let mutable instructions : List<Instruction> = List.empty
    let mutable exp2Register : Map<ComplexVExpression, ComplexVRegister> = Map.empty
    let mutable registerId = 0

    let fftFuncName, ifftFuncName = "FFT_", "IFFT_"

    member private this.Reset() =
        instructions <- List.empty
        exp2Register <- Map.empty
        registerId <- 0

    member private this.NewRegisterName () : string = 
        registerId <- registerId + 1
        sprintf "temp_%d" registerId

    member private this.Evaluate (exp : ComplexVExpression) : ComplexVRegister = 
        let reg = exp2Register |> Map.tryFind exp
        if Option.isSome reg then
            Option.get reg
        else
            let reg : ComplexVRegister = { Name = this.NewRegisterName(); Dimension = DimensionOf exp }
            exp2Register <- exp2Register.Add(exp, reg)
            instructions <- I_Load (reg, exp) :: instructions
            reg

    member private this.Store (addr : Address) (r : ComplexVRegister) =
        instructions <- (I_Store (addr, r)) :: instructions

    member this.LoadVariable (addr : Address) (dimension : int) : ComplexVRegister =
        this.Evaluate (CV_Variable (addr, dimension))

    member private this.Multiply1 (r : ComplexVRegister) (c : Complex) : ComplexVRegister = 
        assert (r.Dimension = 1)
        if Utility.ComplexEquals c Complex.One then
            r
        else if Utility.ComplexEquals -c Complex.One then
            this.Evaluate (CV_Minus r)
        else if Utility.ComplexEquals c Complex.ImaginaryOne then
            this.Evaluate (CV_New [(F_Minus (F_ReadImag (r, 0)), F_ReadReal (r, 0))])
        else if Utility.ComplexEquals -c Complex.ImaginaryOne then
            this.Evaluate (CV_New [(F_ReadImag (r, 0), F_Minus (F_ReadReal (r, 0)))])
        else if Utility.FloatEquals c.Imaginary 0.0 then
            let scaler = this.Evaluate (CV_New [(F_Literal c.Real, F_Literal c.Real)])
            this.Evaluate (CV_ElementMultiply (r, scaler))
        else if Utility.FloatEquals c.Real 0.0 then
            let scaler = this.Evaluate (CV_New [(F_Literal c.Imaginary, F_Literal c.Imaginary)])
            let scaled = this.Evaluate (CV_ElementMultiply (r, scaler))
            this.Evaluate (CV_New [(F_Minus (F_ReadImag (scaled, 0)), F_ReadReal (scaled, 0))])
        else
            this.MultiplyV r [c]

    member private this.MultiplyV (r : ComplexVRegister) (cs : List<Complex>) : ComplexVRegister = 
        let reals = this.Evaluate (CV_New [for c in cs do yield (F_Literal c.Real, F_Literal c.Real)])
        let imags = this.Evaluate (CV_New [for c in cs do yield (F_Literal c.Imaginary, F_Literal c.Imaginary)])
        let rTimesReal = this.Evaluate (CV_ElementMultiply (r, reals))
        let rTimesImags = this.Evaluate (CV_ElementMultiply (r, imags))
        this.Evaluate (CV_ElementCrossAddSub (rTimesReal, rTimesImags))

    member this.Multiply (r : ComplexVRegister) (cs : List<Complex>) : ComplexVRegister = 
        if cs.Length = 1 then
            this.Multiply1 r (cs.Item 0)
        else
            this.MultiplyV r cs

    member this.Add (op0 : ComplexVRegister) (op1 : ComplexVRegister) : ComplexVRegister =
        if op0.Name < op1.Name then
            this.Evaluate (CV_Add (op0, op1))
        else
            this.Add op1 op0

    member this.Substract (op0 : ComplexVRegister) (op1 : ComplexVRegister) : ComplexVRegister = 
        this.Evaluate (CV_Substract (op0, op1))

    member this.MergeRegisters (dlp : int) (l : List<ComplexVRegister>) : List<ComplexVRegister> =
        let chunk = dlp / (l.Item 0).Dimension
        l |> List.chunkBySize chunk |> List.collect (fun rs -> if rs.Length > 1 then [this.Evaluate (CV_Merge rs)] else rs)

    member this.ForwardGen (size : int) (dlp : int) : Function =
        assert (dlp = 1 || dlp = 2)
        this.Reset()
        
        let srcName, destName = "src", "dest"
        let input = [for i in 0..size-1 do yield { Base = srcName; Offset = i }]
        let output = this.RecursiveFFT input 1 1.0 dlp
        output |> List.iteri (fun i r -> this.Store { Base = destName; Offset = i * r.Dimension } r)

        { Name = fftFuncName + size.ToString(); Parameters = [destName; srcName]; Body = List.rev instructions; }

    member this.BackwardGen (size : int) (dlp : int) : Function =
        assert (dlp = 1 || dlp = 2)
        this.Reset()

        let srcName, destName = "src", "dest"
        let input = [for i in 0..size-1 do yield { Base = srcName; Offset = i }]
        let output = this.RecursiveFFT input -1 (1.0 / (double size)) dlp
        output |> List.iteri (fun i r -> this.Store { Base = destName; Offset = i * r.Dimension } r)

        { Name = ifftFuncName + size.ToString(); Parameters = [destName; srcName]; Body = List.rev instructions; }

    abstract member RecursiveFFT : List<Address> -> int -> double -> int -> List<ComplexVRegister>
 end

type FFTRadix2CodeGenerator() = class
    inherit FFTCodeGenerator()

    override this.RecursiveFFT (a : List<Address>) (flag : int) (scale : double) (dlp : int) : List<ComplexVRegister> =
        if a.Length = 1 then
            [ this.Multiply (this.LoadVariable (a.Item 0) 1) [Complex(scale, 0.0)] ]
        else
            let a0, a1 = List.foldBack (fun v (l,r)-> (v::r,l)) a ([],[])

            let b0 = this.RecursiveFFT a0 flag scale dlp |> this.MergeRegisters dlp
            let b1 = this.RecursiveFFT a1 flag scale dlp |> this.MergeRegisters dlp

            let factors = [for e in 0..a0.Length-1 do 
                            yield Utility.TwiddleFactor a.Length (e * flag) ] 
                            |> List.chunkBySize (a0.Length / b0.Length)


            let mutable o0, o1 = List.empty, List.empty

            for c0, c1, f in List.zip3 b0 b1 factors do
                let c1f = this.Multiply c1 f
                o0 <- (this.Add c0 c1f) :: o0
                o1 <- (this.Substract c0 c1f) :: o1

            List.concat [List.rev o0; List.rev o1]
end


//-------------------------------------------------------------------------------
let Translate2Python (func : Function) : string = 
    let translateRegister ({ Name=name; Dimension=d } : ComplexVRegister) : List<string> = 
        [for i in 0..d-1 do yield sprintf "%s_%d" name i] 
    let translateAddress ({ Base=b; Offset=off; } : Address) (dimension : int) : List<string> =
        [for i in 0..dimension-1 do yield sprintf "%s[%d]" b (off + i)] 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | F_Literal v -> sprintf "%.16f" v
            | F_Minus v -> sprintf "-%s" (translateFloatExp v)
            | F_Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | F_ReadReal (c, i) -> sprintf "%s_%d.real" c.Name i
            | F_ReadImag (c, i) -> sprintf "%s_%d.imag" c.Name i
    let translateComplexVExp (exp : ComplexVExpression) : List<string> =
        match exp with 
            | CV_New nums -> [for (r, i) in nums do yield sprintf "complex(%s,%s)" (translateFloatExp r) (translateFloatExp i)]
            | CV_Variable (addr, d) -> translateAddress addr d
            | CV_Merge rs -> rs |> List.map translateRegister |> List.concat
            | CV_Minus r -> translateRegister r |> List.map (fun s->"-"+s) 
            | CV_Add (r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do yield s0+"+"+s1 ]
            | CV_Substract(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do yield s0+"-"+s1 ]
            | CV_ElementMultiply(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do 
                                                yield sprintf "complex(%s.real*%s.real,%s.imag*%s.imag)" s0 s1 s0 s1] 
            | CV_ElementCrossAddSub(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do 
                                                    yield sprintf "complex(%s.real-%s.imag,%s.imag+%s.real)" s0 s1 s0 s1] 
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | I_Load (r, e) -> 
            sprintf "%s%s;" indent ([for l, r in List.zip (translateRegister r) (translateComplexVExp e) do yield sprintf "%s=%s" l r] |> String.concat ";")
        | I_Store (addr, r) -> 
            sprintf "%s%s;" indent ([for l, r in List.zip (translateAddress addr r.Dimension) (translateRegister r) do yield sprintf "%s=%s" l r] |> String.concat ";")

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "def %s(%s):\n%s" func.Name (func.Parameters |> String.concat ",") (insStrs |> String.concat "\n")

let Translate2Cpp (func : Function) : string = 
    let translateRegister ({ Name=name; Dimension=d } : ComplexVRegister) : List<string> = 
        [for i in 0..d-1 do yield sprintf "%s_%d" name i] 
    let translateAddress ({ Base=b; Offset=off; } : Address) (dimension : int) : List<string> =
        [for i in 0..dimension-1 do yield sprintf "%s[%d]" b (off + i)] 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | F_Literal v -> sprintf "%.16f" v
            | F_Minus v -> sprintf "-%s" (translateFloatExp v)
            | F_Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | F_ReadReal (c, i) -> sprintf "%s_%d.real()" c.Name i
            | F_ReadImag (c, i) -> sprintf "%s_%d.imag()" c.Name i
    let translateComplexVExp (exp : ComplexVExpression) : List<string> =
        match exp with 
            | CV_New nums -> [for (r, i) in nums do yield sprintf "std::complex<double>(%s,%s)" (translateFloatExp r) (translateFloatExp i)]
            | CV_Variable (addr, d) -> translateAddress addr d
            | CV_Merge rs -> rs |> List.map translateRegister |> List.concat
            | CV_Minus r -> translateRegister r |> List.map (fun s->"-"+s) 
            | CV_Add (r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do yield s0+"+"+s1 ]
            | CV_Substract(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do yield s0+"-"+s1 ]
            | CV_ElementMultiply(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do 
                                                yield sprintf "std::complex<double>(%s.real()*%s.real(),%s.imag()*%s.imag())" s0 s1 s0 s1] 
            | CV_ElementCrossAddSub(r0, r1) -> [for s0, s1 in List.zip (translateRegister r0) (translateRegister r1) do 
                                                    yield sprintf "std::complex<double>(%s.real()-%s.imag(),%s.imag()+%s.real())" s0 s1 s0 s1] 
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | I_Load (r, e) -> 
            sprintf "%s%s;" indent ([for l, r in List.zip (translateRegister r) (translateComplexVExp e) do yield sprintf "auto %s=%s" l r] |> String.concat ";")
        | I_Store (addr, r) -> 
            sprintf "%s%s;" indent ([for l, r in List.zip (translateAddress addr r.Dimension) (translateRegister r) do yield sprintf "%s=%s" l r] |> String.concat ";")

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "static void %s(std::complex<double> *%s, std::complex<double> const *%s){\n%s\n}\n" 
        func.Name (func.Parameters.Item 0) (func.Parameters.Item 1) (insStrs |> String.concat "\n")

let Translate2CppWithSIMD (func : Function) : string = 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | F_Literal v -> sprintf "%.16f" v
            | F_Minus v -> sprintf "-%s" (translateFloatExp v)
            | F_Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | F_ReadReal ({Name=name;Dimension=1}, 0) -> sprintf "%s.m128d_f64[0]" name
            | F_ReadReal ({Name=name;Dimension=2}, i) -> sprintf "%s.m256d_f64[%d]" name (i * 2)
            | F_ReadReal _ -> assert(false); ""
            | F_ReadImag ({Name=name;Dimension=1}, 0) -> sprintf "%s.m128d_f64[1]" name
            | F_ReadImag ({Name=name;Dimension=2}, i) -> sprintf "%s.m256d_f64[%d+1]" name (i * 2)
            | F_ReadImag _ -> assert(false); ""
    let translateComplexVExp (exp : ComplexVExpression) : string =
        match exp with 
            | CV_New [(r,i)] when r = i -> sprintf "_mm_set1_pd(%s)" (translateFloatExp r)
            | CV_New [(r,i)] -> sprintf "_mm_set_pd(%s,%s)" (translateFloatExp i) (translateFloatExp r)
            | CV_New [(r0,i0);(r1,i1)] when r0 = i0 && r1 = i1 && r0 = r1 -> sprintf "_mm256_set1_pd(%s)" (translateFloatExp r0)
            | CV_New [(r0,i0);(r1,i1)] -> sprintf "_mm256_set_pd(%s,%s,%s,%s)" (translateFloatExp i1) (translateFloatExp r1) (translateFloatExp i0) (translateFloatExp r0)
            | CV_New _ -> assert(false); ""
            | CV_Variable ({Base=b;Offset=off}, 1) -> sprintf "_mm_load_pd(reinterpret_cast<double const*>(&%s[%d]))" b off
            | CV_Variable ({Base=b;Offset=off}, 2) -> sprintf "_mm256_load_pd(reinterpret_cast<double const*>(&%s[%d]))" b off
            | CV_Variable _ -> assert(false); ""
            | CV_Merge [{Name=name0;Dimension=1};{Name=name1;Dimension=1}] -> sprintf "_mm256_set_m128d(%s,%s)" name1 name0
            | CV_Merge _ -> assert(false); ""
            | CV_Minus {Name=name;Dimension=1} -> sprintf "_mm_sub_pd(_mm_set1_pd(0),%s)" name
            | CV_Minus {Name=name;Dimension=2} -> sprintf "_mm256_sub_pd(_mm256_set1_pd(0),%s)" name
            | CV_Minus _ -> assert(false); ""
            | CV_Add ({Name=name0;Dimension=1},{Name=name1;Dimension=1}) -> sprintf "_mm_add_pd(%s,%s)" name0 name1
            | CV_Add ({Name=name0;Dimension=2},{Name=name1;Dimension=2}) -> sprintf "_mm256_add_pd(%s,%s)" name0 name1
            | CV_Add _ -> assert(false); ""
            | CV_Substract ({Name=name0;Dimension=1},{Name=name1;Dimension=1}) -> sprintf "_mm_sub_pd(%s,%s)" name0 name1
            | CV_Substract ({Name=name0;Dimension=2},{Name=name1;Dimension=2}) -> sprintf "_mm256_sub_pd(%s,%s)" name0 name1
            | CV_Substract _ -> assert(false); ""
            | CV_ElementMultiply ({Name=name0;Dimension=1},{Name=name1;Dimension=1}) -> sprintf "_mm_mul_pd(%s,%s)" name0 name1
            | CV_ElementMultiply ({Name=name0;Dimension=2},{Name=name1;Dimension=2}) -> sprintf "_mm256_mul_pd(%s,%s)" name0 name1
            | CV_ElementMultiply _ -> assert(false); ""
            | CV_ElementCrossAddSub ({Name=name0;Dimension=1},{Name=name1;Dimension=1}) -> sprintf "_mm_addsub_pd(%s, _mm_shuffle_pd(%s,%s,1))" name0 name1 name1
            | CV_ElementCrossAddSub ({Name=name0;Dimension=2},{Name=name1;Dimension=2}) -> sprintf "_mm256_addsub_pd(%s, _mm256_shuffle_pd(%s,%s,5))" name0 name1 name1
            | CV_ElementCrossAddSub _ -> assert(false); ""
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | I_Load ({Name=name;Dimension=_}, e) -> sprintf "%sauto %s=%s;" indent name (translateComplexVExp e)
        | I_Store ({Base=b;Offset=off}, {Name=name;Dimension=1}) -> 
            sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%d]),%s);" indent b off name
        | I_Store ({Base=b;Offset=off}, {Name=name;Dimension=2}) -> 
            sprintf "%s_mm256_store_pd(reinterpret_cast<double*>(&%s[%d]),%s);" indent b off name
        | I_Store _ -> assert(false); ""

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "static void %s(std::complex<double> *%s, std::complex<double> const *%s){\n%s\n}\n" 
        func.Name (func.Parameters.Item 0) (func.Parameters.Item 1) (insStrs |> String.concat "\n")
//-------------------------------------------------------------------------------
let GenerateFFTCode (path : string) (sizes : List<int>) (translator : Function -> string) =
    System.IO.File.WriteAllLines(path,
        [for size in sizes do 
            let dlp = 2
            let gen = new FFTRadix2CodeGenerator()
            let fft = gen.ForwardGen size dlp
            let ifft = gen.BackwardGen size dlp
            yield sprintf "%s\n%s\n" (translator(fft)) (translator(ifft)) ]);


[<EntryPoint>]
let main argv =

   
    GenerateFFTCode "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h" [for i in 0..6 do yield 1<<<i] Translate2CppWithSIMD
    // GenerateFFTCode "C:\Programming\Projects\Github\DailyProjects\Python\BigInteger\FFT_gen.py" [for i in 0..6 do yield 1<<<i] Translate2Python

    0