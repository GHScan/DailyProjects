module Program


open System


//-------------------------------------------------------------------------------
type Address = { Base : string; Offset : int }
type Complex = System.Numerics.Complex
type ComplexRegister = { Name : string }
type FloatExpression  = 
    | Literal of Value : double
    | Minus of Value : FloatExpression
    | Multiply of Operand1 : FloatExpression * Operand2 : FloatExpression
    | ReadReal of Value : ComplexRegister
    | ReadImag of Value : ComplexRegister
type ComplexExpression = 
    | NewComplex of Real : FloatExpression * Imag : FloatExpression
    | Variable of Address : Address
    | Minus of Value : ComplexRegister
    | Add of Operand1 : ComplexRegister * Operand2 : ComplexRegister
    | Substract of Operand1 : ComplexRegister * Operand2 : ComplexRegister
    | ElementMultiply of Operand1 : ComplexRegister * Operand2 : ComplexRegister
    | ElementCrossAddSub of Operand1 : ComplexRegister * Operand2 : ComplexRegister
type Instruction = 
    | Load of Register : ComplexRegister * Expression : ComplexExpression
    | Store of Address : Address * Register : ComplexRegister
type Function = { Name : string; Parameters : List<string>; Body : List<Instruction> }


//-------------------------------------------------------------------------------
module Utility =

    let FloatEquals (f0 : double) (f1 : double) : bool =
        Math.Abs(f0 - f1) < 0.000001

    let ComplexEquals (c0 : Complex) (c1 : Complex) : bool =
         FloatEquals c0.Real c1.Real && FloatEquals c0.Imaginary c1.Imaginary

    let TwiddleFactor (n : int) (e : int) : Complex = 
        Complex.FromPolarCoordinates(1.0, 2.0 * Math.PI * (double e) / (double n))

    let rec Zip5 (a0 : List<'T0>) (a1 : List<'T1>) (a2 : List<'T2>) (a3 : List<'T3>) (a4 : List<'T4>) : List<'T0 * 'T1 * 'T2 * 'T3 * 'T4> = 
        if a0.IsEmpty then
            []
        else
            (a0.Head, a1.Head, a2.Head, a3.Head, a4.Head) :: Zip5 a0.Tail a1.Tail a2.Tail a3.Tail a4.Tail


//-------------------------------------------------------------------------------
[< AbstractClass >]
type FFTCodeGenerator() = class
    let mutable body : List<Instruction> = List.empty
    let mutable exp2Register : Map<ComplexExpression, ComplexRegister> = Map.empty
    let mutable registerId = 0

    member private this.Reset() =
        body <- List.empty
        exp2Register <- Map.empty
        registerId <- 0

    member private this.NewRegisterName () : string = 
        registerId <- registerId + 1
        sprintf "temp_%d" registerId

    member private this.LoadComplex (exp : ComplexExpression) : ComplexRegister = 
        let reg = exp2Register |> Map.tryFind exp
        if Option.isSome reg then
            Option.get reg
        else
            let reg : ComplexRegister = { Name = this.NewRegisterName() }
            exp2Register <- exp2Register.Add(exp, reg)
            body <- Load (reg, exp) :: body
            reg

    member private this.StoreComplex (addr : Address) (r : ComplexRegister) =
        body <- (Store (addr, r)) :: body

    member this.LoadVariable (addr : Address) : ComplexRegister =
        this.LoadComplex (Variable addr)

    member this.StoreVariable (addr : Address) (r : ComplexRegister) =
        this.StoreComplex addr r

    member this.Multiply (r : ComplexRegister) (c : Complex) : ComplexRegister = 
        if Utility.ComplexEquals c Complex.One then
            r
        else if Utility.ComplexEquals -c Complex.One then
            this.LoadComplex (Minus r)
        else if Utility.ComplexEquals c Complex.ImaginaryOne then
            this.LoadComplex (NewComplex (FloatExpression.Minus (ReadImag r), (ReadReal r)))
        else if Utility.ComplexEquals -c Complex.ImaginaryOne then
            this.LoadComplex (NewComplex ((ReadImag r), (FloatExpression.Minus (ReadReal r))))
        else if Utility.FloatEquals c.Real 0.0 then
            let newReal = FloatExpression.Minus (FloatExpression.Multiply (Literal c.Imaginary, ReadImag r))
            let newImag = FloatExpression.Multiply (Literal c.Imaginary, ReadReal r)
            this.LoadComplex (NewComplex (newReal, newImag))
        else if Utility.FloatEquals c.Imaginary 0.0 then
            let literal = this.LoadComplex (NewComplex (Literal c.Real, Literal c.Real))
            this.LoadComplex (ElementMultiply (r, literal))
        else 
            let reals = this.LoadComplex (NewComplex (Literal c.Real, Literal c.Real))
            let imags = this.LoadComplex (NewComplex (Literal c.Imaginary, Literal c.Imaginary))
            let rTimesReal = this.LoadComplex (ElementMultiply (r, reals))
            let rTimesImags = this.LoadComplex (ElementMultiply (r, imags))
            this.LoadComplex (ElementCrossAddSub (rTimesReal, rTimesImags))

    member this.Add (a : ComplexRegister) (b : ComplexRegister) : ComplexRegister =
        this.LoadComplex (Add (a, b))

    member this.Substract (a : ComplexRegister) (b : ComplexRegister) : ComplexRegister = 
        this.LoadComplex (Substract (a, b))

    member this.Forward (size : int) : Function =
        this.Reset()
        
        let srcName, destName = "src", "dest"
        let input = [for i in 0..size-1 do yield { Base = srcName; Offset = i }]
        let output = this.RecursiveFFT input 1 1.0
        for i in 0..output.Length-1 do
            this.StoreVariable { Base = destName; Offset = i} (output.Item i)
        { Name = "FFT_" + size.ToString(); Parameters = [destName; srcName]; Body = List.rev body; }

    member this.Backward (size : int) : Function =
        this.Reset()

        let srcName, destName = "src", "dest"
        let input = [for i in 0..size-1 do yield { Base = srcName; Offset = i }]
        let output = this.RecursiveFFT input -1 (1.0 / (double size))
        for i in 0..output.Length-1 do
            this.StoreVariable { Base = destName; Offset = i} (output.Item i)
        { Name = "IFFT_" + size.ToString(); Parameters = [destName; srcName]; Body = List.rev body; }

    member this.RecursiveFFT1 (a : List<Address>) (flag : int) (scale : double) : List<ComplexRegister> =
        assert (a.Length = 1)
        [ this.Multiply (this.LoadVariable (a.Item 0)) (Complex(scale, 0.0)) ]

    member this.RecursiveFFT2 (a : List<Address>) (flag : int) (scale : double) : List<ComplexRegister> =
        assert (a.Length = 2)
        let c1 = this.RecursiveFFT1 [a.Item 0] flag scale |> List.head
        let c2 = this.RecursiveFFT1 [a.Item 1] flag scale |> List.head
        [this.Add c1 c2; this.Substract c1 c2]

    abstract member RecursiveFFT : List<Address> -> int -> double -> List<ComplexRegister>
 end

type FFTRadix2CodeGenerator() = class
    inherit FFTCodeGenerator()

    override this.RecursiveFFT (a : List<Address>) (flag : int) (scale : double) : List<ComplexRegister> =
        if a.Length = 1 then
            this.RecursiveFFT1 a flag scale
        else
            let a0, a1 = List.foldBack (fun v (l,r)-> (v::r,l)) a ([],[])
            let b0 = this.RecursiveFFT a0 flag scale
            let b1 = this.RecursiveFFT a1 flag scale
            let factors = [for e in 0..b0.Length-1 do yield Utility.TwiddleFactor a.Length (e * flag) ]

            let mutable o0, o1 = List.empty, List.empty

            for c0, c1, f in List.zip3 b0 b1 factors do
                let c1f = this.Multiply c1 f
                o0 <- (this.Add c0 c1f) :: o0
                o1 <- (this.Substract c0 c1f) :: o1

            List.concat [List.rev o0; List.rev o1]
end

type FFTRadix4CodeGenerator() = class
    inherit FFTCodeGenerator()

    override this.RecursiveFFT (a : List<Address>) (flag : int) (scale : double) : List<ComplexRegister> =
        if a.Length = 1 then
            this.RecursiveFFT1 a flag scale
        else if a.Length = 2 then
            this.RecursiveFFT2 a flag scale
        else
            let a0, a1, a2, a3 = List.foldBack (fun v (l0,l1,l2,l3)-> (v::l3,l0,l1,l2)) a ([],[],[],[])
            let b0 = this.RecursiveFFT a0 flag scale
            let b1 = this.RecursiveFFT a1 flag scale
            let b2 = this.RecursiveFFT a2 flag scale
            let b3 = this.RecursiveFFT a3 flag scale
            let factors = [for e in 0..b0.Length-1 do yield Utility.TwiddleFactor a.Length (e * flag) ]

            let mutable o0, o1, o2, o3 = List.empty, List.empty, List.empty, List.empty

            for c0, c1, c2, c3, f in Utility.Zip5 b0 b1 b2 b3 factors do
                let c1f = this.Multiply c1 f
                let c3fff = this.Multiply c3 (Complex.Pow(f, 3.0))
                let c1f_plus_c3fff = this.Add c1f c3fff
                let c2ff = this.Multiply c2 (Complex.Pow(f, 2.0))
                let c0_plus_c2ff = this.Add c0 c2ff
                o0 <- (this.Add c0_plus_c2ff c1f_plus_c3fff) :: o0 
                o2 <- (this.Substract c0_plus_c2ff c1f_plus_c3fff) :: o2
                let fc1f = this.Multiply c1 (Complex.Pow(f, 1.0) * Complex.ImaginaryOne * (Complex (double flag, 0.0)))
                let fc3fff = this.Multiply c3 (Complex.Pow(f, 3.0) * Complex.ImaginaryOne * (Complex (double flag, 0.0)))
                let fc1f_minus_fc3fff = this.Substract fc1f fc3fff
                let c0_minus_c2ff = this.Substract c0 c2ff
                o1 <- (this.Add c0_minus_c2ff fc1f_minus_fc3fff) :: o1 
                o3 <- (this.Substract c0_minus_c2ff fc1f_minus_fc3fff) :: o3

            List.concat [List.rev o0; List.rev o1; List.rev o2; List.rev o3]

end

type FFTSplitRadixCodeGenerator() = class
    inherit FFTCodeGenerator()

    override this.RecursiveFFT (a : List<Address>) (flag : int) (scale : double) : List<ComplexRegister> =
        if a.Length = 1 then
            this.RecursiveFFT1 a flag scale
        else if a.Length = 2 then
            this.RecursiveFFT2 a flag scale
        else
            let a0, a13 = List.foldBack (fun v (l,r)-> (v::r,l)) a ([],[])
            let a1, a3 = List.foldBack (fun v (l,r)-> (v::r,l)) a13 ([],[])

            let b0 = this.RecursiveFFT a0 flag scale
            let b1 = this.RecursiveFFT a1 flag scale
            let b3 = this.RecursiveFFT a3 flag scale
            let factors = [for e in 0..b1.Length-1 do yield Utility.TwiddleFactor a.Length (e * flag) ]
            let b00, b01 = (List.take b1.Length b0), (List.skip b1.Length b0)

            let mutable o0, o1, o2, o3 = List.empty, List.empty, List.empty, List.empty

            for c00, c01, c1, c2, f in Utility.Zip5 b00 b01 b1 b3 factors do
                let c1f = this.Multiply c1 f
                let c2fff = this.Multiply c2 (Complex.Pow(f, 3.0))
                let c1f_plus_c2fff = this.Add c1f c2fff
                o0 <- (this.Add c00 c1f_plus_c2fff) :: o0
                o2 <- (this.Substract c00 c1f_plus_c2fff) :: o2
                let fc1f = this.Multiply c1 (Complex.Pow(f, 1.0) * Complex.ImaginaryOne * (Complex (double flag, 0.0)))
                let fc2fff = this.Multiply c2 (Complex.Pow(f, 3.0) * Complex.ImaginaryOne * (Complex (double flag, 0.0)))
                let fc1f_minus_fc2fff = this.Substract fc1f fc2fff
                o1 <- (this.Add c01 fc1f_minus_fc2fff) :: o1
                o3 <- (this.Substract c01 fc1f_minus_fc2fff) :: o3

            List.concat [List.rev o0; List.rev o1; List.rev o2; List.rev o3]
end


//-------------------------------------------------------------------------------
let Translate2Python (func : Function) : string = 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | Literal v -> sprintf "%.16f" v
            | FloatExpression.Minus v -> sprintf "-%s" (translateFloatExp v)
            | FloatExpression.Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | ReadReal c -> sprintf "%s.real" c.Name
            | ReadImag c -> sprintf "%s.imag" c.Name
    let translateComplexExp (exp : ComplexExpression) : string =
        match exp with 
            | NewComplex (real, imag) -> sprintf "complex(%s,%s)" (translateFloatExp real) (translateFloatExp imag)
            | Variable addr -> sprintf "%s[%d]" addr.Base addr.Offset
            | Minus v -> sprintf "-%s" v.Name
            | Add (op1, op2) -> sprintf "%s+%s" op1.Name op2.Name
            | Substract (op1, op2) -> sprintf "%s-%s" op1.Name op2.Name
            | ElementMultiply (op1, op2) -> sprintf "complex(%s.real*%s.real,%s.imag*%s.imag)" op1.Name op2.Name op1.Name op2.Name
            | ElementCrossAddSub (op1, op2) -> sprintf "complex(%s.real-%s.imag,%s.imag+%s.real)" op1.Name op2.Name op1.Name op2.Name
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | Load (r, c) -> sprintf "%s%s=%s;" indent r.Name (translateComplexExp c)
        | Store (addr, r) -> sprintf "%s%s[%d]=%s;" indent addr.Base addr.Offset r.Name

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "def %s(%s):\n%s" func.Name (func.Parameters |> String.concat ",") (insStrs |> String.concat "\n")

let Translate2Cpp (func : Function) : string = 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | Literal v -> sprintf "%.16f" v
            | FloatExpression.Minus v -> sprintf "-%s" (translateFloatExp v)
            | FloatExpression.Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | ReadReal c -> sprintf "%s.real()" c.Name
            | ReadImag c -> sprintf "%s.imag()" c.Name
    let translateComplexExp (exp : ComplexExpression) : string =
        match exp with 
            | NewComplex (real, imag) -> sprintf "std::complex<double>(%s,%s)" (translateFloatExp real) (translateFloatExp imag)
            | Variable addr -> sprintf "%s[%d]" addr.Base addr.Offset
            | Minus v -> sprintf "-%s" v.Name
            | Add (op1, op2) -> sprintf "%s+%s" op1.Name op2.Name
            | Substract (op1, op2) -> sprintf "%s-%s" op1.Name op2.Name
            | ElementMultiply (op1, op2) -> sprintf "std::complex<double>(%s.real()*%s.real(),%s.imag()*%s.imag())" op1.Name op2.Name op1.Name op2.Name
            | ElementCrossAddSub (op1, op2) -> sprintf "std::complex<double>(%s.real()-%s.imag(),%s.imag()+%s.real())" op1.Name op2.Name op1.Name op2.Name
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | Load (r, c) -> sprintf "%sauto %s=%s;" indent r.Name (translateComplexExp c)
        | Store (addr, r) -> sprintf "%s%s[%d]=%s;" indent addr.Base addr.Offset r.Name

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "static void %s(std::complex<double> * %s, std::complex<double> const *%s){\n%s\n}" func.Name (func.Parameters.Item 0) (func.Parameters.Item 1) (insStrs |> String.concat "\n")

let Translate2CppWithSIMD (func : Function) : string = 
    let rec translateFloatExp (exp : FloatExpression) : string =
        match exp with 
            | Literal v -> sprintf "%.16f" v
            | FloatExpression.Minus v -> sprintf "-%s" (translateFloatExp v)
            | FloatExpression.Multiply (op1, op2) -> sprintf "%s*%s" (translateFloatExp op1) (translateFloatExp op2)
            | ReadReal c -> sprintf "%s.m128d_f64[0]" c.Name
            | ReadImag c -> sprintf "%s.m128d_f64[1]" c.Name
    let translateComplexExp (exp : ComplexExpression) : string =
        match exp with 
            | NewComplex (real, imag) -> 
                if real = imag then
                    sprintf "_mm_set1_pd(%s)" (translateFloatExp real)
                else
                    sprintf "_mm_set_pd(%s,%s)" (translateFloatExp imag) (translateFloatExp real)
            | Variable addr -> sprintf "_mm_load_pd(reinterpret_cast<double const*>(&%s[%d]))" addr.Base addr.Offset
            | Minus v -> sprintf "_mm_sub_pd(_mm_set1_pd(0.0), %s)" v.Name
            | Add (op1, op2) -> sprintf "_mm_add_pd(%s,%s)" op1.Name op2.Name
            | Substract (op1, op2) -> sprintf "_mm_sub_pd(%s,%s)" op1.Name op2.Name
            | ElementMultiply (op1, op2) -> sprintf "_mm_mul_pd(%s, %s)" op1.Name op2.Name
            | ElementCrossAddSub (op1, op2) -> sprintf "_mm_addsub_pd(%s, _mm_shuffle_pd(%s,%s,1))" op1.Name op2.Name op2.Name
    let translateIns (ins : Instruction) (indent : string) : string =
        match ins  with
        | Load (r, c) -> sprintf "%sauto %s=%s;" indent r.Name (translateComplexExp c)
        | Store (addr, r) -> sprintf "%s_mm_store_pd(reinterpret_cast<double*>(&%s[%d]),%s);" indent addr.Base addr.Offset r.Name

    let insStrs = [for ins in func.Body do yield translateIns ins "\t"]
    sprintf "static void %s(std::complex<double> * %s, std::complex<double> const *%s){\n%s\n}" func.Name (func.Parameters.Item 0) (func.Parameters.Item 1) (insStrs |> String.concat "\n")


//-------------------------------------------------------------------------------
let GenerateFFTCode (path : string) (sizes : List<int>) (translator : Function -> string) =
    System.IO.File.WriteAllLines(path,
        [for size in sizes do 
            let gen = new FFTRadix2CodeGenerator()
            let fft = gen.Forward size
            let ifft = gen.Backward size
            yield sprintf "%s\n%s\n" (translator(fft)) (translator(ifft)) ]);


[<EntryPoint>]
let main argv =
   
    GenerateFFTCode "C:\Programming\Projects\Cpp2015\Cpp2015\FFT_gen.h" [for i in 0..6 do yield 1<<<i] Translate2CppWithSIMD
    // GenerateFFTCode "C:\Programming\Projects\Github\DailyProjects\Python\BigInteger\FFT_gen.py" [for i in 0..6 do yield 1<<<i] Translate2Python

    0