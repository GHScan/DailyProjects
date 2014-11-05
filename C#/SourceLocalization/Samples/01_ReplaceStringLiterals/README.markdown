### 说明

+ 运行`Replace.Bat`，它以`Replace.json`作为参数调用`LitearlReplace.exe`，后者会执行3步操作:
    1. `Command_FindoutStringLiterals`: 找出源码中的字符串字面值，汇总到`Filter.txt`中
    2. `Command_ReplaceStringLiteralsWithVariables`:  将源码中的字面值引用，替换成变量，并生成`Localization.json`
    3. `Command_GenerateClassFile`: 根据`Localization.json`生成被源码引用的文件`Localization.cs`

+ 关于维护
    + 上面的3步中，1、2步，只执行一次，而第3步，需要在开发过程中，通过维护`Localization.json`(添加新变量、做多语言翻译)，经常生成`Localization.cs`

