### 演示为Localization.json添加多语言支持

1. 根据`Localization.json`生成`Localization.xls`
2. 将`Localization.xls`交给第3方做翻译，这期间，该xls会和我们手中的`Localization.json`不一致。为模拟这种不一致，执行下面的操作
    + 在xls中添加`en_us`列，作为英语文本列
    + 从xls中删除多行，反过来说，相当于开发组维护的`Localization.json`，增加了新的行(对应xls中被删除的行)
3. 根据`Localization.xls`生成`Localization2.json`，相当于翻译将增加了英文翻译的xls发给我们，我们准备做合并
4. 将`Localization2.json`合并到`Localization.json`中
5. 根据最新的`Localization.json`生成最新的、英文的`Localization.cs`
