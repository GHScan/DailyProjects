#include "pch.h" 

#include "Base.h"
#include "List.h"
#include "Interpreter.h"

#define PRINT(e) cout << #e << " ==> " << e << endl

static void testList() {
    PRINT((Length<IntList<1, 3, 5>>::value));
    PRINT((Sum<Append<IntList<1, 2>, IntList<3, 4>>>::value));
    PRINT((Sum<Map<IntList<1, 2, 3, 4>, Inc>>::value));
    PRINT((Lookup<Zip<IntList<1, 2, 3>, IntList<1, 4, 9>>, Const<2>>::value));
    PRINT((Sum<Filter<IntList<1, 2, 3, 4>, Odd>>::value));

    Foreach<Range<1, 10>>::apply([](int i){cout << i << ',';});
    cout << endl;
}

static void testInterpreter() {
    using namespace Implicits;

    PRINT((Eval<
                APP<add, IntList<2, 3>>, G>::value));

    PRINT((Eval<
                APP<
                    LAM<List<x, y>, 
                        APP<add, List<x, y>>>,
                    IntList<2, 3>>,
                G>::value));

    PRINT((Eval<Branch<APP<lt, IntList<3, 2>>, Const<2>, Const<3>>,
                G>::value));

    PRINT((Eval<
            APP<
                APP<
                    LAM<List<f>,
                        APP<
                        LAM<List<g>, APP<g, List<g>>>,
                        List<LAM<List<g>,
                                APP<f, List<
                                        LAM<List<arg>, 
                                            APP<APP<g, List<g>>, List<arg>>>>>>>>>,
                    List<LAM<List<f>, 
                            LAM<List<x>, 
                                Branch<APP<lt, List<x, Const<2>>>,
                                    Const<1>,
                                    APP<add, List<APP<f, List<APP<sub, List<x, Const<1>>>>>,
                                                  APP<f, List<APP<sub, List<x, Const<2>>>>>>>>>>>>,
                IntList<10>>,
            G>::value));
}

int main() {
    testList();
    testInterpreter();
}
