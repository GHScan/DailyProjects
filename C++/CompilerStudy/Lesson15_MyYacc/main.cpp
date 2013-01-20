
#include "pch.h"

#include "UnitTest.h"

int main()
{
    try
    {
        unitTest();
    }
    catch (const exception& e)
    {
        cout << "Exception : " << e.what() << endl;
    }
}
