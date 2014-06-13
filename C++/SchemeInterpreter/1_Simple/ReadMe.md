######

1. This project is failed, because of the lack of considering about GC: 
    + Keep the temporary GC object on native stack is wrong, because we cant mask these object as GC root, and we can also not reach these temporary object from other roots, so these object will be leak from GC system

2. This project take long time to work, but it failed at last. Amount of time spent on engineering details such as the orignization of structures. 
    + Althouhg i have lot of experience on writing Scheme interpreter with Scheme, but this is the first time i work with C++
    + This failure remind me this truth again:
        Do it correct, then optimize !!!!

        (If i have wrote a buggy, slow, and ugly Scheme interpreter with C++ ever, i will not fail this time ...)
