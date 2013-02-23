
#ifndef FIXSIZEALLOCATOR_H
#define FIXSIZEALLOCATOR_H

class FixsizeAllocator
{
public:
    FixsizeAllocator();
    ~FixsizeAllocator();
    static FixsizeAllocator* instance()
    {
        static FixsizeAllocator s_ins;
        return &s_ins;
    }

    void* malloc(int size);
    void free(void *p);
private:
    vector<int> m_sizeNs;
    vector<class SizeNAllocator*> m_sizeNAllocators;
};

#endif
