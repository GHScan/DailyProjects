
#ifndef BITVECTOR_ALLOCATOR_H
#define BITVECTOR_ALLOCATOR_H

class BitVectorAllocator
{
public:
    BitVectorAllocator();
    ~BitVectorAllocator();
    static BitVectorAllocator* instance()
    {
        static BitVectorAllocator s_ins;
        return &s_ins;
    }

    void* alloc(int size);
    void free(void *p);
private:
    vector<int> m_sizeNs;
    vector<class SizeNBitVectorAllocator*> m_sizeNAllocators;
};


#endif
