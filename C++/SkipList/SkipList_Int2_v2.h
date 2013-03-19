
#ifndef SKIP_LIST_INT2_V2_H
#define SKIP_LIST_INT2_V2_H


struct SLNode_v2;
class SkipList_Int2_v2
{
public:
    SkipList_Int2_v2(int maxLevel);
    ~SkipList_Int2_v2();

    int size() const { return m_size; }
    bool empty() const { return size() == 0; }

    void set(int key, int value);
    int get(int key, int def = 0) const;
    void erase(int key);

    vector<pair<int, int> > toList() const;
private:
    SkipList_Int2_v2(SkipList_Int2_v2& o);
    SkipList_Int2_v2& operator = (const SkipList_Int2_v2& o);

    int find(int key, SLNode_v2* &n) const;
    pair<SLNode_v2*, int> insert(SLNode_v2 *head, int levelIdx, int key, int value);
private:
    int m_maxLevel, m_size;
    SLNode_v2 *m_head, *m_tail;
};

#endif
