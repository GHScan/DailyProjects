
#ifndef SKIP_LIST_ARRAY_H
#define SKIP_LIST_ARRAY_H

struct SLANode;
class SkipListArray
{
public:
    SkipListArray(int maxLevel);
    ~SkipListArray();

    void insert(int off, int value);
    void erase(int off);
    int operator [] (int off) const;
    int& operator [](int off);

    vector<int> toList() const;

    int size() const { return m_size; }
    bool empty() const { return size() == 0; }
private:
    SkipListArray(const SkipListArray& o);
    SkipListArray& operator = (const SkipListArray& o);

    SLANode* index(int off) const;
    SLANode* erase(SLANode *head, int levelIdx, int off);
    pair<SLANode*, int> insert(SLANode *head, int levelIdx, int off, int value);
private:
    int m_maxLevel, m_size;
    SLANode *m_head, *m_tail;
};

#endif
