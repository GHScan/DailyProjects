
#ifndef SKIP_LIST_H
#define SKIP_LIST_H

class SkipList_Int2
{
public:
    SkipList_Int2();
    ~SkipList_Int2();

    bool empty() const { return size() == 0; }
    int size() const { return m_size; }

    void set(int key, int value);
    int get(int key, int def = 0) const;
    void erase(int key);

private:
    SkipList_Int2(const SkipList_Int2& o);
    SkipList_Int2& operator = (const SkipList_Int2& o);

private:
    struct Node
    {
        Node *prev, *next, *lower;
        int key, value;
    };
private:
    int m_maxLevel;
    int m_size;
    Node *m_head, *m_tail;
};

#endif
