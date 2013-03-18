
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

    vector<pair<int, int> > toList() const;
private:
    struct Node
    {
        Node *prev, *next, *lower;
        int key, value;
    };
private:
    SkipList_Int2(const SkipList_Int2& o);
    SkipList_Int2& operator = (const SkipList_Int2& o);

    void tryRemoveLevel();
    void tryAddLevel();
    void addLevel();
    void removeLevel();
    bool insert(Node *head, int key, int value);

    static Node* allocNode(int key, int value);
    static void freeNode(Node *n);
private:
    int m_maxLevel;
    int m_size;
    Node *m_head, *m_tail;
};

#endif
