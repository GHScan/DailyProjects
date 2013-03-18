
#include "pch.h"

#include <time.h>

#include <limits>

#include "SkipList_Int2.h"

SkipList_Int2::Node* SkipList_Int2::allocNode(int key, int value)
{
    Node* r = (Node*)malloc(sizeof(Node));
    r->next = r->prev = r->lower = NULL;
    r->key = key, r->value = value;
    return r;
}
void SkipList_Int2::freeNode(Node *n)
{
    free(n);
}

SkipList_Int2::SkipList_Int2():
    m_maxLevel(1), m_size(0)
{
    m_head = allocNode(numeric_limits<int>::min(), 0);
    m_tail = allocNode(numeric_limits<int>::max(), 0);
    m_head->next = m_tail;
    m_tail->prev = m_head; 
}
SkipList_Int2::~SkipList_Int2()
{
    while (m_maxLevel > 1) removeLevel();
}

void SkipList_Int2::set(int key, int value)
{
    insert(m_head, key, value);
    tryAddLevel();

    cout << "set:" << key << "," << value << endl;
    for (auto p : toList()) printf("(%d,%d),", p.first, p.second);
    puts("");
}
bool SkipList_Int2::insert(Node *head, int key, int value)
{
    Node *n = head;
    while (key > n->next->key) n = n->next;
    if (key == n->next->key) {
        while (n != NULL) {
            n->value = value;
            n = n->next;
        }
        return false;
    }
    else {
        if (n->lower == NULL) {
            Node *newN = allocNode(key, value);
            newN->next = n->next, newN->prev = n;
            n->next->prev = newN, n->next = newN;
            ++m_size;
            return true;
        }
        else {
            if (!insert(n->lower, key, value) || rand() < (RAND_MAX / 2)) return false;
            Node *newN = allocNode(key, value);
            newN->next = n->next, newN->prev = n;
            n->next->prev = newN, n->next = newN;
            return true;
        }
    }
}
int SkipList_Int2::get(int key, int def) const
{
    Node *n = m_head;
    for (;;) {
        while (key > n->next->key) n = n->next;
        if (key == n->next->key) return n->next->value;
        if (n->lower != NULL) n = n->lower;
        else return def;
    }
}
void SkipList_Int2::erase(int key)
{
    Node *n = m_head;
    for (;;) {
        while (key > n->next->key) n = n->next;
        if (key == n->next->key) break;
        if (n->lower != NULL) n = n->lower;
        else return;
    }
    --m_size;
    n = n->next;
    while (n != NULL) {
        Node *temp = n;
        n = n->lower;
        temp->prev->next = temp->next;
        temp->next->prev = temp->prev;
        freeNode(temp);
    }

    cout << "erase:" << key << "," << endl;
    for (auto p : toList()) printf("(%d,%d),", p.first, p.second);
    puts("");
}

void SkipList_Int2::tryRemoveLevel()
{
    if (m_size < (1 << (m_maxLevel - 1))) removeLevel();
}
void SkipList_Int2::tryAddLevel()
{
    if (m_size > (1 << (m_maxLevel))) addLevel();
}
void SkipList_Int2::addLevel()
{
    Node *newHead = allocNode(numeric_limits<int>::min(), 0);
    newHead->lower = m_head;

    Node *n = m_head->next, *n2 = newHead;
    while (n != m_tail) {
        if (rand() < (RAND_MAX / 2)) {
            Node *newN = allocNode(n->key, n->value);
            newN->lower = n;
            n2->next = newN;
            newN->prev = n2;
            n2 = newN;
        }
        n = n->next;
    }
    Node *newTail = allocNode(numeric_limits<int>::max(), 0);
    n2->next = newTail;
    newTail->prev = n2, newTail->lower = m_tail;
    m_head = newHead, m_tail = newTail;
    ++m_maxLevel;
}
void SkipList_Int2::removeLevel()
{
    --m_maxLevel;
    Node *n = m_head;
    m_head = m_head->lower, m_tail = m_tail->lower;
    while (n != NULL) {
        Node *temp = n;
        n = n->next;
        freeNode(temp);
    }
}

vector<pair<int, int> > SkipList_Int2::toList() const
{
    vector<pair<int, int> > r;
    Node *head = m_head;
    while (head->lower != NULL) head = head->lower;
    Node *n = head->next;
    while (n->next != NULL) {
        r.push_back(make_pair(n->key, n->value));
        n = n->next;
    }
    return r;
}
