#include "pch.h"

#include <initializer_list>
#include <functional>

struct Node {
    int value;
    Node *next;
    Node(int i, Node *n): value(i), next(n) {}
};
void list_appends(Node ** p, initializer_list<int> &&list) {
    for (auto i : list) {
        *p = new Node(i, *p);
    }
}
void list_removeIf(Node ** p, function<bool(int)> f) {
    while (*p) {
        if (f((*p)->value)) {
            Node *t = *p;
            *p = t->next;
            delete t;
        } else {
            p = &(*p)->next;
        }
    }
}
void list_print(Node *p) {
    for (; p; p = p->next) {
        printf("%d,", p->value);
    }
    puts("");
}

int main() {
    Node *p = nullptr;
    list_appends(&p, {1, 2, 3, 4});
    list_appends(&p, {5, 6, 7});
    list_print(p);
    list_removeIf(&p, [](int i){return i%2;});
    list_print(p);
    list_removeIf(&p, [](int i){return i%3;});
    list_print(p);

    puts("finish");
}
