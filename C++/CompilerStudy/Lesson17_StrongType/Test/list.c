struct Node
{
    int value;
    Node *next;
};
void* NULL;
Node* insertFront(int v, Node *next)
{
    Node *r = (Node*)malloc(sizeof(Node));
    r->value = v;
    r->next = next;
    return r;
}
Node* reverseList(Node *n)
{
    Node *r = (Node*)NULL;
    while (n != NULL) {
        Node *t = n;
        n = n->next;
        t->next = r;
        r = t;
    }
    return r;
}
void printList(Node* n)
{
    while (n != NULL) {
        printf("%d, ", n->value);
        n = n->next;
    }
    printf("\n");
}
int main()
{
    Node *l = insertFront(0, NULL);
    for (int i = 1; i < 10; ++i) {
        l = insertFront(i, l);
    }
    printList(l);
    l = reverseList(l);
    printList(l);
}
