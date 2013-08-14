
char *read_line() {
    static char buf[32];
    gets(buf);
    return buf;
}
void explode_bomb() {
    puts("BOOM!!!");
    puts("The bomb has blown up");
    exit(8);
}

int string_not_equal(const char *s1, const cahr *s2) {
    return strcmp(s1, s2);
}

void read_six_numbers(const char *buf, int a[6]) {
    if (sscanf(buf, "%d %d %d %d %d %d", a, a + 1, a + 2, a + 3, a + 4, a + 5) > 5) {
        return;
    }
    explode_bomb();
}

void func4(int a) {
    if (a <= 1) return 1;
    return func4(a - 1) + func4(a - 2);
}

void phase_1(const char *input) {
    if (!string_not_equal(input, "Public speaking is very easy.")) {
        explode_bomb();
    }
}

void phase_2(const char *input) {
    int a[6];
    read_six_numbers(input, a);
    // assert a[0..5] == 1 2 6 24 120 720
    if (a[0] != 1) explode_bomb();
    for (int i = 1; i <= 5; ++i) {
        int v = (i + 1) * a[i - 1];
        if (v != a[i]) explode_bomb();
    } 
}

void phase_3(const char *input) {
    int a;
    char b;
    int c;
    if (sscanf(input, "%d %c %d", &c, &b, &a) < 3) explode_bomb();
    char cref;
    switch (c) {
        case 0: // 0x8048be0
            cref = 'q';
            if (a == 777) break;
            explode_bomb();
            break;
        case 1: // 0x8048c00
            cref = 'b';
            if (a == 214) break;
            explode_bomb();
            break;
        case 2: // 0x8048c16
            cref = 'b';
            if (a == 755) break;
            explode_bomb();
            break;
        case 3: // 0x8048c28
            cref = 'k';
            if (a == 251) break;
            explode_bomb();
            break;
        case 4: // 0x8048c40
            cref = 'o';
            if (a == 160) break;
            explode_bomb();
            break;
        case 5: // 0x8048c52
            cref = 't';
            if (a == 458) break;
            explode_bomb();
            break;
        case 6: // 0x8048c64
            cref = 'v';
            if (a == 780) break;
            explode_bomb();
            break;
        case 7: // 0x8048c76
            cref = 'b';
            if (a == 524) break;
            explode_bomb();
            break;
        default: 
            cref = 'x';
            explode_bomb();
    } 
    // 0x8048c8f
    if (b != cref) explode_bomb();
}

void phase_4(const char *input) {
    int a;
    if (sscanf(input, "%d", &a) != 1 || a <= 0) {
        explode_bomb();
    }
    // 1..-> 1 2 3 5 8 13 21 34 55
    // so, assert a == 9
    if (func4(a) != 55) {
        explode_bomb();
    }
}

#define STR_5 "isrveawhobpnutfg\xb0\x01"
void phase_5(const char *input) {
    char buf[8];
    if (strlen(input) != 6) explode_bomb();
    for (int i = 0; i <= 5; ++i) {
        buf[i] = STR_5[input[i] & 0xf];
    }
    buf[6] = buf[7] = 0;
    // assert input == ?f ?0 ?5 ?b ?d ?1
    if (string_not_equal(buf, "giants")) explode_bomb();
}

struct Node {int v[2]; Node *next;};
Node* node1;
void phase_6(const char *input) {
    int steps[6]; 
    Node *nodes[6]; 
    Node *head = node1;
    // steps should be node idx which make the node value is in descent order
    // 0xfd 0x2d5 0x12e 0x3e5 0xd4 0x1b0
    // so, assert steps == 4 2 6 3 1 5
    read_six_numbers(input, steps);
    for (int i = 0; i <= 5; ++i) {
        if (steps[i] > 6) explode_bomb();
        for (int j = i + 1; j <= 5; ++j) {
            if (steps[i] == steps[j]) explode_bomb();
        }
    }

    for (int i = 0; i <= 5; ++i) {
        Node *t = head;
        for (int j = 1; j < steps[i]; ++j) {
            t = t->next;
        }
        nodes[i] = t;
    }

    Node *cur = nodes[0];
    head = cur;
    for (int i = 1; i <= 5; ++i) {
        cur->next = nodes[i];
        cur = nodes[i];
    }
    cur->next = NULL;

    cur = head;
    for (int i = 0; i <= 4; ++i) {
        if (cur->v[0] < cur->next->v[0]) explode_bomb();
        cur = cur->next;
    }
}
