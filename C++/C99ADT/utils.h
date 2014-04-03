
#ifndef UTILS_H
#define UTILS_H

#undef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#undef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0]))
#define ARRAY_INSERT(a, n, idx, v) { for (int i = n; i > idx; --i) a[i] = a[i - 1]; a[idx] = v; ++n; }
#define ARRAY_REMOVE(a, n, idx) { for (int i = idx; i < n - 1; ++i) a[i] = a[i + 1]; --n; }
#define ARRAY_FIND(r, a, n, v) { r = -1; for (int i = 0; i < n; ++i) if (a[i] == v) { r = i; break; }  }
 
#endif
