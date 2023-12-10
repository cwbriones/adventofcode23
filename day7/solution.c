#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

static char *STANDARD = "23456789TJQKA";
static char *JOKER = "J23456789TQKA";

const size_t CARDS = 13;
const size_t HAND = 5;

void fatalf(const char * restrict msg, ...) {
    fprintf(stderr, "FATAL:\n");
    va_list arglist;
    va_start(arglist, msg);
    vprintf(msg, arglist);
    va_end(arglist);
    exit(1);
}

typedef enum {
    HIGH = 0,
    ONE_PAIR = 1,
    TWO_PAIR = 2,
    THREE_KIND = 3,
    FULL_HOUSE = 4,
    FOUR_KIND = 5,
    FIVE_KIND = 6,
} cardtype_t;

int card_index(const char c, const char* alphabet) {
    char *e = strchr(alphabet, c);
    if (e == NULL) {
        fatalf("unknown card char %c", c);
    }
    return (int)(e - alphabet);
}


int int_cmp(const void *p, const void *q) {
    int a = *(const int *)p;
    int b = *(const int *)q;
    if (a < b)
        return 1;
    if (b < a)
        return -1;
    return 0;
}

int encode(const char* hand, const char* alphabet) {
    int total = 0;
    for (int i = 0; hand[i] != 0; i++) {
        total = total*13 + card_index(hand[i], alphabet);
    }
    return total;
}

int get_type(const char* hand, const char* alphabet) {
    int counts[CARDS];
    for (int i = 0; i < CARDS; i++) {
        counts[i] = 0;
    }
    for (int i = 0; i < hand[i] != 0; i++) {
        int j = card_index(hand[i], alphabet);
        counts[j]++;
    }
    // FIXME seems dangerous
    if (alphabet[0] == 'J' && counts[0] != HAND && counts[0] > 0) {
        int maxi = 1;
        for (int j = 1; j < CARDS; j++) {
            if (counts[j] > counts[maxi]) {
                maxi = j;
            }
        }
        if (maxi > 0) {
            counts[maxi] += counts[0];
            counts[0] = 0;
        }
    }
    qsort(counts, CARDS, sizeof(int), int_cmp);

    switch (counts[0]) {
    case 5:
        return FIVE_KIND;
    case 4:
        return FOUR_KIND;
    case 3:
        return counts[1] == 2 ? FULL_HOUSE : THREE_KIND;
    case 2:
        return counts[1] == 2 ? TWO_PAIR : ONE_PAIR;
    default:
        return HIGH;
    }
}

typedef struct {
    void* data;
    size_t cap;
    size_t len;
    size_t elem_size;
} array_t;

void array_push(array_t* array, void* value) {
    if (array->len == array->cap) {
        array->cap = array->cap == 0 ? 64 : array->cap*2;
        array->data = realloc(array->data, array->elem_size*array->cap);
    }
    void* dest = array->data + array->elem_size*array->len;
    array->len++;

    memcpy(dest, value, array->elem_size);
}

void* array_get(array_t* array, size_t i) {
    return array->data + array->elem_size*i;
}

typedef struct {
    int encoded;
    cardtype_t type;
    int bid;
    char hand[6];
} entry_t;

int entry_cmp(const void *p, const void *q) {
    entry_t a = *(const entry_t *)p;
    entry_t b = *(const entry_t *)q;
    if (a.type < b.type)
        return -1;
    if (a.type > b.type)
        return 1;
    if (a.encoded < b.encoded) {
        return -1;
    }
    if (a.encoded > b.encoded) {
        return 1;
    }
    return 0;
}

int solve(array_t entries, const char* alphabet) {
    for (int i = 0; i < entries.len; i++) {
        entry_t* e = (entry_t*)(array_get(&entries, i));
        e->encoded = encode(e->hand, alphabet);
        e->type = get_type(e->hand, alphabet);
    }
    qsort(entries.data, entries.len, entries.elem_size, entry_cmp);
    int total = 0;
    for (int i = 0; i < entries.len; i++) {
        entry_t e = *(entry_t*)(array_get(&entries, i));
        total += (i+1) * e.bid;
    }
    return total;
}

int main() {
    // NOTE: Nothing malloc'd is ever actually freed here since
    // the program doesn't run too long and exits.
    //
    // Normally you would call free on every entry in the array
    // and then the array itself.
    array_t entries = {
        .elem_size = sizeof(entry_t),
    };
    char hand[6];
    int bid;
    int scanned;
    while ((scanned = scanf("%5s %d", hand, &bid)) != EOF) {
        if (scanned != 2) {
            fatalf("bad input, only scanned %d\n", scanned);
        }
        entry_t entry = {
            .bid = bid,
        };
        strcpy(entry.hand, hand);
        array_push(&entries, &entry);
    }
    printf("%d\n", solve(entries, STANDARD));
    printf("%d\n", solve(entries, JOKER));
    return 0;
}
