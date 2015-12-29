/* erlmunk_space.h */

typedef struct {
    int id;
    cpBB bb;
    ETERM *from;

    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_subscriber;

typedef struct {
    int id;
    cpBody *body;

    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_body;

typedef struct {
    int id;
    cpSpace *space;
    erlmunk_body *bodies;

    int subscriber_count;
    erlmunk_subscriber *subscribers;

    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_space;

ETERM *space_new(ETERM *fromp, ETERM *argp);
ETERM *space_add_body(ETERM *fromp, ETERM *argp);
ETERM *body_set_position(ETERM *fromp, ETERM *argp);
ETERM *body_set_angle(ETERM *fromp, ETERM *argp);

ETERM *space_subscribe_box(ETERM *fromp, ETERM *argp);

void spacesStep(float dt);
