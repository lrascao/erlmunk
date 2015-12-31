/* erlmunk_space.h */

typedef struct {
    int id;
    cpBB bb;
    erlmunk_client *client;
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
    ETERM *term;
} erlmunk_body_data;

typedef struct {
    int id;
    cpSpace *space;
    erlmunk_body *bodies;

    int subscriber_count;
    erlmunk_subscriber *subscribers;

    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_space;

ETERM *space_new(ETERM *fromp, ETERM *argp);
ETERM *space_delete(ETERM *fromp, ETERM *argp);
ETERM *space_add_body(ETERM *fromp, ETERM *argp);
ETERM *space_remove_body(ETERM *fromp, ETERM *argp);
ETERM *space_subscribe_collision(ETERM *fromp, ETERM *argp);
ETERM *body_activate(ETERM *fromp, ETERM *argp);
ETERM *body_set_position(ETERM *fromp, ETERM *argp);
ETERM *body_update_position(ETERM *fromp, ETERM *argp);
ETERM *body_set_angle(ETERM *fromp, ETERM *argp);
ETERM *body_set_angular_velocity(ETERM *fromp, ETERM *argp);
ETERM *body_get_data(ETERM *fromp, ETERM *argp);
ETERM *body_set_data(ETERM *fromp, ETERM *argp);
ETERM *body_update_user_data(ETERM *fromp, ETERM *argp);
ETERM *body_apply_impulse(ETERM *fromp, ETERM *argp);
ETERM *body_copy(ETERM *fromp, ETERM *argp);
ETERM *body_set_collision_circle(ETERM *fromp, ETERM *argp);

ETERM *space_subscribe_box(ETERM *fromp, ETERM *argp);

void spacesStep(float dt);
void spacesRemoveSubscriber(erlmunk_client *client);
