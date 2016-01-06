#define FRAMES_PER_SECOND 30

#define RADIAN 0.01745329251994

#define BOUNDARY_BODY_ID -1

/* uncomment the next line to enable debug print */
#define DEBUG

#ifdef DEBUG
#   undef DEBUGF
#   define DEBUGF(P) debugf P
#else
#   define DEBUGF(P)
#endif

typedef struct {
    int fd;
    ErlConnect *conn;
    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_client;

void set_current_client(erlmunk_client *client);
erlmunk_client * get_current_client();

int tcp_listen(int *port);

void debugf(char *str, ...);

char *binary_to_string(ETERM *bin);

ETERM *erl_mk_gen_cast(ETERM *arg);

void erl_free_gen_cast(ETERM *gen_cast);

ETERM *erl_mk_node_ref();

ETERM *erl_mk_reply(ETERM *fromp, ETERM *reply);

ETERM *erl_mk_int_prop_value(int id, ETERM *tuple);

ETERM *erl_lists_keyreplace(ETERM *list, ETERM *key, ETERM *value);

float deg_to_rad(float d);

float rad_to_deg(float r);
