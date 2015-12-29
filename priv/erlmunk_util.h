#define FRAMES_PER_SECOND 10

/* uncomment the next line to enable debug print */
#define DEBUG

#ifdef DEBUG
#   undef DEBUGF
#   define DEBUGF(P) debugf P
#else
#   define DEBUGF(P)
#endif

int tcp_listen(int *port);

void debugf(char *str, ...);

char *binary_to_string(ETERM *bin);

ETERM *erl_mk_gen_cast(ETERM *arg);

void erl_free_gen_cast(ETERM *gen_cast);

ETERM *erl_mk_node_ref();

ETERM *erl_mk_reply(ETERM *fromp, ETERM *reply);

