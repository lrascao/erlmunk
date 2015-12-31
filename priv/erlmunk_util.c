/* erlmunk_util.c */

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "erl_interface.h"
#include "ei.h"

#include "uthash.h"

#include "erlmunk_util.h"

extern char *node_name;
unsigned int ref_count = 0;
unsigned char creation = 'c';
erlmunk_client *current_client = NULL;

void set_current_client(erlmunk_client *client) {
    current_client = client;
}

erlmunk_client * get_current_client() {
    return current_client;
}

int tcp_listen(int *port) {
    int fd;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    int on = 1;

    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        return -1;

    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

    memset((void*) &addr, 0, (size_t) sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(*port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
        return -1;

    if (getsockname(fd, (struct sockaddr *)&addr, &len) == -1)
        return -1;

    *port = ntohs(addr.sin_port);

    listen(fd, 5);

    return fd;
}

void debugf(char *str, ...)
{
  va_list ap;
  va_start(ap,str);
  fprintf(stderr,"[erlmunk_cnode] : ");
  vfprintf(stderr,str, ap);
  fprintf(stderr,"\r\n");
  va_end(ap);
}

char *binary_to_string(ETERM *bin) {
    char *out = malloc(sizeof(char) * ERL_BIN_SIZE(bin) + 1);
    memcpy(out, ERL_BIN_PTR(bin), ERL_BIN_SIZE(bin));
    out[ERL_BIN_SIZE(bin)] = '\0';
    return out;
}

ETERM *erl_mk_gen_cast(ETERM *arg) {
    ETERM *gen_cast_atom = erl_mk_atom("$gen_cast");
    ETERM **gen_cast_tuple_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    gen_cast_tuple_array[0] = gen_cast_atom;
    gen_cast_tuple_array[1] = arg;
    ETERM *gen_cast_tuple = erl_mk_tuple(gen_cast_tuple_array, 2);
    free(gen_cast_tuple_array);
    return gen_cast_tuple;
}

void erl_free_gen_cast(ETERM *gen_cast) {
    erl_free_compound(gen_cast);
}

ETERM *erl_mk_node_ref() {
    return erl_mk_ref(node_name, ref_count++, creation);
}

ETERM *erl_mk_reply(ETERM *fromp, ETERM *reply) {

    ETERM *atom_reply = erl_mk_atom("reply");
    ETERM **reply_array = (ETERM **) malloc(sizeof(ETERM*) * 3);
    reply_array[0] = atom_reply;
    reply_array[1] = fromp;
    reply_array[2] = reply;
    ETERM *reply_tuple = erl_mk_tuple(reply_array, 3);
    free(reply_array);
    return reply_tuple;
}

ETERM *erl_mk_int_prop_value(int id, ETERM *value) {

    ETERM *key_term = erl_mk_int(id);
    ETERM **tuple_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    tuple_array[0] = key_term;
    tuple_array[1] = value;
    ETERM *t = erl_mk_tuple(tuple_array, 2);
    free(tuple_array);
    return t;
}

ETERM *update_prop_value(ETERM *old_value, ETERM *new_value) {
    if (ERL_IS_INTEGER(old_value)) {
        int delta = ERL_INT_VALUE(new_value);
        return erl_mk_int(ERL_INT_VALUE(old_value) + delta);
    }

    return erl_copy_term(new_value);
}

ETERM *erl_lists_keyreplace(ETERM *list, ETERM *key, ETERM *value) {

    int length = erl_length(list);
    ETERM **new_list_array = (ETERM **) malloc(sizeof(ETERM*) * length);
    int i = 0;
    do {
        ETERM *hd = erl_hd(list);
        ETERM *k = erl_element(1, hd);
        ETERM *v = erl_element(2, hd);

        ETERM **new_prop_tuple_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
        if (strcmp(ERL_ATOM_PTR(k), ERL_ATOM_PTR(key)) == 0) {
            // found our match
            new_prop_tuple_array[0] = erl_copy_term(key);
            new_prop_tuple_array[1] = update_prop_value(v, value);
        } else {
            // make a new copy of the current values
            new_prop_tuple_array[0] = erl_copy_term(k);
            new_prop_tuple_array[1] = erl_copy_term(v);
        }
        ETERM *new_prop_tuple = erl_mk_tuple(new_prop_tuple_array, 2);
        free(new_prop_tuple_array);
        new_list_array[i] = new_prop_tuple;
        i++;
        list = erl_tl(list);
    } while (i < length);

    free(new_list_array);
    return erl_mk_list(new_list_array, length);
}

float deg_to_rad(float d) {
    return d * RADIAN;
}

float rad_to_deg(float r) {
    return r / RADIAN;
}
