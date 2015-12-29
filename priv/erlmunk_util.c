/* erlmunk_util.c */

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "erl_interface.h"
#include "ei.h"

extern char *node_name;
unsigned int ref_count = 0;
unsigned char creation = 'c';

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
    return reply_tuple;
}
