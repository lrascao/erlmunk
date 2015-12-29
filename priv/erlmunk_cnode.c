/* erlmunk_cnode.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <poll.h>

#include "erl_interface.h"
#include "ei.h"

#include "uthash.h"

#include "erlmunk_util.h"
#include "erlmunk_space.h"

#define BUFSIZE 2048

typedef struct {
    int fd;
    ErlConnect *conn;
    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_client;

typedef ETERM* (*erlmunk_command_fptr_)(ETERM *, ETERM *);
typedef struct {
    char name[64];
    erlmunk_command_fptr_ command;
    UT_hash_handle hh; /* makes this structure hashable */
} erlmunk_command;

static ETERM *init(ETERM *fromp, ETERM *argp);

char *node_name;
erlmunk_client *erlmunk_clients = NULL;
erlmunk_command *erlmunk_commands = NULL;

static void init_commands() {
    erlmunk_command commands[] = {
        { .name = "init", .command = init },
        { .name = "space_new", .command = space_new },
        { .name = "space_add_body", .command = space_add_body },
        { .name = "body_set_position", .command = body_set_position },
        { .name = "body_set_angle", .command = body_set_angle },
        { .name = "space_subscribe_box", .command = space_subscribe_box },
        { .name = "", .command = NULL }
    };

    int i = 0;
    while (1) {
        /* end of the command array */
        if (commands[i].command == NULL)
            break;

        erlmunk_command *cmd = (erlmunk_command *) malloc(sizeof(erlmunk_command));
        memcpy(cmd, &commands[i], sizeof(erlmunk_command));
        HASH_ADD_STR(erlmunk_commands, name, cmd);
        i++;
    }
}

static ETERM* run_command(char *name, ETERM *fromp, ETERM *argp) {
    erlmunk_command *c;

    HASH_FIND_STR(erlmunk_commands, name, c);
    if (c)
        return (c->command)(fromp, argp);
    return NULL;
}

static ETERM *command(ETERM *command, ETERM *fromp, ETERM *argp)
{
    ETERM *resp = NULL;
    char *command_str = NULL;

    command_str = ERL_ATOM_PTR(command);
    DEBUGF(("command: %s", command_str));
    resp = run_command(command_str, fromp, argp);

    return resp;
}

static ETERM *init(ETERM *fromp, ETERM *argp) {
    ETERM **inited_array;
    ETERM *ref, *atom_inited, *inited_tuple;

    ref = erl_element(1, argp);
    if (! ERL_IS_REF(ref))
        return NULL;

    atom_inited = erl_mk_atom("inited");

    inited_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    inited_array[0] = atom_inited;
    inited_array[1] = ref;
    inited_tuple = erl_mk_tuple(inited_array, 2);

    ETERM *reply_tuple = erl_mk_reply(fromp, inited_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    DEBUGF(("init has succeeded"));
    return gen_cast_tuple;
}

void add_erlmunk_client(int fd, ErlConnect *conn) {
    erlmunk_client *client = (erlmunk_client *) malloc(sizeof(erlmunk_client));
    client->fd = fd;
    client->conn = conn;
    HASH_ADD_INT(erlmunk_clients, fd, client);
}

void remove_erlmunk_client(erlmunk_client *client) {
    HASH_DEL(erlmunk_clients, client);
    free(client);
}

struct pollfd *get_clients_to_poll(int listen_fd, int *nfds) {
    static struct pollfd *poll_fds = NULL;
    static int n_poll_fds = 0;

    // total number of clients plus the accept socket
    int n_clients = HASH_COUNT(erlmunk_clients) + 1;
    if (n_poll_fds != n_clients) {
        if (poll_fds != NULL)
            free(poll_fds);
        poll_fds = malloc(sizeof(struct pollfd) * n_clients);
        n_poll_fds = n_clients;
    }

    // the zero slot is reserved for the accept socket
    poll_fds[0].fd = listen_fd;
    poll_fds[0].events = POLLIN;
    int n = 1;
    for(erlmunk_client *client = erlmunk_clients; client != NULL; client = client->hh.next) {
        poll_fds[n].fd = client->fd;
        poll_fds[n].events = POLLIN;
        n++;
    }

    *nfds = n_clients;
    return poll_fds;
}

void handle_cast(ETERM *message) {

    ETERM *commandp = erl_element(1, message);
    ETERM *argp = erl_element(2, message);
    command(commandp, NULL, argp);
}

ETERM *handle_call(ETERM *fromp, ETERM *message) {

    ETERM *commandp = erl_element(1, message);
    ETERM *argp = erl_element(2, message);
    return command(commandp, fromp, argp);
}

void handle_message(erlmunk_client *client) {
    int got;                                 /* Result of receive */
    unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
    ErlMessage emsg;                         /* Incoming message */

    got = erl_receive_msg(client->fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
        // DEBUGF(("tick"));
        /* ignore */
        return;
    }

    if (got == ERL_ERROR) {
        DEBUGF(("error receiving message"));
        remove_erlmunk_client(client);
        return;
    }

    if (emsg.type != ERL_REG_SEND) {
        DEBUGF(("got something other than a ERL_REG_SEND: %d\n", emsg.type));
        return;
    }

    /* {cast, {Command, Args}} | {call, From, {Command, Args}} */
    char *message_type = ERL_ATOM_PTR(erl_element(1, emsg.msg));

    if (strcmp(message_type, "cast") == 0) {
        ETERM *message = erl_element(2, emsg.msg);
        handle_cast(message);
    } else if (strcmp(message_type, "call") == 0) {
        ETERM *from = erl_element(2, emsg.msg);
        ETERM *message = erl_element(3, emsg.msg);
        ETERM *resp = handle_call(from, message);
        if (erl_send(client->fd, emsg.from, resp) == 0) {
            DEBUGF(("failed to send reply to client %d\n", client->fd));
        }
        erl_free_compound(resp);
    }

    erl_free_compound(emsg.from);
    erl_free_compound(emsg.msg);
}

void handle_clients(int listen_fd, struct pollfd *fds, int nfds, int n_set_fds, ei_cnode *ec) {

    int n_checked_fds = 0;
    for(int i = 0; i < nfds; i++) {
        if (!fds[i].revents & POLLIN)
            continue;

        // fds[i].fd is set
        if (fds[i].fd == listen_fd) {
            // accept the connection and add to the client list
            ErlConnect *conn = malloc(sizeof(ErlConnect));                         /* Connection data */
            int fd = ei_accept(ec, listen_fd, conn);
            if (fd == ERL_ERROR) {
                DEBUGF(("ei_accept failed: %d", fd));
            }
            DEBUGF(("Connected to %s",
                conn->nodename));
            add_erlmunk_client(fd, conn);
        } else {
            erlmunk_client *client = NULL;
            HASH_FIND_INT(erlmunk_clients, &fds[i].fd, client);

            handle_message(client);
        }
        n_checked_fds++;

        if (n_checked_fds == n_set_fds)
            return;
    }
}

int main(int argc, char **argv) {
    struct in_addr addr;                     /* 32-bit IP number of host */
    int listen_fd;                              /* Listen socket */
    int port = 0;                            /* port number */
    ei_cnode ec;
    char *node_host, *cookie;
    char full_node_name[2048];

    node_name = argv[1];
    node_host = argv[2];
    cookie = argv[3];

    DEBUGF(("args:"));
    DEBUGF(("   [1] node_name: %s", node_name));
    DEBUGF(("   [2] node_host: %s", node_host));
    DEBUGF(("   [3] cookie: %s", cookie));

    erl_init(NULL, 0);

    node_name = argv[1];
    node_host = argv[2];
    cookie = argv[3];
    full_node_name[0] = '\0';
    strcat(full_node_name, node_name);
    strcat(full_node_name, "@");
    strcat(full_node_name, node_host);

    addr.s_addr = inet_addr("127.0.0.1");
    if (ei_connect_xinit(&ec,
                         node_host, node_name,
                         full_node_name,
                         &addr,
                         cookie, 0) == -1)
        erl_err_quit("erlmunk_cnode : ei_connect_xinit failed");

    /* Make a listen socket */
    if ((listen_fd = tcp_listen(&port)) <= 0)
        erl_err_quit("erlmunk_cnode : tcp_listen failed");

    if (ei_publish(&ec, port) == -1)
        erl_err_quit("erlmunk_cnode : ei_publish failed");

    DEBUGF(("published port: %d", port));

    init_commands();

    while (1) {
        int timeout = 1000 / FRAMES_PER_SECOND;
        int nfds;

        struct pollfd *fds = get_clients_to_poll(listen_fd, &nfds);
        int n_set_fds = poll(fds, nfds, timeout);

        if (n_set_fds != 0)
            handle_clients(listen_fd, fds, nfds, n_set_fds, &ec);

        spacesStep(1.0 / FRAMES_PER_SECOND);
    }
    return 0;
}
