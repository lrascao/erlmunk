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
#include <sys/timeb.h>

#include "erl_interface.h"
#include "ei.h"

#include "uthash.h"

#include "chipmunk/chipmunk.h"

#include "erlmunk_util.h"
#include "erlmunk_space.h"

#define BUFSIZE 2048

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
        { .name = "space_delete", .command = space_delete },
        { .name = "space_add_body", .command = space_add_body },
        { .name = "space_remove_body", .command = space_remove_body },
        { .name = "space_subscribe_collision", .command = space_subscribe_collision },
        { .name = "space_add_boundaries", .command = space_add_boundaries },
        { .name = "body_activate", .command = body_activate },
        { .name = "body_get_position", .command = body_get_position },
        { .name = "body_set_position", .command = body_set_position },
        { .name = "body_update_position", .command = body_update_position },
        { .name = "body_set_angle", .command = body_set_angle },
        { .name = "body_set_angle", .command = body_set_angle },
        { .name = "body_set_angular_velocity", .command = body_set_angular_velocity },
        { .name = "body_get_data", .command = body_set_data },
        { .name = "body_set_data", .command = body_set_data },
        { .name = "body_update_user_data", .command = body_update_user_data },
        { .name = "body_apply_impulse", .command = body_apply_impulse },
        { .name = "body_copy", .command = body_copy },
        { .name = "body_set_collision_circle", .command = body_set_collision_circle },
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
    spacesRemoveSubscriber(client);
    HASH_DEL(erlmunk_clients, client);
    free(client->conn);
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

    set_current_client(client);

    /* {cast, {Command, Args}} | {call, From, {Command, Args}} */
    char *message_type = ERL_ATOM_PTR(erl_element(1, emsg.msg));

    if (strcmp(message_type, "cast") == 0) {
        ETERM *message = erl_element(2, emsg.msg);
        handle_cast(message);
    } else if (strcmp(message_type, "call") == 0) {
        ETERM *from = erl_element(2, emsg.msg);
        ETERM *message = erl_element(3, emsg.msg);
        ETERM *resp = handle_call(from, message);
        if (erl_send(client->fd, emsg.from, resp) != 1) {
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
        // only care about data coming in
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

    addr.s_addr = inet_addr("0.0.0.0");
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
        struct timeb start, end;

        // get the time when we started waiting
        ftime(&start);

        struct pollfd *fds = get_clients_to_poll(listen_fd, &nfds);
        int n_set_fds = poll(fds, nfds, timeout);

        if (n_set_fds != 0) {
            handle_clients(listen_fd, fds, nfds, n_set_fds, &ec);

            // and the time when we finished handling the client requests
            ftime(&end);
            // diff is in millis
            int diff = (int) (1000.0 * (end.time - start.time) + (end.millitm - start.millitm));
            // we're interested in constant time steps, so wait out the remainder of the delta t
            // if any exists
            if (diff < timeout) {
                // DEBUGF(("sleeping the remainder of the cycle: %d (%d - %d)",
                //     timeout - diff, timeout, diff));
                poll(NULL, 0, timeout - diff);
            }
        }

        spacesStep(1.0 / FRAMES_PER_SECOND);
    }
    return 0;
}
