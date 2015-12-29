/* erlmunk_space.c */

#include "chipmunk/chipmunk.h"

#include "erl_interface.h"
#include "ei.h"

#include "uthash.h"
#include "utlist.h"

#include "erlmunk_util.h"
#include "erlmunk_space.h"

typedef struct element {
    cpBody *body;
    struct element *next; /* needed for singly- or doubly-linked lists */
} element;

erlmunk_space *erlmunk_spaces = NULL;

void add_space_body(erlmunk_space *s, int id, cpBody *body) {
    erlmunk_body *b = (erlmunk_body *) malloc(sizeof(erlmunk_body));
    b->id = id;
    b->body = body;
    HASH_ADD_INT(s->bodies, id, b);
}

void remove_space_body(erlmunk_space *s, erlmunk_body *body) {
    HASH_DEL(s->bodies, body);
    free(body);
}

void space_add_subscriber(erlmunk_space *s,
                          ETERM *from,
                          float l, float b,
                          float r, float t) {

    erlmunk_subscriber *subscriber = (erlmunk_subscriber *) malloc(sizeof(erlmunk_subscriber));
    subscriber->id = s->subscriber_count++;
    subscriber->bb = cpBBNew(l, b, r, t);
    subscriber->from = from;

    HASH_ADD_INT(s->subscribers, id, subscriber);
}

ETERM *space_new(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *iterationsp = erl_element(1, argp);
    ETERM *gravityp = erl_element(2, argp);
    ETERM *gravityxp = erl_element(1, gravityp);
    ETERM *gravityyp = erl_element(2, gravityp);

    // create the new space
    cpSpace *space = cpSpaceNew();
    cpSpaceSetIterations(space, ERL_INT_VALUE(iterationsp));
    cpSpaceSetGravity(space, cpv(ERL_FLOAT_VALUE(gravityxp),
                                 ERL_FLOAT_VALUE(gravityyp)));

    // add it to the hash table
    ETERM *ref = erl_mk_node_ref();
    erlmunk_space *s = (erlmunk_space *) malloc(sizeof(erlmunk_space));
    s->id = ERL_REF_NUMBER(ref);
    s->space = space;
    s->subscriber_count = 0;
    s->subscribers = NULL;
    HASH_ADD_INT(erlmunk_spaces, id, s);

    ETERM *atom_ok = erl_mk_atom("ok");
    ETERM **space_new_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    space_new_array[0] = atom_ok;
    space_new_array[1] = ref;
    ETERM *space_new_tuple = erl_mk_tuple(space_new_array, 2);

    ETERM *reply_tuple = erl_mk_reply(fromp, space_new_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    DEBUGF(("space_new has succeeded"));
    return gen_cast_tuple;
}

ETERM *space_add_body(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *massp = erl_element(3, argp);
    // ETERM *inertiap = erl_element(4, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    cpBody *body = cpSpaceAddBody(s->space,
                                  cpBodyNew(ERL_FLOAT_VALUE(massp),
                                            INFINITY));
    cpShape *shape = cpSpaceAddShape(s->space, cpCircleShapeNew(body, 5, cpvzero));
    cpShapeSetCollisionType(shape, 1);
    add_space_body(s, ERL_INT_VALUE(idp), body);

    DEBUGF(("space_add_body has succeeded"));
    return NULL;
}

ETERM *body_set_position(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *vectorp = erl_element(3, argp);
    ETERM *xp = erl_element(1, vectorp);
    ETERM *yp = erl_element(2, vectorp);

    erlmunk_space *s = NULL;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b = NULL;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpBodySetPosition(b->body, cpv(ERL_FLOAT_VALUE(xp),
                                   ERL_FLOAT_VALUE(yp)));

    DEBUGF(("body_set_position(x: %f, y: %f) has succeeded",
        ERL_FLOAT_VALUE(xp), ERL_FLOAT_VALUE(yp)));
    return NULL;
}

ETERM *body_set_angle(ETERM *fromp, ETERM *argp) {
    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *anglep = erl_element(3, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpBodySetAngle(b->body, ERL_FLOAT_VALUE(anglep));

    DEBUGF(("body_set_angle(%f) has succeeded", ERL_FLOAT_VALUE(anglep)));
    return NULL;
}

ETERM *space_subscribe_box(ETERM *fromp, ETERM *argp) {
    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *subscriber_pidp = erl_element(2, argp);
    ETERM *bounding_boxp = erl_element(3, argp);
    ETERM *leftp = erl_element(1, bounding_boxp);
    ETERM *bottomp = erl_element(2, bounding_boxp);
    ETERM *rightp = erl_element(3, bounding_boxp);
    ETERM *topp = erl_element(4, bounding_boxp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    space_add_subscriber(s, subscriber_pidp,
                         ERL_FLOAT_VALUE(leftp), ERL_FLOAT_VALUE(bottomp),
                         ERL_FLOAT_VALUE(rightp), ERL_FLOAT_VALUE(topp));

    DEBUGF(("space_subscribe_box has succeeded"));
    return NULL;
}

void handle_shape(cpShape *shape, void *data) {

    element *l = (element *) data;
    cpBody *body = cpShapeGetBody(shape);
    DEBUGF(("body: %p\n", body));
    element *el = (element *) malloc(sizeof(element));
    el->body = body;
    LL_APPEND(l, el);
}

void handle_space_subscribers(erlmunk_space *s) {

    for(erlmunk_subscriber *subscriber = s->subscribers;
        subscriber != NULL;
        subscriber = subscriber->hh.next) {

        element *bodies_in_bb = NULL, *el, *tmp;
        cpSpaceBBQuery(s->space, subscriber->bb,
                       CP_SHAPE_FILTER_ALL,
                       handle_shape, bodies_in_bb);

        LL_FOREACH(bodies_in_bb, el) {
            cpVect vect = cpBodyGetPosition(el->body);
            DEBUGF(("x: %f, y: %f\n", vect.x, vect.y));
        }

        /* now delete each element, use the safe iterator */
        LL_FOREACH_SAFE(bodies_in_bb, el, tmp) {
            LL_DELETE(bodies_in_bb, el);
        }
    }
}

void spacesStep(float dt) {

    for(erlmunk_space *s = erlmunk_spaces; s != NULL; s = s->hh.next) {
        cpSpaceStep(s->space, (1.0 / FRAMES_PER_SECOND));

        handle_space_subscribers(s);
    }
}
