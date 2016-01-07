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

void space_add_body_hash(erlmunk_space *s, int id, cpBody *body) {
    erlmunk_body *b = (erlmunk_body *) malloc(sizeof(erlmunk_body));
    b->id = id;
    b->body = body;
    HASH_ADD_INT(s->bodies, id, b);
}

void space_remove_body_hash(erlmunk_space *s, erlmunk_body *body) {
    HASH_DEL(s->bodies, body);
    free(body);
}

void space_add_subscriber(erlmunk_space *s,
                          erlmunk_client *client,
                          ETERM *from,
                          float l, float b,
                          float r, float t) {

    erlmunk_subscriber *subscriber = (erlmunk_subscriber *) malloc(sizeof(erlmunk_subscriber));
    subscriber->id = s->subscriber_count++;
    subscriber->bb = cpBBNew(l, b, r, t);
    subscriber->client = client;
    subscriber->from = from;

    HASH_ADD_INT(s->subscribers, id, subscriber);
}

void space_remove_subscriber(erlmunk_space *s, erlmunk_subscriber *subscriber) {
    HASH_DEL(s->subscribers, subscriber);
    free(subscriber);
}

void space_remove_subscriptions(erlmunk_space *s, erlmunk_client *client) {
    for(erlmunk_subscriber *subscriber = s->subscribers; subscriber != NULL;
        subscriber = subscriber->hh.next) {
        if (subscriber->client == client)
            space_remove_subscriber(s, subscriber);
    }
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
    cpSpaceSetSleepTimeThreshold(space, 5.0);

    // add it to the hash table
    ETERM *ref = erl_mk_node_ref();
    erlmunk_space *s = (erlmunk_space *) malloc(sizeof(erlmunk_space));
    s->id = ERL_REF_NUMBER(ref);
    s->space = space;
    s->subscriber_count = 0;
    s->subscribers = NULL;
    s->bodies = NULL;
    HASH_ADD_INT(erlmunk_spaces, id, s);

    ETERM *atom_ok = erl_mk_atom("ok");
    ETERM **space_new_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    space_new_array[0] = atom_ok;
    space_new_array[1] = ref;
    ETERM *space_new_tuple = erl_mk_tuple(space_new_array, 2);
    free(space_new_array);

    ETERM *reply_tuple = erl_mk_reply(fromp, space_new_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    return gen_cast_tuple;
}

ETERM *space_delete(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    // remove all subscribers
    for(erlmunk_subscriber *subscriber = s->subscribers; subscriber != NULL;
        subscriber = subscriber->hh.next) {
        space_remove_subscriber(s, subscriber);
    }
    // remove all bodies
    for(erlmunk_body *b = s->bodies; b != NULL;
        b = b->hh.next) {
        erlmunk_body_data *data = cpBodyGetUserData(b->body);
        if (data->term != NULL)
            erl_free_compound(data->term);
        free(data);

        cpSpaceRemoveBody(s->space, b->body);
        cpBodyDestroy(b->body);
        cpBodyFree(b->body);
        space_remove_body_hash(s, b);
    }

    return NULL;
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

    int object_id = ERL_INT_VALUE(idp);

    cpBody *body = cpSpaceAddBody(s->space,
                                  cpBodyNew(ERL_FLOAT_VALUE(massp),
                                            INFINITY));
    // the body is created inactive, it is explicitly activated
    // when all it's values have been set.
    cpBodySleep(body);
    erlmunk_body_data *data = malloc(sizeof(erlmunk_body_data));
    data->id = object_id;
    data->term = NULL;
    cpBodySetUserData(body, (cpDataPointer) data);
    space_add_body_hash(s, object_id, body);

    return NULL;
}

void shapeRemove(cpBody *body, cpShape *shape, void *data) {
    cpSpace *space = cpBodyGetSpace(body);
    cpSpaceRemoveShape(space, shape);
    cpShapeDestroy(shape);
    cpShapeFree(shape);
}

ETERM *space_remove_body(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b = NULL;
    HASH_FIND_INT(s->bodies, &body_id, b);
    if (b == NULL)
        return NULL;

    // remove the user data associated with the body
    erlmunk_body_data *data = cpBodyGetUserData(b->body);
    if (data->term != NULL)
        erl_free_compound(data->term);
    free(data);

    cpBodyEachShape(b->body, shapeRemove, NULL);
    cpSpaceRemoveBody(s->space, b->body);

    space_remove_body_hash(s, b);

    cpBodyDestroy(b->body);
    cpBodyFree(b->body);

    return NULL;
}

static cpBool
handle_collision(cpArbiter *arbiter, cpSpace *space, cpDataPointer user_data)
{
    CP_ARBITER_GET_BODIES(arbiter, b1, b2);
    erlmunk_subscriber *subscriber = (erlmunk_subscriber *) user_data;

    erlmunk_body_data *data1 = cpBodyGetUserData(b1);
    erlmunk_body_data *data2 = cpBodyGetUserData(b2);

    // DEBUGF(("collision detected between %d and %d\n", data1->id, data2->id));

    ETERM **data1_array = (ETERM **) malloc(sizeof(ETERM) * 2);
    data1_array[0] = erl_mk_int(data1->id);
    data1_array[1] = data1->term;
    ETERM *data1_term = erl_mk_tuple(data1_array, 2);
    free(data1_array);

    ETERM **data2_array = (ETERM **) malloc(sizeof(ETERM) * 2);
    data2_array[0] = erl_mk_int(data2->id);
    data2_array[1] = data2->term;
    ETERM *data2_term = erl_mk_tuple(data2_array, 2);
    free(data2_array);

    ETERM *a = erl_mk_atom("erlmunk_collision");
    ETERM **data_array = (ETERM **) malloc(sizeof(ETERM) * 3);
    data_array[0] = a;
    data_array[1] = data1_term;
    data_array[2] = data2_term;
    ETERM *data = erl_mk_tuple(data_array, 3);
    free(data_array);

    ETERM *gen_cast = erl_mk_gen_cast(data);

    if (erl_send(subscriber->client->fd, subscriber->from, gen_cast) != 1) {
        DEBUGF(("failed to send data to subscriber"));
    }

    return cpFalse;
}

ETERM *space_subscribe_collision(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *typeap = erl_element(2, argp);
    ETERM *typebp = erl_element(3, argp);
    ETERM *pidp = erl_element(4, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    erlmunk_subscriber *subscriber = (erlmunk_subscriber *) malloc(sizeof(erlmunk_subscriber));
    subscriber->client = get_current_client();
    subscriber->from = erl_copy_term(pidp);

    cpCollisionHandler *handler = cpSpaceAddCollisionHandler(s->space,
                                                             ERL_INT_VALUE(typeap),
                                                             ERL_INT_VALUE(typebp));
    handler->beginFunc = handle_collision;
    handler->userData = subscriber;

    return NULL;
}

ETERM *space_add_boundaries(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *lower_leftp = erl_element(2, argp);
    ETERM *lower_rightp = erl_element(3, argp);
    ETERM *upper_leftp = erl_element(4, argp);
    ETERM *upper_rightp = erl_element(5, argp);
    ETERM *collision_categoryp = erl_element(6, argp);
    ETERM *datap = erl_element(7, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    cpVect lowerLeft = cpv(ERL_FLOAT_VALUE(erl_element(1, lower_leftp)),
                           ERL_FLOAT_VALUE(erl_element(2, lower_leftp)));
    cpVect lowerRight = cpv(ERL_FLOAT_VALUE(erl_element(1, lower_rightp)),
                            ERL_FLOAT_VALUE(erl_element(2, lower_rightp)));
    cpVect upperLeft = cpv(ERL_FLOAT_VALUE(erl_element(1, upper_leftp)),
                           ERL_FLOAT_VALUE(erl_element(2, upper_leftp)));
    cpVect upperRight = cpv(ERL_FLOAT_VALUE(erl_element(1, upper_rightp)),
                            ERL_FLOAT_VALUE(erl_element(2, upper_rightp)));

    // get the static body that comes with the space
    cpBody *static_body = cpSpaceGetStaticBody(s->space);
    erlmunk_body_data *data = malloc(sizeof(erlmunk_body_data));
    data->id = BOUNDARY_BODY_ID;
    data->term = erl_copy_term(datap);
    cpBodySetUserData(static_body, (cpDataPointer) data);

    // bottom
    cpShape *bottomBoundaryShape = cpSegmentShapeNew(static_body, lowerLeft, lowerRight, 0.0f);
    cpShapeSetCollisionType(bottomBoundaryShape, ERL_INT_VALUE(collision_categoryp));
    cpSpaceAddShape(s->space, bottomBoundaryShape);
    // top
    cpShape *topBoundaryShape = cpSegmentShapeNew(static_body, upperLeft, upperRight, 0.0f);
    cpShapeSetCollisionType(topBoundaryShape, ERL_INT_VALUE(collision_categoryp));
    cpSpaceAddShape(s->space, topBoundaryShape);
    // left
    cpShape *leftBoundaryShape = cpSegmentShapeNew(static_body, lowerLeft, upperLeft, 0.0f);
    cpShapeSetCollisionType(leftBoundaryShape, ERL_INT_VALUE(collision_categoryp));
    cpSpaceAddShape(s->space, leftBoundaryShape);
    // right
    cpShape *rightBoundaryShape = cpSegmentShapeNew(static_body, lowerRight, upperRight, 0.0f);
    cpShapeSetCollisionType(rightBoundaryShape, ERL_INT_VALUE(collision_categoryp));
    cpSpaceAddShape(s->space, rightBoundaryShape);

    return NULL;
}

ETERM *body_activate(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpBodyActivate(b->body);

    return NULL;
}

ETERM *body_get_position(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);

    erlmunk_space *s = NULL;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b = NULL;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpVect position = cpBodyGetPosition(b->body);
    float angle = cpBodyGetAngle(b->body);

    ETERM **position_tuple_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    position_tuple_array[0] = erl_mk_float(position.x);
    position_tuple_array[1] = erl_mk_float(position.y);
    ETERM *position_tuple = erl_mk_tuple(position_tuple_array, 2);
    free(position_tuple_array);

    ETERM *atom_ok = erl_mk_atom("ok");
    ETERM **get_position_array = (ETERM **) malloc(sizeof(ETERM*) * 3);
    get_position_array[0] = atom_ok;
    get_position_array[1] = position_tuple;
    get_position_array[2] = erl_mk_float(angle);
    ETERM *get_position_tuple = erl_mk_tuple(get_position_array, 3);
    free(get_position_array);

    ETERM *reply_tuple = erl_mk_reply(fromp, get_position_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    // DEBUGF(("body_get_position(body: %d, x: %f, y: %f, angle: %f) has succeeded",
    //     body_id, position.x, position.y, angle));

    return gen_cast_tuple;
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

    // DEBUGF(("body_set_position(x: %f, y: %f) has succeeded",
    //     ERL_FLOAT_VALUE(xp), ERL_FLOAT_VALUE(yp)));
    return NULL;
}

ETERM *body_update_position(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *deltap = erl_element(3, argp);

    erlmunk_space *s = NULL;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b = NULL;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpVect position = cpBodyGetPosition(b->body);
    float angle = deg_to_rad(cpBodyGetAngle(b->body));
    cpVect angleV = cpvforangle(angle);
    cpVect projection = cpvmult(angleV, ERL_FLOAT_VALUE(deltap));
    cpVect new_position = cpvadd(projection, position);
    cpBodySetPosition(b->body, new_position);

    // DEBUGF(("body_update_position(x: %f, y: %f, delta: %f) has succeeded (x: %f, y: %f)",
    //     position.x, position.y, ERL_FLOAT_VALUE(deltap),
    //     new_position.x, new_position.y));
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

    // DEBUGF(("body_set_angle(%f) has succeeded", ERL_FLOAT_VALUE(anglep)));
    return NULL;
}

ETERM *body_set_angular_velocity(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *angular_vel = erl_element(3, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpBodySetAngularVelocity(b->body, ERL_FLOAT_VALUE(angular_vel));

    // DEBUGF(("body_set_angular_velocity(%f) has succeeded",
    //     ERL_FLOAT_VALUE(angular_vel)));
    return NULL;
}

ETERM *body_set_collision_circle(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *radiusp = erl_element(3, argp);
    ETERM *collision_typep = erl_element(4, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    cpShape *shape = cpSpaceAddShape(s->space,
                                     cpCircleShapeNew(b->body, ERL_FLOAT_VALUE(radiusp),
                                                      cpvzero));
    cpShapeSetCollisionType(shape, ERL_INT_VALUE(collision_typep));

    // DEBUGF(("body_set_collision_circle has succeeded"));
    return NULL;
}

ETERM *body_get_user_data(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    erlmunk_body_data *data = cpBodyGetUserData(b->body);
    ETERM *datap = erl_copy_term(data->term);

    ETERM *atom_ok = erl_mk_atom("ok");
    ETERM **body_get_data_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    body_get_data_array[0] = atom_ok;
    body_get_data_array[1] = datap;
    ETERM *body_get_data_tuple = erl_mk_tuple(body_get_data_array, 2);

    ETERM *reply_tuple = erl_mk_reply(fromp, body_get_data_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    return gen_cast_tuple;
}

ETERM *body_set_user_data(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *datap = erl_element(3, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    erlmunk_body_data *data = cpBodyGetUserData(b->body);
    data->term = erl_copy_term(datap);
    cpBodySetUserData(b->body, (cpDataPointer) data);

    return NULL;
}

ETERM *body_update_user_data(ETERM *fromp, ETERM *argp) {

    // DEBUGF(("body_update_user_data\n"));

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *keyp = erl_element(3, argp);
    ETERM *valuep = erl_element(4, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    erlmunk_body_data *data = cpBodyGetUserData(b->body);

    ETERM *new_data_term = erl_lists_keyreplace(data->term, keyp, valuep);
    erl_free_compound(data->term);
    data->term = new_data_term;

    ETERM *new_valuep = erl_proplists_get_value(keyp, new_data_term);

    // obtain updated key value and return that
    ETERM *atom_ok = erl_mk_atom("ok");
    ETERM **body_update_data_array = (ETERM **) malloc(sizeof(ETERM*) * 2);
    body_update_data_array[0] = atom_ok;
    body_update_data_array[1] = new_valuep;
    ETERM *body_update_data_tuple = erl_mk_tuple(body_update_data_array, 2);

    ETERM *reply_tuple = erl_mk_reply(fromp, body_update_data_tuple);
    ETERM *gen_cast_tuple = erl_mk_gen_cast(reply_tuple);

    return gen_cast_tuple;
}

ETERM *body_apply_impulse(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *impulsep = erl_element(3, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    // apply the impulse at the center of the body and along it's current angle
    float angle = deg_to_rad(cpBodyGetAngle(b->body));
    cpVect angleV = cpvforangle(angle);
    cpVect impulse = cpvmult(angleV, ERL_FLOAT_VALUE(impulsep));
    cpBodyApplyImpulseAtWorldPoint(b->body, impulse, angleV);

    return NULL;
}

ETERM *body_copy(ETERM *fromp, ETERM *argp) {

    // get the args
    ETERM *space_refp = erl_element(1, argp);
    ETERM *idp = erl_element(2, argp);
    ETERM *from_idp = erl_element(3, argp);

    erlmunk_space *s;
    int space_id = ERL_REF_NUMBER(space_refp);
    HASH_FIND_INT(erlmunk_spaces, &space_id, s);

    int body_id = ERL_INT_VALUE(idp);
    erlmunk_body *b;
    HASH_FIND_INT(s->bodies, &body_id, b);

    int from_body_id = ERL_INT_VALUE(from_idp);
    erlmunk_body *from_b;
    HASH_FIND_INT(s->bodies, &from_body_id, from_b);

    // DEBUGF(("copying location from body #%d(%p) to #%d(%p)",
    //     from_body_id, from_b, body_id, b));

    // copy position and angle from the from body
    cpBodySetPosition(b->body, cpBodyGetPosition(from_b->body));
    cpBodySetAngle(b->body, cpBodyGetAngle(from_b->body));
    cpBodySetVelocity(b->body, cpBodyGetVelocity(from_b->body));

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

    erlmunk_client *client = get_current_client();
    space_add_subscriber(s,
                         client, erl_copy_term(subscriber_pidp),
                         ERL_FLOAT_VALUE(leftp), ERL_FLOAT_VALUE(bottomp),
                         ERL_FLOAT_VALUE(rightp), ERL_FLOAT_VALUE(topp));

    // DEBUGF(("space_subscribe_box(client fd: %d) has succeeded",
    //     client->fd));
    return NULL;
}

void handle_shape(cpShape *shape, void *data) {

    cpBody *body = cpShapeGetBody(shape);

    // get the static body that comes with the space
    cpSpace *space = cpBodyGetSpace(body);
    cpBody *static_body = cpSpaceGetStaticBody(space);
    // ignore the static body
    if (static_body == body)
        return;

    element *el = (element *) malloc(sizeof(element));
    el->body = body;
    element **l = (element **) data;
    LL_APPEND(*l, el);
}

void handle_subscriber(erlmunk_subscriber *subscriber, element *bodies) {

    element *el;

    int count = 0;
    LL_COUNT(bodies, el, count);
    // DEBUGF(("# bodies in subscriber bounding box: %d", count));

    ETERM **l_array = (ETERM **) malloc(sizeof(ETERM) * count);

    int nth_body = 0;
    LL_FOREACH(bodies, el) {
        cpVect vect = cpBodyGetPosition(el->body);
        float angle = cpBodyGetAngle(el->body);
        // cpVect vel = cpBodyGetVelocity(el->body);
        erlmunk_body_data *data = (erlmunk_body_data *) cpBodyGetUserData(el->body);
        // DEBUGF(("id: %d, x: %f, y: %f, angle: %f, vel.x: %f, vel.y: %f, data: %p",
        //     data->id, vect.x, vect.y, angle, vel.x, vel.y, data));

        ETERM **t_array = (ETERM **) malloc(sizeof(ETERM) * 4);
        t_array[0] = erl_mk_float(vect.x);
        t_array[1] = erl_mk_float(vect.y);
        t_array[2] = erl_mk_float(angle);
        if (data->term == NULL)
            t_array[3] = erl_mk_undefined();
        else
            t_array[3] = erl_copy_term(data->term);
        ETERM *tuple = erl_mk_tuple(t_array, 4);
        free(t_array);

        ETERM *prop_value = erl_mk_int_prop_value(data->id, tuple);
        l_array[nth_body++] = prop_value;
    }

    ETERM *a = erl_mk_atom("erlmunk_update");
    ETERM *l = erl_mk_list(l_array, count);
    free(l_array);
    ETERM **data_array = (ETERM **) malloc(sizeof(ETERM) * 2);
    data_array[0] = a;
    data_array[1] = l;
    ETERM *data = erl_mk_tuple(data_array, 2);
    free(data_array);
    ETERM *gen_cast = erl_mk_gen_cast(data);

    if (erl_send(subscriber->client->fd, subscriber->from, gen_cast) != 1) {
        DEBUGF(("failed to send data to subscriber"));
    }

    erl_free_compound(gen_cast);
}

void handle_space_subscribers(erlmunk_space *s) {

    for(erlmunk_subscriber *subscriber = s->subscribers;
        subscriber != NULL;
        subscriber = subscriber->hh.next) {

        element *bodies_in_bb = NULL, *el, *tmp;
        cpSpaceBBQuery(s->space, subscriber->bb,
                       CP_SHAPE_FILTER_ALL,
                       handle_shape, &bodies_in_bb);

        handle_subscriber(subscriber, bodies_in_bb);

        /* now delete each element, use the safe iterator */
        LL_FOREACH_SAFE(bodies_in_bb, el, tmp) {
            LL_DELETE(bodies_in_bb, el);
            free(el);
        }
    }
}

void spacesStep(float dt) {

    for(erlmunk_space *s = erlmunk_spaces; s != NULL; s = s->hh.next) {
        cpSpaceStep(s->space, (1.0 / FRAMES_PER_SECOND));

        handle_space_subscribers(s);
    }
}

void spacesRemoveSubscriber(erlmunk_client *client) {

    for(erlmunk_space *s = erlmunk_spaces; s != NULL; s = s->hh.next) {
        space_remove_subscriptions(s, client);
    }
}
