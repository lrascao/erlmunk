% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlmunk_body).

-export([activate/3,
         get_position/3,
         set_position/4,
         update_position/4,
         set_angle/4,
         set_angular_velocity/4,
         get_data/3,
         set_data/4,
         update_user_data/5,
         apply_impulse/4,
         copy/4,
         set_collision_circle/5]).

activate(Pid, SpaceRef, BodyId)
        when is_integer(BodyId) ->
    gen_server:cast(Pid, {body_activate, {SpaceRef, BodyId}}).

get_position(Pid, SpaceRef, BodyId)
        when is_integer(BodyId) ->
    {ok, {_x, _y} = Position, Angle} = gen_server:call(Pid, {body_get_position, {SpaceRef, BodyId}}),
    {ok, Position, Angle}.

set_position(Pid, SpaceRef, BodyId, {X, Y} = Vector)
        when is_integer(BodyId),
             is_float(X),
             is_float(Y) ->
    gen_server:cast(Pid, {body_set_position, {SpaceRef, BodyId, Vector}}).

update_position(Pid, SpaceRef, BodyId, Delta)
        when is_integer(BodyId),
             is_float(Delta) ->
    gen_server:cast(Pid, {body_update_position, {SpaceRef, BodyId, Delta}}).

set_angle(Pid, SpaceRef, BodyId, Angle)
        when is_integer(BodyId),
             is_float(Angle) ->
    gen_server:cast(Pid, {body_set_angle, {SpaceRef, BodyId, Angle}}).

set_angular_velocity(Pid, SpaceRef, BodyId, AngularVelocity)
        when is_integer(BodyId),
             is_float(AngularVelocity) ->
    gen_server:cast(Pid, {body_set_angular_velocity, {SpaceRef, BodyId, AngularVelocity}}).

get_data(Pid, SpaceRef, BodyId) ->
    gen_server:cast(Pid, {body_get_data, {SpaceRef, BodyId}}).

set_data(Pid, SpaceRef, BodyId, Data)
        when is_integer(BodyId) ->
    gen_server:cast(Pid, {body_set_data, {SpaceRef, BodyId, Data}}).

update_user_data(Pid, SpaceRef, BodyId, Key, Value)
        when is_integer(BodyId) ->
    gen_server:cast(Pid, {body_update_user_data, {SpaceRef, BodyId, Key, Value}}).

apply_impulse(Pid, SpaceRef, BodyId, Impulse)
        when is_integer(BodyId),
             is_float(Impulse) ->
    gen_server:cast(Pid, {body_apply_impulse, {SpaceRef, BodyId, Impulse}}).

copy(Pid, SpaceRef, BodyId, FromObjectId)
        when is_integer(BodyId),
             is_integer(FromObjectId) ->
    gen_server:cast(Pid, {body_copy, {SpaceRef, BodyId, FromObjectId}}).

set_collision_circle(Pid, SpaceRef, BodyId, Radius, CollisionType)
        when is_integer(BodyId),
             is_float(Radius),
             is_integer(CollisionType) ->
    gen_server:cast(Pid, {body_set_collision_circle,
                    {SpaceRef, BodyId, Radius, CollisionType}}).
