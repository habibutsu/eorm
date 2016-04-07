-module(eorm_builder_where).

-export([
    build/1
    ,build_sql/1
]).

build(#{'query' := #{where := Where}} = State) when map_size(Where) == 0 ->
    State;

build(#{'query' := #{where := Where, table := Table}, expr := Expr} = State) ->
    UpdExpr = maps:fold(
        erlz:partial(fun build_condition/4, [Table]),
        Expr,
        Where),

    State#{expr => UpdExpr};

build(State) ->
    State.

build_sql(#{expr := #{where := []}} = State) ->
    State;
build_sql(#{expr := #{where := Where, sql := SQL} = Expr} = State) ->
    BinWhere = eorm_utils:binary_join(Where, <<" AND ">>),
    State#{expr => Expr#{sql => SQL ++ [<<"where ", BinWhere/binary>>]}}.

build_condition(Table, {F, Op}, V, Expr) when not is_binary(F) ->
    build_condition(Table, {eorm_utils:to_binary(F), Op}, V, Expr);

build_condition(Table, FeildOp, V, Expr) ->
    #{
        bindings := Bindings,
        where := Where
    } = Expr,
    Num = length(Bindings),
    BinNum = erlang:integer_to_binary(Num+1),

    {F, Op} = case FeildOp of
        {InF, InOp} -> {eorm_utils:to_binary(InF), InOp};
        InF -> {eorm_utils:to_binary(InF), '='}
    end,

    {Binding, Cond} = case Op of
        in ->
            case V of
                [] ->
                    throw({invalid_condition, "bindings for 'IN' is empty"});
                _ ->
                    {V, <<F/binary, (build_op_in(Num+1, Num+length(V)))/binary>>}
            end;
        any ->
            {[V], <<F/binary, " = ANY($", BinNum/binary,")">>};
        'is_not' ->
            {[V], <<F/binary, " = IS NOT $", BinNum/binary>>};
        '=' ->
            {[V], <<F/binary, " = ", "$",BinNum/binary>>};
        Op ->
            {[V], <<F/binary, " ", (eorm_utils:to_binary(Op))/binary, " $",BinNum/binary>>}
    end,
    Expr#{
        bindings => Bindings ++ Binding,
        where => Where ++ [<<Table/binary, ".", Cond/binary>>]}.

build_op_in(From, To) ->
    <<" IN ($",
    (eorm_utils:binary_join(
        lists:map(
            fun integer_to_binary/1,
            lists:seq(From, To)),
        <<",$">>))/binary, ")">>.
