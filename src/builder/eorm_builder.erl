-module(eorm_builder).

-compile([export_all]).

build_sql(#{expr := #{sql := SQL} = Expr} = State) ->
    UpdSQL = eorm_utils:binary_join(SQL, <<" ">>),
    State#{expr => Expr#{sql => UpdSQL}}.
