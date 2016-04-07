
# Definition

Before start using of library is needed define entities. Entity - this is a
representation relation from db and can be defined in following simplest way:

```erlang

{ok, Conn} = epgsql:connect(
    "127.0.0.1", "dbuser", "dbpassword", [
        {database, "testdb"},
        {timeout, 4000}
]),

eorm:def_entity(user, #{
    db_connection => Conn,
    table => users
}).
```

Each definition of entity is stored in ETS table and can be got by this method:

```erlang
eorm:get_entity(user).
```

If in definition key **fields** is not specified will be will be performed query for
analyzing of table (only once).

```sql
select column_name from information_schema.columns
where table_schema=current_schema() and table_name = $1
```

## Available keys in definition

 * **db_adapter** - module that used for performing sql-queries (by default *adapter_epgsql*)
 * **db_connection** - connection to db that will be used for work with entity
 * **table** - table on which the entity should be mapped
 * **fields** - fields that should be selected from table
 * **has-one** - relation one to one
 * **has-many** - relation one to many
 * **belongs-to** - relation many to one
 * **transform-from** - transformation some format to entity
 * **transform-to** - transformation of entity to some format
 * **callbacks** - callbacks for extend behaviour

## Transformations

Is able to specify transformation of data after getting result from db or before
inserting (or updating) of it in db.

```erlang
#{
    'transform-from' => #{
        'db' => #{
            <<"json_field">> => fun jiffy:decode/1
        }
    },
    'transform-to' => #{
        'db' => #{
            <<"json_field">> => fun jiffy:encode/1
        }
    }
}
```

Also can be specified custom formats:

```erlang
#{
    'transform-from' => #{
        'json' => #{
            <<"json_field">> => fun iso8601:parse/1
        }
    },
    'transform-to' => #{
        'json' => #{
            <<"json_field">> => fun iso8601:format/1
        }
    }
}
```

For using custom format transformation should be called functions **transform_from** or
**transform_to**, for example:

```erlang
JsonObj = eorm_object:new(Type, Attrs),
NewObj = eorm:transform_from(json, JsonObj)
```

## Callbacks

Can be specified two callbacks:

 * prepare_obj_statement - for queries that build based on object
 * prepare_statement - for queries that build only based on parameters

```erlang
-spec prepare_obj_statement(Obj, {Kind, Query}) -> Query when
    Obj :: map(),
    Kind :: update | insert | delete,
    Query :: map()
```

```erlang
-spec prepare_statement({Kind, Query}) -> Query when
    Kind :: select,
    Query :: map()
```
