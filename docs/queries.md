# Queries

## Select data

Select objects from db

```erlang
Result = eorm_db:select(user, #{
    where =>#{
        {priority, '>'} => 1
    },
    order_by => {id, asc}
    limit => 5,
    offset 10
}).
```

For selection will be used following sql:

```sql
select
    user.id, user.priority, user.name
from users
where
    user.priority > 1
order by id asc
limit 5
offset 10
```

## Select with related data

For example we have following definitions:
 
 * *user* that **has-many** *post*
 * *post* that **belongs-to** *user*

```erlang
eorm:def_entity(user, #{
    pk => id,
    table => users,
    'has-many' => [post]
}),

eorm:def_entity([post], #{
    pk => id,
    table => posts
    'belongs-to' => [user],
}).
```

For selecting *post* with related *user* can be used folloowing code:

```erlang
eorm_db:select(post, #{
    fields => [likes, data],
    with => [
        {user, #{
            fields => [name, priority]
            where => #{
                {priority, '>'} => 2
            }
        }
    ],
    where => #{
        {id, in} => [1,2,3]
    }
}).
```

This code will be translated in following query:

```sql
select 
    posts.id,
    posts.likes,
    posts.data,
    users.id,
    users.name,
    users.priority
from
    posts
left join posts on posts.user_id = users.id
where
    posts.id IN (1,2,3) and
    users.priority > 2
```

Or if we want to select *user* with related *post*:

```erlang
eorm_db:select(user, #{
    fields => [name],
    with => [
        {post, #{
            where => #{
                {likes, '>'} => 10
            }
        }
    ]
}).
```

In this case will be performed two queries:

```sql
select 
    users.id,
    users.name
from
    user
```


```sql
select 
    posts.id,
    posts.likes,
    posts.data,
from
    posts
where
    posts.likes > 10 and
    posts.user_id IN (...)
```