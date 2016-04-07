
create table users (
    id serial,
    priority integer not null,
    name character varying(64) not null,
    primary key (id)
);
insert into users (priority, name) values(1, 'user1');
insert into users (priority, name) values(2, 'user2');
insert into users (priority, name) values(3, 'user3');
insert into users (priority, name) values(4, 'user4');
insert into users (priority, name) values(5, 'user5');

create table user_emails (
    id serial,
    user_id integer not null references users (id) on delete cascade,
    email character varying(64) not null,
    primary key (id)
);

insert into user_emails (user_id, email) values(1, 'user1@domain');

create table posts (
    id serial,
    user_id integer not null,
    created_at timestamp without time zone default now(),
    likes integer,
    data bytea,
    primary key (id)
);

create index on posts (user_id);

create table posts_01 (
    like posts including all
);
alter table posts_01 inherit posts;

create table posts_02 (
    like posts including all
);
alter table posts_02 inherit posts;


insert into posts_01 (user_id, likes) values(1, 0);
insert into posts_01 (user_id, likes) values(1, 0);
insert into posts_01 (user_id, likes) values(1, 1);
insert into posts_01 (user_id, likes) values(1, 1);
insert into posts_01 (user_id, likes) values(1, 2);

insert into posts_02 (user_id, likes) values(2, 0);
insert into posts_02 (user_id, likes) values(2, 0);
insert into posts_02 (user_id, likes) values(2, 1);
insert into posts_02 (user_id, likes) values(2, 1);
insert into posts_02 (user_id, likes) values(2, 2);

create table post_actions_log (
    id serial,
    action_post_id integer not null,
    created_at timestamp without time zone default now(),
    action character varying(64) not null,
    primary key (id)
);

insert into post_actions_log (action_post_id, action) values(1, 'created');
insert into post_actions_log (action_post_id, action) values(1, 'edited');
insert into post_actions_log (action_post_id, action) values(1, 'edited');

insert into post_actions_log (action_post_id, action) values(2, 'created');
insert into post_actions_log (action_post_id, action) values(2, 'edited');
insert into post_actions_log (action_post_id, action) values(2, 'edited');

insert into post_actions_log (action_post_id, action) values(3, 'created');
insert into post_actions_log (action_post_id, action) values(3, 'edited');
insert into post_actions_log (action_post_id, action) values(3, 'edited');

create table tags (
    id serial,
    value character varying(64) not null,
    primary key (id)
);

create table posts_tags (
    id serial,
    post_id integer not null references posts (id) on delete cascade,
    tag_id integer not null references tags (id) on delete cascade,
    primary key (id)
);

-- drop owned by dbuser cascade;
-- set enable_seqscan = off