CREATE TABLE post
(
    id         bigserial primary key,
    title      varchar(255)             not null,
    content    text                     not null,
    type       varchar(50)              not null,
    created_at timestamp with time zone not null,
    updated_at timestamp with time zone not null,
    author_id  bigint                   not null
);
