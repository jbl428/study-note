create table notes
(
    id         int primary key,
    content    text,
    isVerified bool
);

insert into notes (id, content, isVerified) value (1, 'note 1', true);
insert into notes (id, content, isVerified) value (2, 'note 2', false);
insert into notes (id, content, isVerified) value (3, 'note 3', true);
