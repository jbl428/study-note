import { Migration } from '@mikro-orm/migrations';

export class Migration20220219121221 extends Migration {
  async up(): Promise<void> {
    this.addSql(
      'create table "post" ("id" bigserial primary key, "created_at" timestamptz(0) not null, "updated_at" timestamptz(0) not null, "name" varchar(255) not null, "content" varchar(255) not null, "memo" varchar(255) null);',
    );

    this.addSql(
      'create table "comment" ("id" bigserial primary key, "created_at" timestamptz(0) not null, "updated_at" timestamptz(0) not null, "content" varchar(255) not null, "like" int not null, "memo" varchar(255) null, "post_id" bigint not null);',
    );
  }
}
