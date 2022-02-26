import { Collection, Entity, OneToMany, Property } from '@mikro-orm/core';
import { BaseEntity } from '../base-entity/base.entity';
import { Comment } from '../comment/comment.entity';
import { PostStatus } from './post-status';
import { PostStatusType } from '../type/post-status-type';

@Entity()
export class Post extends BaseEntity {
  @Property()
  name: string;

  @Property()
  content: string;

  @Property({ type: PostStatusType })
  status: PostStatus;

  @Property()
  memo?: string;

  @Property({ type: 'json' })
  extra: Record<string, unknown>;

  @OneToMany(() => Comment, (comment) => comment.post)
  comments = new Collection<Comment>(this);
}
