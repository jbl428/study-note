import { BaseEntity } from '../base-entity/base.entity';
import { Comment } from '../comment/comment.entity';
import { PostStatus } from './post-status';
import { PostStatusType } from '../type/post-status-type';
import { Column, Entity, OneToMany } from 'typeorm';

@Entity()
export class Post extends BaseEntity {
  @Column()
  name: string;

  @Column()
  content: string;

  @Column({ type: 'varchar', length: 10, transformer: new PostStatusType() })
  status: PostStatus;

  @Column({ nullable: true })
  memo?: string;

  @OneToMany(() => Comment, (comment) => comment.post)
  comments: Comment[];
}
