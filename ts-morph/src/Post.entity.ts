import {
  Column,
  Entity,
  Index,
  JoinColumn,
  ManyToOne,
  PrimaryGeneratedColumn,
} from 'typeorm';
import { Profile } from './Profile.entity';

@Entity()
@Index('idx_post_1', ['profile'])
@Index('idx_post_2', ['status'])
export class Post {
  @PrimaryGeneratedColumn()
  id: number;

  @Column()
  content: string;

  @Column()
  status: string;

  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}
