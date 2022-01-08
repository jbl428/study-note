import { EntityValidator } from './EntityValidator';
import { join } from 'path';

describe('EntityValidator', () => {
  it('빈 내용을 주면 에러가 발생하지 않는다', () => {
    // given
    const path = 'empty.ts';
    const content = ``;
    const checker = new EntityValidator(path, content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).not.toThrowError();
  });

  it('entity 데코레이터가 없으면 에러가 발생한다', () => {
    // given
    const path = 'post.ts';
    const content = `
export class Post {
  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}`;
    const checker = new EntityValidator(path, content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).toThrowError('Entity 데코레이터가 존재하지 않습니다');
  });

  it('JoinColumn 에 선언된 컬럼에 인덱스가 없으면 에러가 발생한다', () => {
    // given
    const path = 'post.ts';
    const content = `
@Entity()
export class Post {
  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}`;
    const checker = new EntityValidator(path, content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).toThrowError();
  });

  it('JoinColumn 에 선언된 컬럼에 인덱스 컬럼명을 잘못 입력하면 에러가 발생한다', () => {
    // given
    const content = `
@Entity()
@Index('idx_post_1', ['invalid'])
export class Post {
  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}`;
    const checker = new EntityValidator(content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).not.toThrowError();
  });

  it('JoinColumn 에 선언된 컬럼에 인덱스가 있으면 검증에 성공한다', () => {
    // given
    const content = `
@Entity()
@Index('idx_post_1', ['profile'])
export class Post {
  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}`;
    const checker = new EntityValidator(content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).not.toThrowError();
  });

  it('JoinColumn 이 아닌 컬럼에 대한 인덱스가 있어도 검증에 성공한다', () => {
    // given
    const content = `
@Entity()
@Index('idx_post_1', ['profile'])
@Index('idx_post_2', ['name'])
export class Post {
  @Column()
  name: string
  
  @ManyToOne(() => Profile, { createForeignKeyConstraints: false })
  @JoinColumn({ name: 'profile_id', referencedColumnName: 'id' })
  profile: Profile;
}`;
    const checker = new EntityValidator(content);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).not.toThrowError();
  });

  it('엔티티 파일들은 JoinColumn 에 대한 인덱스가 존재한다', () => {
    // given
    const path = join(__dirname, '../src/**/*.entity.ts');
    const checker = new EntityValidator(path);

    // when
    const validate = () => checker.validate();

    // then
    expect(validate).not.toThrowError();
  });
});
