export class PostStatus {
  static PUBLIC = new PostStatus('PUBLIC', '공개');
  static PRIVATE = new PostStatus('PRIVATE', '비공개');

  constructor(private code: string, private name: string) {}

  find(value: 'PUBLIC' | 'PRIVATE'): PostStatus {
    return value === 'PUBLIC' ? PostStatus.PUBLIC : PostStatus.PRIVATE;
  }
}
