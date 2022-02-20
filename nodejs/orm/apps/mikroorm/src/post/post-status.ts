export class PostStatus {
  static PUBLIC = new PostStatus('PUBLIC', '공개');
  static PRIVATE = new PostStatus('PRIVATE', '비공개');

  readonly #code: string;
  readonly #name: string;

  constructor(code: string, name: string) {
    this.#code = code;
    this.#name = name;
  }

  get code() {
    return this.#code;
  }

  get name() {
    return this.#name;
  }

  static find(value: string): PostStatus {
    return value === 'PUBLIC' ? PostStatus.PUBLIC : PostStatus.PRIVATE;
  }

  toJSON(): string {
    return this.#code;
  }
}
