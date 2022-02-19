import { Injectable } from '@nestjs/common';

@Injectable()
export class MikroormService {
  getHello(): string {
    return 'Hello World!';
  }
}
